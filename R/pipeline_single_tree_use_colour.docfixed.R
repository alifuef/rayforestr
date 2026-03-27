# Single-tree pipeline ----------------------------------------------------

#' Run the single-tree RayExtract pipeline
#'
#' Orchestrates the step functions in `rayforestr` with support for selective
#' steps, resume logic, and multiple entry modes.
#'
#' Supported `input_mode` values are:
#' - `"point_cloud"`: start from a raw `.ply` and optionally run `rayimport`
#' - `"raycloud"`: start from an existing `*_raycloud.ply`
#' - `"treefile"`: start from an existing `*_trees.txt`
#' - `"smoothed_treefile"`: start from an existing `*_smoothed.txt`
#'
#' For `input_mode = "point_cloud"`, import behavior is controlled by
#' `reuse_existing_import`:
#' - `"never"`: always try a fresh import
#' - `"prefer"`: use an existing raycloud immediately
#' - `"fallback"`: try fresh import first, then reuse an existing raycloud
#'
#' @param input_file Path to the starting input file for the chosen `input_mode`.
#' @param input_mode One of `"point_cloud"`, `"raycloud"`, `"treefile"`,
#'   or `"smoothed_treefile"`.
#' @param project_root Project root mounted into Docker.
#' @param out_root Optional run folder path.
#' @param run_name Optional run name used under `<project_root>/runs/`.
#' @param steps Optional character vector of step names to run.
#' @param stop_after Optional step name after which to stop.
#' @param resume Logical; reuse existing prerequisites when available.
#' @param overwrite Logical; rerun requested steps even if outputs exist.
#' @param reuse_existing_import Import reuse policy forwarded to `ray_import()`.
#' @param existing_raycloud Optional explicit existing raycloud path.
#' @param search_sibling_raycloud Logical; search for `<stem>_raycloud.ply`
#'   beside a raw input point cloud.
#' @param use_colour Logical; whether to run the colour step when relevant.
#' @param mount_dir Docker mount path inside the container.
#' @param raycloud_image Docker image used for RayCloudTools commands.
#'   Defaults to package options.
#' @param treetools_image Docker image used for TreeTools commands.
#'   Defaults to the RayCloudTools image when unset.
#' @param ray_direction Ray direction string passed to `rayimport`,
#'   for example `"0,0,-1"`.
#' @param max_intensity Maximum intensity passed to `rayimport`.
#' @param remove_start_pos Logical; passed to `rayimport` when supported.
#' @param colour_mode Mode string passed to `raycolour`.
#' @param args_import Additional raw command-line arguments passed to `rayimport`.
#' @param args_colour Additional raw command-line arguments passed to `raycolour`.
#' @param skip_terrain Logical; skip terrain extraction when an existing
#'   terrain product is already available.
#' @param mesh_file Optional existing mesh file path.
#' @param args_terrain Additional raw command-line arguments passed to `rayextract terrain`.
#' @param args_trees Additional raw command-line arguments passed to `rayextract trees`.
#' @param args_smooth Additional raw command-line arguments passed to `treesmooth`.
#' @param args_info Additional raw command-line arguments passed to `treeinfo`.
#' @param args_mesh Additional raw command-line arguments passed to `treemesh`.
#' @param validate_single_tree Logical; validate the resulting single-tree
#'   treefile before returning.
#' @param dry_run Logical; construct commands without running Docker.
#' @param echo Logical; print Docker command output.
#'
#' @return A `ray_result` object summarizing the run.
#' @export
ray_pipeline_single_tree <- function(input_file,
                                     project_root = NULL,
                                     out_root = NULL,
                                     run_name = NULL,
                                     mount_dir = "/workspace/project",
                                     input_mode = NULL,
                                     raycloud_image = NULL,
                                     treetools_image = NULL,
                                     ray_direction = "0,0,-1",
                                     max_intensity = 0,
                                     remove_start_pos = FALSE,
                                     use_colour = FALSE,
                                     colour_mode = "alpha 1",
                                     args_colour = "",
                                     skip_terrain = FALSE,
                                     mesh_file = NULL,
                                     args_import = "",
                                     existing_raycloud = NULL,
                                     reuse_existing_import = c("never", "fallback", "prefer"),
                                     search_sibling_raycloud = TRUE,
                                     args_terrain = "",
                                     args_trees = "",
                                     args_smooth = "",
                                     args_info = "",
                                     args_mesh = "",
                                     steps = NULL,
                                     stop_after = NULL,
                                     resume = FALSE,
                                     overwrite = FALSE,
                                     validate_single_tree = TRUE,
                                     dry_run = FALSE,
                                     echo = interactive()) {
  input_file <- ray_norm_path(input_file, must_work = !dry_run)
  reuse_existing_import <- match.arg(reuse_existing_import)
  input_mode <- ray_resolve_input_mode(input_mode, input_file, default = "point_cloud")
  imgs <- ray_get_images(raycloud_image = raycloud_image, treetools_image = treetools_image)

  paths <- ray_resolve_paths(input_file, project_root, out_root, run_name, mount_dir)
  log_file <- paths$log_file
  ray_write_log(log_file, "Starting single-tree pipeline for: ", paths$input_file, " [mode=", input_mode, "]")

  all_steps <- c("import", "colour", "terrain", "trees", "smooth", "info", "mesh")
  default_steps <- ray_default_steps_single_tree(input_mode = input_mode, use_colour = use_colour, skip_terrain = skip_terrain)
  if (is.null(steps)) {
    selected_steps <- default_steps
  } else {
    bad <- setdiff(steps, all_steps)
    if (length(bad)) stop("Unknown step(s): ", paste(bad, collapse = ", "), call. = FALSE)
    selected_steps <- unique(steps)
  }
  if (isTRUE(use_colour) && input_mode %in% c("point_cloud", "raycloud") && !"colour" %in% selected_steps && is.null(steps)) {
    selected_steps <- unique(c(selected_steps, "colour"))
  }
  if (isTRUE(skip_terrain)) selected_steps <- setdiff(selected_steps, "terrain")
  selected_steps <- ray_validate_selected_steps(selected_steps, all_steps, stop_after = stop_after)
  ray_assert_mode_compatible(input_mode, selected_steps, wrapper = "ray_pipeline_single_tree")

  skip_existing <- isTRUE(resume) && !isTRUE(overwrite)
  step_selected <- function(step) step %in% selected_steps
  active_index <- if (length(selected_steps)) max(match(selected_steps, all_steps)) else 0L
  step_needed <- function(step) match(step, all_steps) <= active_index

  make_existing <- function(step, input, outputs, hint) {
    needed <- unlist(outputs, use.names = FALSE)
    needed <- needed[nzchar(needed)]
    if (!dry_run && length(needed)) {
      absent <- needed[!file.exists(needed)]
      if (length(absent)) {
        stop(paste0(step, " prerequisite not found:\n", paste(absent, collapse = "\n"), "\n", hint), call. = FALSE)
      }
    }
    ray_existing_result(step = step, input = input, outputs = outputs, log_file = log_file)
  }

  # Stage 1: import / entry raycloud ------------------------------------
  import_expected <- file.path(paths$dirs$import, paste0(ray_stem_no_ext(paths$input_file), "_raycloud.ply"))
  if (identical(input_mode, "point_cloud")) {
    if (!step_needed("import")) {
      s1 <- ray_not_requested_result("ray_import", paths$input_file, list(raycloud = NA_character_), log_file = log_file)
    } else if (step_selected("import")) {
      s1 <- ray_import(
        input_file = paths$input_file,
        project_root = paths$project_root,
        out_dir = paths$dirs$import,
        ray_direction = ray_direction,
        max_intensity = max_intensity,
        remove_start_pos = remove_start_pos,
        args_import = args_import,
        existing_raycloud = existing_raycloud,
        reuse_existing = reuse_existing_import,
        search_sibling_raycloud = search_sibling_raycloud,
        docker_image = imgs$raycloud_image,
        mount_dir = mount_dir,
        dry_run = dry_run,
        echo = echo,
        log_file = log_file,
        skip_existing = skip_existing
      )
    } else {
      s1 <- make_existing("ray_import", paths$input_file, list(raycloud = import_expected),
                          "Run the import step first, or include 'import' in steps.")
    }
  } else if (identical(input_mode, "raycloud")) {
    s1 <- ray_external_input_result(
      step = "ray_import",
      input = paths$input_file,
      outputs = list(raycloud = paths$input_file),
      log_file = log_file,
      details = list(input_mode = input_mode)
    )
  } else {
    s1 <- ray_not_requested_result("ray_import", paths$input_file, list(raycloud = NA_character_), log_file = log_file)
  }

  # Stage 2: colour ------------------------------------------------------
  s1c <- NULL
  raycloud_for_extract <- s1$outputs$raycloud
  if (input_mode %in% c("point_cloud", "raycloud")) {
    if (!step_needed("colour")) {
      s1c <- ray_not_requested_result("ray_colour", s1$outputs$raycloud, list(coloured_raycloud = NA_character_), log_file = log_file)
    } else if (step_selected("colour")) {
      s1c <- ray_colour(
        raycloud_file = s1$outputs$raycloud,
        project_root = paths$project_root,
        out_dir = paths$dirs$colour,
        colour_mode = colour_mode,
        args_colour = args_colour,
        docker_image = imgs$raycloud_image,
        mount_dir = mount_dir,
        dry_run = dry_run,
        echo = echo,
        log_file = log_file,
        skip_existing = skip_existing
      )
      raycloud_for_extract <- s1c$outputs$coloured_raycloud
    } else if (isTRUE(use_colour)) {
      colour_expected <- file.path(paths$dirs$colour, paste0(ray_stem_no_ext(s1$outputs$raycloud), "_coloured.ply"))
      s1c <- make_existing("ray_colour", s1$outputs$raycloud, list(coloured_raycloud = colour_expected),
                           "Run the colour step first, or include 'colour' in steps.")
      raycloud_for_extract <- s1c$outputs$coloured_raycloud
    }
  }

  # Stage 3: terrain / external mesh ------------------------------------
  if (input_mode %in% c("treefile", "smoothed_treefile")) {
    s2 <- ray_not_requested_result("ray_terrain", NA_character_, list(terrain_mesh = NA_character_), log_file = log_file)
  } else if (!step_needed("terrain") && !isTRUE(skip_terrain)) {
    s2 <- ray_not_requested_result("ray_terrain", raycloud_for_extract, list(terrain_mesh = NA_character_), log_file = log_file)
  } else if (isTRUE(skip_terrain)) {
    if (is.null(mesh_file)) stop("mesh_file must be supplied when skip_terrain = TRUE", call. = FALSE)
    s2 <- ray_external_input_result(
      step = "ray_terrain",
      input = raycloud_for_extract,
      outputs = list(terrain_mesh = ray_norm_path(mesh_file, must_work = !dry_run)),
      log_file = log_file,
      details = list(skip_terrain = TRUE)
    )
  } else if (step_selected("terrain")) {
    s2 <- ray_terrain(
      raycloud_file = raycloud_for_extract,
      project_root = paths$project_root,
      out_dir = paths$dirs$terrain,
      args_terrain = args_terrain,
      docker_image = imgs$raycloud_image,
      mount_dir = mount_dir,
      dry_run = dry_run,
      echo = echo,
      log_file = log_file,
      skip_existing = skip_existing
    )
  } else {
    terrain_expected <- file.path(paths$dirs$terrain, paste0(ray_stem_no_ext(raycloud_for_extract), "_mesh.ply"))
    s2 <- make_existing("ray_terrain", raycloud_for_extract, list(terrain_mesh = terrain_expected),
                        "Run the terrain step first, or include 'terrain' in steps.")
  }

  # Stage 4: trees / external treefile ----------------------------------
  if (identical(input_mode, "treefile")) {
    s3 <- ray_external_input_result(
      step = "ray_trees",
      input = paths$input_file,
      outputs = list(segmented_ply = NA_character_, treefile = paths$input_file, combined_mesh = NA_character_),
      log_file = log_file,
      details = list(input_mode = input_mode)
    )
  } else if (identical(input_mode, "smoothed_treefile")) {
    s3 <- ray_not_requested_result("ray_trees", NA_character_, list(segmented_ply = NA_character_, treefile = NA_character_, combined_mesh = NA_character_), log_file = log_file)
  } else if (!step_needed("trees")) {
    s3 <- ray_not_requested_result("ray_trees", c(raycloud_for_extract, s2$outputs$terrain_mesh), list(segmented_ply = NA_character_, treefile = NA_character_, combined_mesh = NA_character_), log_file = log_file)
  } else if (step_selected("trees")) {
    s3 <- ray_trees(
      raycloud_file = raycloud_for_extract,
      terrain_mesh = s2$outputs$terrain_mesh,
      project_root = paths$project_root,
      out_dir = paths$dirs$trees,
      args_trees = args_trees,
      docker_image = imgs$raycloud_image,
      mount_dir = mount_dir,
      dry_run = dry_run,
      echo = echo,
      log_file = log_file,
      skip_existing = skip_existing
    )
  } else {
    trees_stem <- ray_stem_no_ext(raycloud_for_extract)
    s3 <- make_existing(
      step = "ray_trees",
      input = c(raycloud_for_extract, s2$outputs$terrain_mesh),
      outputs = list(
        segmented_ply = file.path(paths$dirs$trees, paste0(trees_stem, "_segmented.ply")),
        treefile = file.path(paths$dirs$trees, paste0(trees_stem, "_trees.txt")),
        combined_mesh = file.path(paths$dirs$trees, paste0(trees_stem, "_trees_mesh.ply"))
      ),
      hint = "Run the trees step first, or include 'trees' in steps."
    )
  }

  validation <- if (identical(input_mode, "point_cloud") || identical(input_mode, "raycloud")) {
    if (!step_needed("trees") || dry_run || !isTRUE(validate_single_tree) || !step_selected("trees")) {
      list(ok = TRUE, n_trees = NA_integer_, treefile = s3$outputs$treefile)
    } else {
      ray_validate_single_tree(s3$outputs$treefile)
    }
  } else {
    list(ok = TRUE, n_trees = NA_integer_, treefile = s3$outputs$treefile)
  }

  if (!dry_run && isTRUE(validate_single_tree) && !isTRUE(validation$ok)) {
    stop("Single-tree validation failed: rayextract trees appears to contain ",
         validation$n_trees, " trees. Retune tree extraction parameters.", call. = FALSE)
  }

  # Stage 5: smooth / external smoothed treefile -------------------------
  if (identical(input_mode, "smoothed_treefile")) {
    s4 <- ray_external_input_result(
      step = "ray_smooth",
      input = paths$input_file,
      outputs = list(smoothed_treefile = paths$input_file),
      log_file = log_file,
      details = list(input_mode = input_mode)
    )
  } else if (!step_needed("smooth")) {
    s4 <- ray_not_requested_result("ray_smooth", s3$outputs$treefile, list(smoothed_treefile = NA_character_), log_file = log_file)
  } else if (step_selected("smooth")) {
    s4 <- ray_smooth(
      treefile = s3$outputs$treefile,
      project_root = paths$project_root,
      out_dir = paths$dirs$smooth,
      args_smooth = args_smooth,
      docker_image = imgs$treetools_image,
      mount_dir = mount_dir,
      dry_run = dry_run,
      echo = echo,
      log_file = log_file,
      skip_existing = skip_existing
    )
  } else {
    s4 <- make_existing(
      step = "ray_smooth",
      input = s3$outputs$treefile,
      outputs = list(smoothed_treefile = file.path(paths$dirs$smooth, paste0(ray_stem_no_ext(s3$outputs$treefile), "_smoothed.txt"))),
      hint = "Run the smooth step first, or include 'smooth' in steps."
    )
  }

  # Stage 6: info --------------------------------------------------------
  if (!step_needed("info")) {
    s5 <- ray_not_requested_result("ray_info", s4$outputs$smoothed_treefile, list(info_file = NA_character_), log_file = log_file)
  } else if (step_selected("info")) {
    s5 <- ray_info(
      treefile = s4$outputs$smoothed_treefile,
      project_root = paths$project_root,
      out_dir = paths$dirs$info,
      args_info = args_info,
      docker_image = imgs$treetools_image,
      mount_dir = mount_dir,
      dry_run = dry_run,
      echo = echo,
      log_file = log_file,
      skip_existing = skip_existing
    )
  } else {
    s5 <- make_existing(
      step = "ray_info",
      input = s4$outputs$smoothed_treefile,
      outputs = list(info_file = file.path(paths$dirs$info, paste0(ray_stem_no_ext(s4$outputs$smoothed_treefile), "_info.txt"))),
      hint = "Run the info step first, or include 'info' in steps."
    )
  }

  # Stage 7: mesh --------------------------------------------------------
  if (!step_needed("mesh")) {
    s6 <- ray_not_requested_result("ray_mesh", s4$outputs$smoothed_treefile, list(mesh_file = NA_character_), log_file = log_file)
  } else if (step_selected("mesh")) {
    s6 <- ray_mesh(
      treefile = s4$outputs$smoothed_treefile,
      project_root = paths$project_root,
      out_dir = paths$dirs$mesh,
      args_mesh = args_mesh,
      docker_image = imgs$treetools_image,
      mount_dir = mount_dir,
      dry_run = dry_run,
      echo = echo,
      log_file = log_file,
      skip_existing = skip_existing
    )
  } else {
    s6 <- make_existing(
      step = "ray_mesh",
      input = s4$outputs$smoothed_treefile,
      outputs = list(mesh_file = file.path(paths$dirs$mesh, paste0(ray_stem_no_ext(s4$outputs$smoothed_treefile), "_mesh.ply"))),
      hint = "Run the mesh step first, or include 'mesh' in steps."
    )
  }

  new_ray_result(
    step = "ray_pipeline_single_tree",
    input = paths$input_file,
    outputs = list(
      raycloud_file = s1$outputs$raycloud,
      coloured_raycloud = if (!is.null(s1c)) s1c$outputs$coloured_raycloud else NA_character_,
      terrain_mesh = s2$outputs$terrain_mesh,
      segmented_ply = s3$outputs$segmented_ply,
      treefile = s3$outputs$treefile,
      smoothed_treefile = s4$outputs$smoothed_treefile,
      info_file = s5$outputs$info_file,
      mesh_file = s6$outputs$mesh_file
    ),
    command = NULL,
    status = if (dry_run) "dry_run" else "success",
    log_file = log_file,
    details = list(
      steps = list(import = s1, colour = s1c, terrain = s2, trees = s3, smooth = s4, info = s5, mesh = s6),
      validation = validation,
      paths = paths,
      input_mode = input_mode,
      selected_steps = selected_steps,
      stop_after = stop_after,
      resume = resume,
      overwrite = overwrite,
      use_colour = use_colour,
      skip_terrain = skip_terrain
    )
  )
}
