# Forest / plot pipeline --------------------------------------------------

#' Run the forest RayExtract pipeline
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
#' @param args_terrain Additional raw command-line arguments passed to `rayextract terrain`.
#' @param args_trees Additional raw command-line arguments passed to `rayextract trees`.
#' @param args_split Additional raw command-line arguments passed to `raysplit`.
#' @param args_smooth Additional raw command-line arguments passed to `treesmooth`.
#' @param args_info Additional raw command-line arguments passed to `treeinfo`.
#' @param args_mesh Additional raw command-line arguments passed to `treemesh`.
#' @param dry_run Logical; construct commands without running Docker.
#' @param echo Logical; print Docker command output.
#'
#' @return A `ray_result` object summarizing the run.
#' @export
ray_pipeline_forest <- function(input_file,
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
                                args_import = "",
                                existing_raycloud = NULL,
                                reuse_existing_import = c("never", "fallback", "prefer"),
                                search_sibling_raycloud = TRUE,
                                args_colour = "",
                                args_terrain = "",
                                args_trees = "",
                                args_split = "",
                                args_smooth = "",
                                args_info = "",
                                args_mesh = "",
                                steps = NULL,
                                stop_after = NULL,
                                resume = FALSE,
                                overwrite = FALSE,
                                dry_run = FALSE,
                                echo = interactive()) {
  input_file <- ray_norm_path(input_file, must_work = !dry_run)
  reuse_existing_import <- match.arg(reuse_existing_import)
  input_mode <- ray_resolve_input_mode(input_mode, input_file, default = "point_cloud")
  imgs <- ray_get_images(raycloud_image = raycloud_image, treetools_image = treetools_image)

  paths <- ray_resolve_paths(input_file, project_root, out_root, run_name, mount_dir)
  log_file <- paths$log_file
  ray_write_log(log_file, "Starting forest pipeline for: ", paths$input_file, " [mode=", input_mode, "]")

  all_steps <- c("import", "colour", "terrain", "trees", "split", "smooth", "info", "mesh")
  default_steps <- ray_default_steps_forest(input_mode = input_mode, use_colour = use_colour)
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
  selected_steps <- ray_validate_selected_steps(selected_steps, all_steps, stop_after = stop_after)
  ray_assert_mode_compatible(input_mode, selected_steps, wrapper = "ray_pipeline_forest")

  skip_existing <- isTRUE(resume) && !isTRUE(overwrite)
  step_selected <- function(step) step %in% selected_steps
  active_index <- if (length(selected_steps)) max(match(selected_steps, all_steps)) else 0L
  step_needed <- function(step) match(step, all_steps) <= active_index

  make_existing <- function(step, input, outputs, hint) {
    needed <- unlist(outputs, use.names = FALSE)
    needed <- needed[nzchar(needed)]
    if (!dry_run && length(needed)) {
      absent <- needed[!file.exists(needed)]
      if (length(absent)) stop(paste0(step, " prerequisite not found:\n", paste(absent, collapse = "\n"), "\n", hint), call. = FALSE)
    }
    ray_existing_result(step = step, input = input, outputs = outputs, log_file = log_file)
  }

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
    s1 <- ray_external_input_result("ray_import", paths$input_file, list(raycloud = paths$input_file), log_file = log_file,
                                    details = list(input_mode = input_mode))
  } else {
    s1 <- ray_not_requested_result("ray_import", paths$input_file, list(raycloud = NA_character_), log_file = log_file)
  }

  s2 <- NULL
  raycloud_for_extract <- s1$outputs$raycloud
  if (input_mode %in% c("point_cloud", "raycloud")) {
    if (!step_needed("colour")) {
      s2 <- ray_not_requested_result("ray_colour", s1$outputs$raycloud, list(coloured_raycloud = NA_character_), log_file = log_file)
    } else if (step_selected("colour")) {
      s2 <- ray_colour(
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
      raycloud_for_extract <- s2$outputs$coloured_raycloud
    } else if (isTRUE(use_colour)) {
      colour_expected <- file.path(paths$dirs$colour, paste0(ray_stem_no_ext(s1$outputs$raycloud), "_coloured.ply"))
      s2 <- make_existing("ray_colour", s1$outputs$raycloud, list(coloured_raycloud = colour_expected),
                          "Run the colour step first, or include 'colour' in steps.")
      raycloud_for_extract <- s2$outputs$coloured_raycloud
    }
  }

  if (input_mode %in% c("treefile", "smoothed_treefile")) {
    s3 <- ray_not_requested_result("ray_terrain", NA_character_, list(terrain_mesh = NA_character_), log_file = log_file)
  } else if (!step_needed("terrain")) {
    s3 <- ray_not_requested_result("ray_terrain", raycloud_for_extract, list(terrain_mesh = NA_character_), log_file = log_file)
  } else if (step_selected("terrain")) {
    s3 <- ray_terrain(
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
    s3 <- make_existing("ray_terrain", raycloud_for_extract, list(terrain_mesh = terrain_expected),
                        "Run the terrain step first, or include 'terrain' in steps.")
  }

  if (identical(input_mode, "treefile")) {
    s4 <- ray_external_input_result("ray_trees", paths$input_file,
                                    list(segmented_ply = NA_character_, treefile = paths$input_file, combined_mesh = NA_character_),
                                    log_file = log_file, details = list(input_mode = input_mode))
  } else if (identical(input_mode, "smoothed_treefile")) {
    s4 <- ray_not_requested_result("ray_trees", NA_character_, list(segmented_ply = NA_character_, treefile = NA_character_, combined_mesh = NA_character_), log_file = log_file)
  } else if (!step_needed("trees")) {
    s4 <- ray_not_requested_result("ray_trees", c(raycloud_for_extract, s3$outputs$terrain_mesh), list(segmented_ply = NA_character_, treefile = NA_character_, combined_mesh = NA_character_), log_file = log_file)
  } else if (step_selected("trees")) {
    s4 <- ray_trees(
      raycloud_file = raycloud_for_extract,
      terrain_mesh = s3$outputs$terrain_mesh,
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
    s4 <- make_existing(
      "ray_trees",
      c(raycloud_for_extract, s3$outputs$terrain_mesh),
      list(
        segmented_ply = file.path(paths$dirs$trees, paste0(trees_stem, "_segmented.ply")),
        treefile = file.path(paths$dirs$trees, paste0(trees_stem, "_trees.txt")),
        combined_mesh = file.path(paths$dirs$trees, paste0(trees_stem, "_trees_mesh.ply"))
      ),
      "Run the trees step first, or include 'trees' in steps."
    )
  }

  if (input_mode %in% c("treefile", "smoothed_treefile")) {
    s5 <- ray_not_requested_result("ray_split_segmented", s4$outputs$segmented_ply, list(split_segmented = character()), log_file = log_file)
  } else if (!step_needed("split")) {
    s5 <- ray_not_requested_result("ray_split_segmented", s4$outputs$segmented_ply, list(split_segmented = character()), log_file = log_file)
  } else if (step_selected("split")) {
    s5 <- ray_split_segmented(
      segmented_ply = s4$outputs$segmented_ply,
      project_root = paths$project_root,
      out_dir = paths$dirs$split,
      split_mode = "seg_colour",
      args_split = args_split,
      docker_image = imgs$raycloud_image,
      mount_dir = mount_dir,
      dry_run = dry_run,
      echo = echo,
      log_file = log_file,
      skip_existing = skip_existing
    )
  } else {
    split_existing <- if (!dry_run) list.files(paths$dirs$split, pattern = "\\.ply$", full.names = TRUE) else character()
    if (!dry_run && !length(split_existing)) {
      stop("split prerequisite not found in: ", paths$dirs$split,
           "\nRun the split step first, or include 'split' in steps.", call. = FALSE)
    }
    s5 <- ray_existing_result("ray_split_segmented", s4$outputs$segmented_ply, list(split_segmented = split_existing), log_file = log_file)
  }

  if (identical(input_mode, "smoothed_treefile")) {
    s6 <- ray_external_input_result("ray_smooth", paths$input_file, list(smoothed_treefile = paths$input_file), log_file = log_file,
                                    details = list(input_mode = input_mode))
  } else if (!step_needed("smooth")) {
    s6 <- ray_not_requested_result("ray_smooth", s4$outputs$treefile, list(smoothed_treefile = NA_character_), log_file = log_file)
  } else if (step_selected("smooth")) {
    s6 <- ray_smooth(
      treefile = s4$outputs$treefile,
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
    s6 <- make_existing("ray_smooth", s4$outputs$treefile,
                        list(smoothed_treefile = file.path(paths$dirs$smooth, paste0(ray_stem_no_ext(s4$outputs$treefile), "_smoothed.txt"))),
                        "Run the smooth step first, or include 'smooth' in steps.")
  }

  if (!step_needed("info")) {
    s7 <- ray_not_requested_result("ray_info", s6$outputs$smoothed_treefile, list(info_file = NA_character_), log_file = log_file)
  } else if (step_selected("info")) {
    s7 <- ray_info(
      treefile = s6$outputs$smoothed_treefile,
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
    s7 <- make_existing("ray_info", s6$outputs$smoothed_treefile,
                        list(info_file = file.path(paths$dirs$info, paste0(ray_stem_no_ext(s6$outputs$smoothed_treefile), "_info.txt"))),
                        "Run the info step first, or include 'info' in steps.")
  }

  if (!step_needed("mesh")) {
    s8 <- ray_not_requested_result("ray_mesh", s6$outputs$smoothed_treefile, list(mesh_file = NA_character_), log_file = log_file)
  } else if (step_selected("mesh")) {
    s8 <- ray_mesh(
      treefile = s6$outputs$smoothed_treefile,
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
    s8 <- make_existing("ray_mesh", s6$outputs$smoothed_treefile,
                        list(mesh_file = file.path(paths$dirs$mesh, paste0(ray_stem_no_ext(s6$outputs$smoothed_treefile), "_mesh.ply"))),
                        "Run the mesh step first, or include 'mesh' in steps.")
  }

  new_ray_result(
    step = "ray_pipeline_forest",
    input = paths$input_file,
    outputs = list(
      raycloud_file = s1$outputs$raycloud,
      coloured_raycloud = if (!is.null(s2)) s2$outputs$coloured_raycloud else NA_character_,
      terrain_mesh = s3$outputs$terrain_mesh,
      segmented_ply = s4$outputs$segmented_ply,
      treefile = s4$outputs$treefile,
      split_segmented = s5$outputs$split_segmented,
      split_segmented_dir = paths$dirs$split,
      smoothed_treefile = s6$outputs$smoothed_treefile,
      info_file = s7$outputs$info_file,
      mesh_file = s8$outputs$mesh_file
    ),
    command = NULL,
    status = if (dry_run) "dry_run" else "success",
    log_file = log_file,
    details = list(
      steps = list(import = s1, colour = s2, terrain = s3, trees = s4, split = s5, smooth = s6, info = s7, mesh = s8),
      paths = paths,
      input_mode = input_mode,
      selected_steps = selected_steps,
      stop_after = stop_after,
      resume = resume,
      overwrite = overwrite,
      use_colour = use_colour
    )
  )
}
