# Docker and path helpers -------------------------------------------------

ray_norm_path <- function(path, must_work = FALSE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

ray_to_posix <- function(path) {
  gsub("\\\\", "/", path)
}

ray_bash_quote <- function(x) {
  paste0("'", gsub("'", "'\"'\"'", x, fixed = TRUE), "'")
}

ray_stem_no_ext <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

ray_write_log <- function(log_file, ...) {
  if (is.null(log_file)) return(invisible(NULL))
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  cat(
    sprintf("[%s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    ...,
    "\n",
    sep = "",
    file = log_file,
    append = TRUE
  )
  invisible(log_file)
}

ray_rel_to_root <- function(path, root) {
  path <- ray_norm_path(path, must_work = FALSE)
  root <- ray_norm_path(root, must_work = TRUE)

  if (identical(path, root)) return(".")

  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop(
      "Path is not inside project_root:\n",
      "  path = ", path, "\n",
      "  root = ", root,
      call. = FALSE
    )
  }

  substring(path, nchar(prefix) + 1L)
}

ray_docker_path <- function(path, project_root, mount_dir = "/workspace/project") {
  rel <- ray_rel_to_root(path, project_root)
  file.path(mount_dir, rel) |> ray_to_posix()
}

ray_resolve_paths <- function(input_file,
                              project_root = NULL,
                              out_root = NULL,
                              run_name = NULL,
                              mount_dir = "/workspace/project") {
  input_file <- ray_norm_path(input_file, must_work = TRUE)
  if (is.null(project_root)) project_root <- dirname(input_file)
  project_root <- ray_norm_path(project_root, must_work = TRUE)

  if (is.null(run_name)) run_name <- paste0(ray_stem_no_ext(input_file), "_run")
  if (is.null(out_root)) out_root <- file.path(project_root, "runs", run_name)
  out_root <- ray_norm_path(out_root, must_work = FALSE)

  dirs <- list(
    run = out_root,
    input = file.path(out_root, "step1_input"),
    import = file.path(out_root, "step2_rayimport"),
    colour = file.path(out_root, "step3_raycolour"),
    terrain = file.path(out_root, "step4_terrain"),
    trees = file.path(out_root, "step5_trees"),
    split = file.path(out_root, "step6_split"),
    smooth = file.path(out_root, "step7_smooth"),
    info = file.path(out_root, "step8_info"),
    mesh = file.path(out_root, "step9_mesh"),
    analysis = file.path(out_root, "analysis"),
    logs = file.path(out_root, "logs")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  list(
    input_file = input_file,
    project_root = project_root,
    out_root = out_root,
    run_name = run_name,
    mount_dir = mount_dir,
    dirs = dirs,
    log_file = file.path(dirs$logs, "pipeline.log")
  )
}

ray_compose_extra_args <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  paste(parts, collapse = " ")
}

ray_stage_snapshot <- function(out_dir, dry_run = FALSE) {
  if (isTRUE(dry_run)) return(character())
  list.files(out_dir, full.names = TRUE, recursive = FALSE)
}

ray_pick_new_files <- function(before, after, pattern = NULL) {
  out <- setdiff(after, before)
  if (!is.null(pattern)) out <- out[grepl(pattern, basename(out), perl = TRUE)]
  out
}

ray_stage_new_files <- function(before,
                                out_dir,
                                pattern = NULL,
                                exclude = character()) {
  after <- list.files(out_dir, full.names = TRUE, recursive = FALSE)
  out <- ray_pick_new_files(before, after, pattern = pattern)
  if (length(exclude)) out <- setdiff(out, exclude)
  out
}

ray_find_newest <- function(paths) {
  if (!length(paths)) return(character())
  info <- file.info(paths)
  paths[order(info$mtime, decreasing = TRUE)][1]
}

ray_check_output <- function(paths, label = "Expected output") {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(label, " missing:\n", paste(missing, collapse = "\n"), call. = FALSE)
  }
  invisible(paths)
}

ray_check_docker <- function() {
  out <- tryCatch(system2("docker", "--version", stdout = TRUE, stderr = TRUE), error = function(e) NULL)
  !is.null(out)
}

ray_skip_result <- function(step, input, outputs, command = NULL, log_file = NULL, details = list()) {
  new_ray_result(step = step, input = input, outputs = outputs, command = command,
                 status = "skipped_existing", log_file = log_file, details = details)
}

ray_existing_result <- function(step, input, outputs, log_file = NULL, details = list()) {
  new_ray_result(step = step, input = input, outputs = outputs, command = NULL,
                 status = "existing_input", log_file = log_file, details = details)
}

ray_external_input_result <- function(step, input, outputs, log_file = NULL, details = list()) {
  new_ray_result(step = step, input = input, outputs = outputs, command = NULL,
                 status = "external_input", log_file = log_file, details = details)
}

ray_not_requested_result <- function(step, input, outputs = list(), log_file = NULL, details = list()) {
  new_ray_result(step = step, input = input, outputs = outputs, command = NULL,
                 status = "not_requested", log_file = log_file, details = details)
}

ray_require_file <- function(path, label, dry_run = FALSE, hint = NULL) {
  path <- ray_norm_path(path, must_work = FALSE)
  if (isTRUE(dry_run) || file.exists(path)) return(path)
  msg <- paste0(label, " not found: ", path)
  if (!is.null(hint) && nzchar(hint)) msg <- paste0(msg, "\n", hint)
  stop(msg, call. = FALSE)
}

ray_localize_input <- function(input_file,
                               out_dir,
                               dry_run = FALSE,
                               prefer_link = TRUE) {
  input_file <- ray_norm_path(input_file, must_work = !dry_run)
  out_dir <- ray_norm_path(out_dir, must_work = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  stage_path <- file.path(out_dir, basename(input_file))

  if (isTRUE(dry_run)) {
    return(list(
      source = input_file,
      stage_path = stage_path,
      basename = basename(stage_path),
      local_name = basename(stage_path),
      method = if (isTRUE(prefer_link)) "planned_hardlink" else "planned_copy",
      cleanup = TRUE
    ))
  }

  same_path <- file.exists(stage_path) && identical(
    ray_norm_path(stage_path, must_work = TRUE),
    ray_norm_path(input_file, must_work = TRUE)
  )

  if (!same_path && file.exists(stage_path)) unlink(stage_path, force = TRUE)

  method <- "existing"
  cleanup <- FALSE
  if (!same_path) {
    cleanup <- TRUE
    if (isTRUE(prefer_link) && isTRUE(suppressWarnings(file.link(input_file, stage_path)))) {
      method <- "hardlink"
    } else {
      copied <- file.copy(input_file, stage_path, overwrite = TRUE)
      if (!isTRUE(copied)) stop("Could not localize input into stage folder: ", stage_path, call. = FALSE)
      method <- "copy"
    }
  }

  list(
    source = input_file,
    stage_path = stage_path,
    basename = basename(stage_path),
    local_name = basename(stage_path),
    method = method,
    cleanup = cleanup
  )
}

ray_cleanup_localized_inputs <- function(localized,
                                         dry_run = FALSE,
                                         log_file = NULL) {
  if (isTRUE(dry_run) || is.null(localized) || !length(localized)) return(invisible(NULL))

  is_single_localized <- function(x) {
    is.list(x) && !is.null(x$stage_path) && !is.null(x$cleanup)
  }

  locs <- if (is_single_localized(localized)) list(localized) else localized
  for (loc in locs) {
    if (!is_single_localized(loc) || !isTRUE(loc$cleanup)) next
    if (file.exists(loc$stage_path)) {
      unlink(loc$stage_path, force = TRUE)
      ray_write_log(log_file, "Cleaned localized input: ", loc$stage_path)
    }
  }
  invisible(NULL)
}

# Compatibility aliases kept intentionally while the package API stabilizes.
ray_stage_localize_input <- function(input_file,
                                     out_dir = NULL,
                                     stage_dir = NULL,
                                     dry_run = FALSE,
                                     prefer_link = TRUE,
                                     ...) {
  target_dir <- if (!is.null(out_dir)) out_dir else stage_dir
  if (is.null(target_dir)) stop("Either out_dir or stage_dir must be supplied.", call. = FALSE)
  ray_localize_input(input_file = input_file, out_dir = target_dir, dry_run = dry_run, prefer_link = prefer_link)
}

ray_cleanup_localized_input <- function(localized,
                                        dry_run = FALSE,
                                        log_file = NULL,
                                        ...) {
  ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file)
}

ray_step_inputs <- function() c("point_cloud", "raycloud", "treefile", "smoothed_treefile")

ray_step_input_labels <- function() {
  c(
    point_cloud = "raw point cloud",
    raycloud = "existing raycloud",
    treefile = "existing treefile",
    smoothed_treefile = "existing smoothed treefile"
  )
}

ray_resolve_input_mode <- function(input_mode = NULL,
                                   input_file,
                                   default = "point_cloud") {
  if (is.null(input_mode) || !nzchar(input_mode)) {
    ext <- tolower(tools::file_ext(input_file))
    stem <- tolower(basename(input_file))
    guessed <- if (grepl("_raycloud\\.ply$", stem)) {
      "raycloud"
    } else if (grepl("_smoothed\\.txt$", stem)) {
      "smoothed_treefile"
    } else if (grepl("_trees\\.txt$", stem)) {
      "treefile"
    } else if (ext %in% c("ply", "laz", "las", "pcd", "xyz", "txt")) {
      default
    } else {
      default
    }
    return(guessed)
  }
  match.arg(input_mode, ray_step_inputs())
}

ray_default_steps_single_tree <- function(input_mode = "point_cloud",
                                          use_colour = FALSE,
                                          skip_terrain = FALSE) {
  input_mode <- match.arg(input_mode, ray_step_inputs())
  steps <- switch(
    input_mode,
    point_cloud = c("import", if (isTRUE(use_colour)) "colour", if (!isTRUE(skip_terrain)) "terrain", "trees", "smooth", "info", "mesh"),
    raycloud = c(if (isTRUE(use_colour)) "colour", if (!isTRUE(skip_terrain)) "terrain", "trees", "smooth", "info", "mesh"),
    treefile = c("smooth", "info", "mesh"),
    smoothed_treefile = c("info", "mesh")
  )
  steps[nzchar(steps)]
}

ray_default_steps_forest <- function(input_mode = "point_cloud",
                                     use_colour = FALSE) {
  input_mode <- match.arg(input_mode, ray_step_inputs())
  steps <- switch(
    input_mode,
    point_cloud = c("import", if (isTRUE(use_colour)) "colour", "terrain", "trees", "split", "smooth", "info", "mesh"),
    raycloud = c(if (isTRUE(use_colour)) "colour", "terrain", "trees", "split", "smooth", "info", "mesh"),
    treefile = c("smooth", "info", "mesh"),
    smoothed_treefile = c("info", "mesh")
  )
  steps[nzchar(steps)]
}

ray_validate_selected_steps <- function(selected_steps,
                                        all_steps,
                                        stop_after = NULL) {
  selected_steps <- intersect(all_steps, unique(selected_steps))
  if (!is.null(stop_after)) {
    if (!stop_after %in% all_steps) stop("Unknown stop_after step: ", stop_after, call. = FALSE)
    selected_steps <- selected_steps[match(selected_steps, all_steps) <= match(stop_after, all_steps)]
  }
  selected_steps
}

ray_assert_mode_compatible <- function(input_mode,
                                       steps,
                                       wrapper = "pipeline") {
  incompatible <- switch(
    input_mode,
    point_cloud = character(),
    raycloud = setdiff(steps, c("colour", "terrain", "trees", "split", "smooth", "info", "mesh")),
    treefile = intersect(steps, c("import", "colour", "terrain", "trees", "split")),
    smoothed_treefile = intersect(steps, c("import", "colour", "terrain", "trees", "split", "smooth"))
  )
  if (length(incompatible)) {
    stop(
      wrapper, ": steps not compatible with input_mode = '", input_mode, "': ",
      paste(incompatible, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ray_expected_stage_files <- function(paths,
                                     raycloud_for_extract = NULL,
                                     treefile = NULL,
                                     smoothed_treefile = NULL) {
  list(
    import = file.path(paths$dirs$import, paste0(ray_stem_no_ext(paths$input_file), "_raycloud.ply")),
    colour = if (!is.null(raycloud_for_extract)) file.path(paths$dirs$colour, paste0(ray_stem_no_ext(raycloud_for_extract), "_coloured.ply")) else NA_character_,
    terrain = if (!is.null(raycloud_for_extract)) file.path(paths$dirs$terrain, paste0(ray_stem_no_ext(raycloud_for_extract), "_mesh.ply")) else NA_character_,
    trees = if (!is.null(raycloud_for_extract)) list(
      segmented_ply = file.path(paths$dirs$trees, paste0(ray_stem_no_ext(raycloud_for_extract), "_segmented.ply")),
      treefile = file.path(paths$dirs$trees, paste0(ray_stem_no_ext(raycloud_for_extract), "_trees.txt")),
      combined_mesh = file.path(paths$dirs$trees, paste0(ray_stem_no_ext(raycloud_for_extract), "_trees_mesh.ply"))
    ) else NULL,
    smooth = if (!is.null(treefile)) file.path(paths$dirs$smooth, paste0(ray_stem_no_ext(treefile), "_smoothed.txt")) else NA_character_,
    info = if (!is.null(smoothed_treefile)) file.path(paths$dirs$info, paste0(ray_stem_no_ext(smoothed_treefile), "_info.txt")) else NA_character_,
    mesh = if (!is.null(smoothed_treefile)) file.path(paths$dirs$mesh, paste0(ray_stem_no_ext(smoothed_treefile), "_mesh.ply")) else NA_character_
  )
}

ray_run_docker <- function(cmd,
                           project_root,
                           docker_image,
                           mount_dir = "/workspace/project",
                           dry_run = FALSE,
                           echo = interactive(),
                           log_file = NULL,
                           error_on_status = TRUE) {
  project_root <- ray_norm_path(project_root, must_work = TRUE)
  mount_spec <- sprintf("%s:%s", ray_to_posix(project_root), mount_dir)

  args <- c(
    "run", "--rm",
    "-v", mount_spec,
    "-w", mount_dir,
    docker_image,
    "bash", "-lc", cmd
  )

  if (isTRUE(dry_run)) {
    ray_write_log(log_file, "COMMAND: docker ", paste(args, collapse = " "))
    return(list(status = 0L, output = character(), args = args, cmd = cmd, dry_run = TRUE, failed = FALSE))
  }

  if (.Platform$OS.type == "windows") {
    escaped_cmd <- gsub('"', '\\\"', cmd, fixed = TRUE)
    full_cmd <- paste(
      "docker run --rm",
      "-v", shQuote(mount_spec, type = "cmd"),
      "-w", shQuote(mount_dir, type = "cmd"),
      docker_image,
      "bash -lc",
      shQuote(escaped_cmd, type = "cmd")
    )
    ray_write_log(log_file, "COMMAND: ", full_cmd)
    out <- tryCatch(
      system(full_cmd, intern = TRUE, ignore.stderr = FALSE),
      error = function(e) structure(conditionMessage(e), status = 999L)
    )
    args_used <- NULL
  } else {
    ray_write_log(log_file, "COMMAND: docker ", paste(args, collapse = " "))
    out <- tryCatch(
      system2("docker", args, stdout = TRUE, stderr = TRUE),
      error = function(e) structure(conditionMessage(e), status = 999L)
    )
    args_used <- args
  }

  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  if (length(out)) ray_write_log(log_file, paste(out, collapse = "\n"))
  ray_write_log(log_file, "EXIT STATUS: ", status)
  if (isTRUE(echo) && length(out)) cat(paste(out, collapse = "\n"), "\n")

  failed <- !identical(status, 0L)
  if (isTRUE(error_on_status) && failed) {
    stop(
      "Docker command failed with exit status ", status, ".\\n",
      if (length(out)) paste(out, collapse = "\n") else "No command output captured.",
      call. = FALSE
    )
  }

  list(status = status, output = out, args = args_used, cmd = cmd, dry_run = FALSE, failed = failed)
}


ray_first_existing <- function(paths) {
  paths <- as.character(paths)
  hits <- paths[file.exists(paths)]
  if (length(hits)) hits[[1]] else NA_character_
}