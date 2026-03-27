# Import and colour steps -------------------------------------------------

# Import diagnostics ------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

ray_guess_sibling_raycloud <- function(input_file) {
  input_file <- ray_norm_path(input_file, must_work = FALSE)
  candidate <- file.path(dirname(input_file), paste0(ray_stem_no_ext(input_file), "_raycloud.ply"))
  if (file.exists(candidate)) ray_norm_path(candidate, must_work = TRUE) else NA_character_
}

ray_detect_import_issue <- function(output) {
  txt <- paste(output %||% character(), collapse = "\n")
  if (!nzchar(txt)) return(NA_character_)
  if (grepl("zero-intensity non-returns", txt, ignore.case = TRUE, perl = TRUE)) return("zero_intensity_nonreturns")
  if (grepl("has no identified points", txt, ignore.case = TRUE, perl = TRUE)) return("no_identified_points")
  if (grepl("Couldn't open file", txt, ignore.case = TRUE, perl = TRUE)) return("could_not_open_input")
  NA_character_
}

ray_import_failure_message <- function(input_file,
                                       expected,
                                       issue = NA_character_,
                                       existing_candidates = character(),
                                       output = character()) {
  lines <- c(
    "rayimport did not produce an output raycloud.",
    paste0("Input: ", input_file),
    paste0("Expected output: ", expected)
  )

  if (!is.na(issue) && identical(issue, "zero_intensity_nonreturns")) {
    lines <- c(lines, "", "RayCloudTools reported a degenerate zero-intensity/non-return cloud for this import.")
  }

  if (length(existing_candidates)) {
    lines <- c(lines, "", "Existing raycloud candidate(s) detected:", paste0("  - ", existing_candidates))
  }

  lines <- c(
    lines,
    "",
    "Recommended next steps:",
    "  * If you already have a valid *_raycloud.ply, run the pipeline with input_mode = 'raycloud'.",
    "  * Or call ray_import(..., existing_raycloud = 'path/to/file_raycloud.ply', reuse_existing = 'prefer' or 'fallback').",
    "  * Or manually inspect the raw point cloud and import outside the package, then restart from the raycloud stage."
  )

  if (length(output)) {
    lines <- c(lines, "", "Last command output lines:", paste0("  ", utils::tail(output, 8)))
  }

  paste(lines, collapse = "\n")
}

#' Import a point cloud into a RayCloudTools raycloud
#'
#' Runs `rayimport` inside the stage folder and returns the resulting
#' `*_raycloud.ply`. The function can also reuse an existing raycloud when
#' raw import is unreliable or when a validated raycloud already exists.
#'
#' @param input_file Path to the raw input point cloud (`.ply`).
#' @param project_root Project root mounted into Docker.
#' @param out_dir Stage output directory for the import step.
#' @param ray_direction Character vector like `"0,0,-1"` passed to `rayimport`.
#' @param max_intensity Numeric value forwarded to `--max_intensity`.
#' @param remove_start_pos Logical; include `--remove_start_pos` when `TRUE`.
#' @param args_import Additional raw command-line arguments for `rayimport`.
#' @param existing_raycloud Optional path to an existing `*_raycloud.ply`.
#' @param reuse_existing One of `"never"`, `"fallback"`, or `"prefer"`.
#'   `prefer` reuses an existing raycloud immediately. `fallback` tries raw
#'   import first and reuses an existing raycloud only if needed.
#' @param search_sibling_raycloud Logical; search beside `input_file` for
#'   `<stem>_raycloud.ply` as a fallback candidate.
#' @param docker_image Docker image used for the import command.
#' @param mount_dir Mount point used inside the container.
#' @param dry_run Logical; construct commands without running Docker.
#' @param echo Logical; print Docker command output.
#' @param log_file Optional log file path.
#' @param skip_existing Logical; skip the step if the expected stage raycloud
#'   already exists.
#'
#' @return A `ray_result` object.
#' @export
ray_import <- function(input_file,
                       project_root,
                       out_dir,
                       ray_direction = "0,0,-1",
                       max_intensity = 0,
                       remove_start_pos = FALSE,
                       args_import = "",
                       existing_raycloud = NULL,
                       reuse_existing = c("never", "fallback", "prefer"),
                       search_sibling_raycloud = TRUE,
                       docker_image = ray_default_raycloud_image(),
                       mount_dir = "/workspace/project",
                       dry_run = FALSE,
                       echo = interactive(),
                       log_file = NULL,
                       skip_existing = FALSE) {
  reuse_existing <- match.arg(reuse_existing)

  input_file <- ray_norm_path(input_file, must_work = !dry_run)
  project_root <- ray_norm_path(project_root, must_work = TRUE)
  out_dir <- ray_norm_path(out_dir, must_work = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  expected <- file.path(out_dir, paste0(ray_stem_no_ext(input_file), "_raycloud.ply"))
  if (!is.null(existing_raycloud) && nzchar(existing_raycloud)) {
    existing_raycloud <- ray_norm_path(existing_raycloud, must_work = !dry_run)
  } else {
    existing_raycloud <- NA_character_
  }
  sibling_raycloud <- if (isTRUE(search_sibling_raycloud)) ray_guess_sibling_raycloud(input_file) else NA_character_
  existing_candidates <- unique(stats::na.omit(c(existing_raycloud, sibling_raycloud)))
  existing_candidates <- existing_candidates[nzchar(existing_candidates)]

  if (!dry_run && isTRUE(skip_existing) && file.exists(expected)) {
    ray_write_log(log_file, "Skipping rayimport; existing output found: ", expected)
    return(ray_skip_result(
      step = "ray_import",
      input = input_file,
      outputs = list(raycloud = expected),
      log_file = log_file
    ))
  }

  if (identical(reuse_existing, "prefer") && length(existing_candidates)) {
    chosen <- existing_candidates[[1]]
    ray_write_log(log_file, "Reusing existing raycloud before import: ", chosen)
    return(ray_external_input_result(
      step = "ray_import",
      input = input_file,
      outputs = list(raycloud = chosen),
      log_file = log_file,
      details = list(reuse_existing = reuse_existing, import_attempted = FALSE, fallback_used = FALSE, chosen_existing_raycloud = chosen)
    ))
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- ray_localize_input(input_file, out_dir, dry_run = dry_run)
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(
    if (isTRUE(remove_start_pos)) "--remove_start_pos" else "",
    args_import
  )

  cmd <- sprintf(
    "mkdir -p %s && cd %s && rayimport %s ray %s --max_intensity %s%s",
    ray_bash_quote(c_out_dir),
    ray_bash_quote(c_out_dir),
    ray_bash_quote(localized$basename),
    ray_direction,
    max_intensity,
    if (nzchar(extra)) paste0(" ", extra) else ""
  )

  res <- ray_run_docker(
    cmd = cmd,
    project_root = project_root,
    docker_image = docker_image,
    mount_dir = mount_dir,
    dry_run = dry_run,
    echo = echo,
    log_file = log_file,
    error_on_status = FALSE
  )

  if (!dry_run && !file.exists(expected)) {
    new_raycloud <- ray_stage_new_files(before, out_dir, pattern = "_raycloud\\.ply$", exclude = localized$stage_path)
    if (length(new_raycloud)) {
      expected <- ray_find_newest(new_raycloud)
    } else {
      new_ply <- ray_stage_new_files(before, out_dir, pattern = "\\.ply$", exclude = localized$stage_path)
      if (length(new_ply)) expected <- ray_find_newest(new_ply)
    }
  }

  if (!dry_run && file.exists(expected)) {
    return(new_ray_result(
      step = "ray_import",
      input = input_file,
      outputs = list(raycloud = expected),
      command = res$cmd,
      status = "success",
      log_file = log_file,
      details = list(
        out_dir = out_dir,
        localized_input = localized$method,
        cleanup_localized_input = TRUE,
        import_status = res$status,
        existing_candidates = existing_candidates,
        import_attempted = TRUE,
        import_failed = FALSE,
        fallback_used = FALSE
      )
    ))
  }

  if (!dry_run && identical(reuse_existing, "fallback") && length(existing_candidates)) {
    chosen <- existing_candidates[[1]]
    issue <- ray_detect_import_issue(res$output)
    ray_write_log(log_file, "rayimport did not create a new raycloud; falling back to existing raycloud: ", chosen)
    return(ray_external_input_result(
      step = "ray_import",
      input = input_file,
      outputs = list(raycloud = chosen),
      log_file = log_file,
      details = list(
        reuse_existing = reuse_existing,
        import_attempted = TRUE,
        import_failed = TRUE,
        fallback_used = TRUE,
        fallback_source = if (!is.na(existing_raycloud) && identical(chosen, existing_raycloud)) "explicit_existing_raycloud" else if (!is.na(sibling_raycloud) && identical(chosen, sibling_raycloud)) "sibling_raycloud" else "existing_candidate",
        chosen_existing_raycloud = chosen,
        import_status = res$status,
        import_issue = issue,
        command_output_tail = utils::tail(res$output %||% character(), 8)
      )
    ))
  }

  if (!dry_run) {
    issue <- ray_detect_import_issue(res$output)
    stop(
      ray_import_failure_message(
        input_file = input_file,
        expected = expected,
        issue = issue,
        existing_candidates = existing_candidates,
        output = res$output
      ),
      call. = FALSE
    )
  }

  new_ray_result(
    step = "ray_import",
    input = input_file,
    outputs = list(raycloud = expected),
    command = res$cmd,
    status = "dry_run",
    log_file = log_file,
    details = list(
      out_dir = out_dir,
      localized_input = localized$method,
      cleanup_localized_input = TRUE,
      reuse_existing = reuse_existing,
      existing_candidates = existing_candidates
    )
  )
}

#' Apply `raycolour` to an existing raycloud
#'
#' Runs `raycolour` inside the colour stage folder using a stage-local
#' localized input file.
#'
#' @param raycloud_file Path to an existing `*_raycloud.ply`.
#' @param project_root Project root mounted into Docker.
#' @param out_dir Stage output directory for the colour step.
#' @param colour_mode Raw `raycolour` mode string, for example `"alpha 1"`.
#' @param args_colour Additional raw command-line arguments passed to `raycolour`.
#' @param docker_image Docker image used for the colour command.
#' @param mount_dir Mount point used inside the container.
#' @param dry_run Logical; construct commands without running Docker.
#' @param echo Logical; print Docker command output.
#' @param log_file Optional log file path.
#' @param skip_existing Logical; skip when the expected coloured output exists.
#'
#' @return A `ray_result` object.
#' @export
ray_colour <- function(raycloud_file,
                       project_root,
                       out_dir,
                       colour_mode = "alpha 1",
                       args_colour = "",
                       docker_image = ray_default_raycloud_image(),
                       mount_dir = "/workspace/project",
                       dry_run = FALSE,
                       echo = interactive(),
                       log_file = NULL,
                       skip_existing = FALSE) {
  raycloud_file <- ray_norm_path(raycloud_file, must_work = !dry_run)
  project_root <- ray_norm_path(project_root, must_work = TRUE)
  out_dir <- ray_norm_path(out_dir, must_work = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  expected <- file.path(out_dir, paste0(ray_stem_no_ext(raycloud_file), "_coloured.ply"))
  if (!dry_run && isTRUE(skip_existing) && file.exists(expected)) {
    ray_write_log(log_file, "Skipping raycolour; existing output found: ", expected)
    return(ray_skip_result(
      step = "ray_colour",
      input = raycloud_file,
      outputs = list(coloured_raycloud = expected),
      log_file = log_file
    ))
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- ray_localize_input(raycloud_file, out_dir, dry_run = dry_run)
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(args_colour)
  cmd <- sprintf(
    "mkdir -p %s && cd %s && raycolour %s %s%s",
    ray_bash_quote(c_out_dir),
    ray_bash_quote(c_out_dir),
    ray_bash_quote(localized$basename),
    colour_mode,
    if (nzchar(extra)) paste0(" ", extra) else ""
  )

  res <- ray_run_docker(
    cmd = cmd,
    project_root = project_root,
    docker_image = docker_image,
    mount_dir = mount_dir,
    dry_run = dry_run,
    echo = echo,
    log_file = log_file
  )

  if (!dry_run && !file.exists(expected)) {
    new_col <- ray_stage_new_files(before, out_dir, pattern = "_coloured\\.ply$", exclude = localized$stage_path)
    if (length(new_col)) expected <- ray_find_newest(new_col)
  }

  if (!dry_run) ray_check_output(expected, "raycolour output")

  new_ray_result(
    step = "ray_colour",
    input = raycloud_file,
    outputs = list(coloured_raycloud = expected),
    command = res$cmd,
    status = if (dry_run) "dry_run" else "success",
    log_file = log_file,
    details = list(
      out_dir = out_dir,
      localized_input = localized$method,
      cleanup_localized_input = TRUE
    )
  )
}

debug_rayimport_once <- function(input_file,
                                 project_root = "D:/packageR",
                                 out_dir = file.path(project_root, "runs", "debug_rayimport"),
                                 ray_direction = "0,0,-1",
                                 max_intensity = 0,
                                 docker_image = ray_default_raycloud_image(),
                                 mount_dir = "/workspace/project") {
  log_file <- file.path(out_dir, "rayimport_debug.log")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res <- ray_import(
    input_file = input_file,
    project_root = project_root,
    out_dir = out_dir,
    ray_direction = ray_direction,
    max_intensity = max_intensity,
    docker_image = docker_image,
    mount_dir = mount_dir,
    dry_run = FALSE,
    echo = TRUE,
    log_file = log_file
  )

  list(
    result = res,
    files = list.files(out_dir, full.names = TRUE),
    log_file = log_file
  )
}
