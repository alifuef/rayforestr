# Tree operations ---------------------------------------------------------

ray_split_segmented <- function(segmented_ply,
                                project_root,
                                out_dir,
                                split_mode = "seg_colour",
                                args_split = "",
                                docker_image = ray_default_raycloud_image(),
                                mount_dir = "/workspace/project",
                                dry_run = FALSE,
                                echo = interactive(),
                                log_file = NULL,
                                skip_existing = FALSE) {
  segmented_ply <- ray_norm_path(segmented_ply, must_work = !dry_run)
  project_root <- ray_norm_path(project_root, must_work = TRUE)
  out_dir <- ray_norm_path(out_dir, must_work = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (!dry_run && isTRUE(skip_existing)) {
    existing_ply <- list.files(out_dir, pattern = "\\.ply$", full.names = TRUE)
    if (length(existing_ply)) {
      ray_write_log(log_file, "Skipping raysplit; existing split files found in: ", out_dir)
      return(ray_skip_result(
        step = "ray_split_segmented",
        input = segmented_ply,
        outputs = list(split_segmented = existing_ply),
        log_file = log_file
      ))
    }
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- ray_localize_input(segmented_ply, out_dir, dry_run = dry_run)
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(args_split)
  cmd <- sprintf(
    "mkdir -p %s && cd %s && raysplit %s %s%s",
    ray_bash_quote(c_out_dir),
    ray_bash_quote(c_out_dir),
    ray_bash_quote(localized$basename),
    split_mode,
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

  out_files <- if (dry_run) character() else ray_stage_new_files(before, out_dir, pattern = "\\.ply$", exclude = localized$stage_path)
  if (!dry_run && !length(out_files)) {
    stop("raysplit created no new PLY files in: ", out_dir, call. = FALSE)
  }

  new_ray_result(
    step = "ray_split_segmented",
    input = segmented_ply,
    outputs = list(split_segmented = out_files),
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

ray_smooth <- function(treefile,
                       project_root,
                       out_dir,
                       args_smooth = "",
                       docker_image = ray_default_treetools_image(),
                       mount_dir = "/workspace/project",
                       dry_run = FALSE,
                       echo = interactive(),
                       log_file = NULL,
                       skip_existing = FALSE) {
  treefile <- ray_norm_path(treefile, must_work = !dry_run)
  project_root <- ray_norm_path(project_root, must_work = TRUE)
  out_dir <- ray_norm_path(out_dir, must_work = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  expected <- file.path(out_dir, paste0(ray_stem_no_ext(treefile), "_smoothed.txt"))
  if (!dry_run && isTRUE(skip_existing) && file.exists(expected)) {
    ray_write_log(log_file, "Skipping treesmooth; existing output found: ", expected)
    return(ray_skip_result(
      step = "ray_smooth",
      input = treefile,
      outputs = list(smoothed_treefile = expected),
      log_file = log_file
    ))
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- ray_localize_input(treefile, out_dir, dry_run = dry_run)
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(args_smooth)
  cmd <- sprintf(
    "mkdir -p %s && cd %s && treesmooth %s%s",
    ray_bash_quote(c_out_dir),
    ray_bash_quote(c_out_dir),
    ray_bash_quote(localized$basename),
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
    new_txt <- ray_stage_new_files(before, out_dir, pattern = "_smoothed\\.txt$", exclude = localized$stage_path)
    if (length(new_txt)) expected <- ray_find_newest(new_txt)
  }
  if (!dry_run) ray_check_output(expected, "treesmooth output")

  new_ray_result(
    step = "ray_smooth",
    input = treefile,
    outputs = list(smoothed_treefile = expected),
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

ray_mesh <- function(treefile,
                     project_root,
                     out_dir,
                     args_mesh = "",
                     docker_image = ray_default_treetools_image(),
                     mount_dir = "/workspace/project",
                     dry_run = FALSE,
                     echo = interactive(),
                     log_file = NULL,
                     skip_existing = FALSE) {
  treefile <- ray_norm_path(treefile, must_work = !dry_run)
  project_root <- ray_norm_path(project_root, must_work = TRUE)
  out_dir <- ray_norm_path(out_dir, must_work = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  expected <- file.path(out_dir, paste0(ray_stem_no_ext(treefile), "_mesh.ply"))
  if (!dry_run && isTRUE(skip_existing) && file.exists(expected)) {
    ray_write_log(log_file, "Skipping treemesh; existing output found: ", expected)
    return(ray_skip_result(
      step = "ray_mesh",
      input = treefile,
      outputs = list(mesh_file = expected),
      log_file = log_file
    ))
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- ray_localize_input(treefile, out_dir, dry_run = dry_run)
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(args_mesh)
  cmd <- sprintf(
    "mkdir -p %s && cd %s && treemesh %s%s",
    ray_bash_quote(c_out_dir),
    ray_bash_quote(c_out_dir),
    ray_bash_quote(localized$basename),
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
    new_ply <- ray_stage_new_files(before, out_dir, pattern = "\\.ply$", exclude = localized$stage_path)
    if (length(new_ply)) expected <- ray_find_newest(new_ply)
  }
  if (!dry_run) ray_check_output(expected, "treemesh output")

  new_ray_result(
    step = "ray_mesh",
    input = treefile,
    outputs = list(mesh_file = expected),
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

ray_validate_single_tree <- function(treefile) {
  parsed <- ray_read_treefile(treefile)
  n <- if (is.list(parsed) && !is.data.frame(parsed)) length(parsed) else length(unique(parsed$tree_id))
  list(ok = identical(n, 1L), n_trees = n, treefile = treefile)
}
