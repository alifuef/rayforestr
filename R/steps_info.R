# treeinfo step -----------------------------------------------------------

ray_info <- function(treefile,
                     project_root,
                     out_dir,
                     args_info = "",
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

  expected <- file.path(out_dir, paste0(ray_stem_no_ext(treefile), "_info.txt"))
  if (!dry_run && isTRUE(skip_existing) && file.exists(expected)) {
    ray_write_log(log_file, "Skipping treeinfo; existing output found: ", expected)
    return(ray_skip_result(
      step = "ray_info",
      input = treefile,
      outputs = list(info_file = expected),
      log_file = log_file
    ))
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- ray_localize_input(treefile, out_dir, dry_run = dry_run)
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(args_info)
  cmd <- sprintf(
    "mkdir -p %s && cd %s && treeinfo %s%s",
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
    new_txt <- ray_stage_new_files(before, out_dir, pattern = "_info\\.txt$", exclude = localized$stage_path)
    if (length(new_txt)) expected <- ray_find_newest(new_txt)
  }
  if (!dry_run) ray_check_output(expected, "treeinfo output")

  new_ray_result(
    step = "ray_info",
    input = treefile,
    outputs = list(info_file = expected),
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
