# Terrain and tree extraction --------------------------------------------

ray_terrain <- function(raycloud_file,
                        project_root,
                        out_dir,
                        args_terrain = "",
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

  expected <- file.path(out_dir, paste0(ray_stem_no_ext(raycloud_file), "_mesh.ply"))
  if (!dry_run && isTRUE(skip_existing) && file.exists(expected)) {
    ray_write_log(log_file, "Skipping terrain extraction; existing output found: ", expected)
    return(ray_skip_result(
      step = "ray_terrain",
      input = raycloud_file,
      outputs = list(terrain_mesh = expected),
      log_file = log_file
    ))
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- ray_localize_input(raycloud_file, out_dir, dry_run = dry_run)
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(args_terrain)
  cmd <- sprintf(
    "mkdir -p %s && cd %s && rayextract terrain %s%s",
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
    new_ply <- ray_stage_new_files(before, out_dir, pattern = "_mesh\\.ply$", exclude = localized$stage_path)
    if (length(new_ply)) expected <- ray_find_newest(new_ply)
  }
  if (!dry_run) ray_check_output(expected, "terrain mesh")

  new_ray_result(
    step = "ray_terrain",
    input = raycloud_file,
    outputs = list(terrain_mesh = expected),
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

ray_trees <- function(raycloud_file,
                      terrain_mesh,
                      project_root,
                      out_dir,
                      args_trees = "",
                      docker_image = ray_default_raycloud_image(),
                      mount_dir = "/workspace/project",
                      dry_run = FALSE,
                      echo = interactive(),
                      log_file = NULL,
                      skip_existing = FALSE) {
  raycloud_file <- ray_norm_path(raycloud_file, must_work = !dry_run)
  terrain_mesh <- ray_norm_path(terrain_mesh, must_work = !dry_run)
  project_root <- ray_norm_path(project_root, must_work = TRUE)
  out_dir <- ray_norm_path(out_dir, must_work = FALSE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  stem <- ray_stem_no_ext(raycloud_file)
  segmented <- file.path(out_dir, paste0(stem, "_segmented.ply"))
  treefile <- file.path(out_dir, paste0(stem, "_trees.txt"))
  combined_mesh <- file.path(out_dir, paste0(stem, "_trees_mesh.ply"))
  required_outputs <- c(segmented, treefile)

  if (!dry_run && isTRUE(skip_existing) && all(file.exists(required_outputs))) {
    ray_write_log(log_file, "Skipping tree extraction; existing outputs found in: ", out_dir)
    return(ray_skip_result(
      step = "ray_trees",
      input = c(raycloud_file, terrain_mesh),
      outputs = list(
        segmented_ply = segmented,
        treefile = treefile,
        combined_mesh = ray_first_existing(combined_mesh)
      ),
      log_file = log_file
    ))
  }

  before <- ray_stage_snapshot(out_dir, dry_run = dry_run)
  localized <- list(
    raycloud = ray_localize_input(raycloud_file, out_dir, dry_run = dry_run),
    terrain_mesh = ray_localize_input(terrain_mesh, out_dir, dry_run = dry_run)
  )
  on.exit(ray_cleanup_localized_inputs(localized, dry_run = dry_run, log_file = log_file), add = TRUE)

  c_out_dir <- ray_docker_path(out_dir, project_root, mount_dir)
  extra <- ray_compose_extra_args(args_trees)
  cmd <- sprintf(
    "mkdir -p %s && cd %s && rayextract trees %s %s%s",
    ray_bash_quote(c_out_dir),
    ray_bash_quote(c_out_dir),
    ray_bash_quote(localized$raycloud$basename),
    ray_bash_quote(localized$terrain_mesh$basename),
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

  if (!dry_run && !all(file.exists(required_outputs))) {
    new_files <- ray_stage_new_files(
      before,
      out_dir,
      pattern = "(_segmented\\.ply$|_trees\\.txt$|_trees_mesh\\.ply$)",
      exclude = c(localized$raycloud$stage_path, localized$terrain_mesh$stage_path)
    )
    if (!file.exists(segmented)) {
      cand <- new_files[grepl("_segmented\\.ply$", basename(new_files))]
      if (length(cand)) segmented <- ray_find_newest(cand)
    }
    if (!file.exists(treefile)) {
      cand <- new_files[grepl("_trees\\.txt$", basename(new_files))]
      if (length(cand)) treefile <- ray_find_newest(cand)
    }
    if (!file.exists(combined_mesh)) {
      cand <- new_files[grepl("_trees_mesh\\.ply$", basename(new_files))]
      if (length(cand)) combined_mesh <- ray_find_newest(cand)
    }
    required_outputs <- c(segmented, treefile)
  }
  if (!dry_run) ray_check_output(required_outputs, "tree extraction output")

  new_ray_result(
    step = "ray_trees",
    input = c(raycloud_file, terrain_mesh),
    outputs = list(
      segmented_ply = segmented,
      treefile = treefile,
      combined_mesh = combined_mesh
    ),
    command = res$cmd,
    status = if (dry_run) "dry_run" else "success",
    log_file = log_file,
    details = list(
      out_dir = out_dir,
      localized_input = c(
        raycloud = localized$raycloud$method,
        terrain_mesh = localized$terrain_mesh$method
      ),
      cleanup_localized_input = TRUE
    )
  )
}
