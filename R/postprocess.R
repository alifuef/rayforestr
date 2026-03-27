# Postprocessing helpers for rayforestr

#' Get default project paths
#'
#' @param project_root Root project directory
#'
#' @return Named list of standard directories
#' @export
ray_default_paths <- function(project_root = "D:/packageR") {
  list(
    project_root = project_root,
    forest_dir = file.path(project_root, "data", "forest_plot"),
    single_tree_dir = file.path(project_root, "data", "single_tree"),
    output_dir = file.path(project_root, "output"),
    log_dir = file.path(project_root, "logs")
  )
}

#' List forest plot input files
#'
#' @param project_root Root project directory
#' @param pattern File pattern
#'
#' @return Character vector of input paths
#' @export
ray_list_forest_inputs <- function(project_root = "D:/packageR", pattern = "\\.ply$") {
  p <- ray_default_paths(project_root)$forest_dir
  if (!dir.exists(p)) return(character(0))
  list.files(p, pattern = pattern, full.names = TRUE)
}

#' List single-tree input files
#'
#' @param project_root Root project directory
#' @param pattern File pattern
#'
#' @return Character vector of input paths
#' @export
ray_list_single_tree_inputs <- function(project_root = "D:/packageR", pattern = "\\.ply$") {
  p <- ray_default_paths(project_root)$single_tree_dir
  if (!dir.exists(p)) return(character(0))
  list.files(p, pattern = pattern, full.names = TRUE)
}

.resolve_postprocess_input <- function(x, info = NULL) {
  qsm_cols <- c("tree_id", "start_x", "start_y", "start_z", "end_x", "end_y", "end_z", "radius", "segment_length")

  if (is.data.frame(x) && all(qsm_cols %in% names(x))) {
    return(list(qsm = x, info = info, source = "qsm"))
  }

  if (is.data.frame(x) && "total_volume" %in% names(x) && "tree_id" %in% names(x)) {
    stop("x looks like a metrics table. Pass a QSM table, info file, treefile, or pipeline result.", call. = FALSE)
  }

  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    if (grepl("_info\\.txt$", basename(x), ignore.case = TRUE)) {
      return(list(qsm = ray_treefile_to_qsm(x), info = x, source = "info_file"))
    }
    if (grepl("\\.txt$", basename(x), ignore.case = TRUE)) {
      return(list(qsm = ray_treefile_to_qsm(x), info = info, source = "treefile"))
    }
    stop("Unsupported input file for postprocessing: ", x, call. = FALSE)
  }

  if (is.list(x)) {
    out <- if (!is.null(x$outputs) && is.list(x$outputs)) x$outputs else x

    info_file <- NULL
    treefile <- NULL

    if ("info_file" %in% names(out) && !is.null(out$info_file) && file.exists(out$info_file)) {
      info_file <- out$info_file
    }

    if ("smoothed_treefile" %in% names(out) && !is.null(out$smoothed_treefile) && file.exists(out$smoothed_treefile)) {
      treefile <- out$smoothed_treefile
    } else if ("treefile" %in% names(out) && !is.null(out$treefile) && file.exists(out$treefile)) {
      treefile <- out$treefile
    }

    if (!is.null(info_file)) {
      return(list(qsm = ray_treefile_to_qsm(info_file), info = info_file, source = "pipeline_info"))
    }
    if (!is.null(treefile)) {
      return(list(qsm = ray_treefile_to_qsm(treefile), info = info, source = "pipeline_treefile"))
    }
  }

  stop("Could not resolve postprocessing input. Pass a QSM data.frame, info file, treefile, or pipeline result.", call. = FALSE)
}

#' Postprocess one tree into QSM, metrics, COG, CSVs and optional plot
#'
#' @param x QSM table, info file, treefile, or pipeline result
#' @param info Optional info file path or data.frame
#' @param out_dir Output directory
#' @param prefix Output file prefix
#' @param merch_min_diam Minimum merchantable diameter in metres
#' @param density_trunk Wood density for trunk cylinders (kg/m^3)
#' @param density_branch Wood density for branch cylinders (kg/m^3)
#' @param branch_factor Optional multiplier for branch mass
#' @param export_csv If TRUE, write CSVs
#' @param make_plot If TRUE, write compass plot
#' @param overwrite Overwrite existing output files
#'
#' @return Named list containing qsm, metrics, cog and output paths
#' @export
ray_postprocess_tree <- function(
  x,
  info = NULL,
  out_dir = file.path("D:/packageR", "output"),
  prefix = NULL,
  merch_min_diam = 0.07,
  density_trunk = 600,
  density_branch = 600,
  branch_factor = 1,
  export_csv = TRUE,
  make_plot = TRUE,
  overwrite = TRUE
) {
  resolved <- .resolve_postprocess_input(x, info = info)
  qsm <- resolved$qsm
  info_use <- resolved$info

  qsm2 <- ray_classify_stem(qsm)
  metrics <- ray_tree_metrics(
    qsm = qsm2,
    info = info_use,
    merch_min_diam = merch_min_diam,
    density_trunk = density_trunk,
    density_branch = density_branch,
    branch_factor = branch_factor
  )
  cog <- ray_compute_cog(
    qsm = qsm2,
    density_trunk = density_trunk,
    density_branch = density_branch,
    branch_factor = branch_factor
  )

  if (is.null(prefix)) {
    if (is.character(x) && length(x) == 1L && file.exists(x)) {
      prefix <- tools::file_path_sans_ext(basename(x))
    } else {
      prefix <- "tree"
    }
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  metrics_file <- file.path(out_dir, paste0(prefix, "_metrics.csv"))
  cog_file <- file.path(out_dir, paste0(prefix, "_cog.csv"))
  plot_file <- file.path(out_dir, paste0(prefix, "_cog_compass.png"))

  if (isTRUE(export_csv)) {
    ray_export_metrics(metrics, metrics_file, overwrite = overwrite)
    ray_export_cog(cog, cog_file, overwrite = overwrite)
  }

  if (isTRUE(make_plot)) {
    ray_plot_cog_compass(cog, file = plot_file)
  }

  list(
    qsm = qsm2,
    metrics = metrics,
    cog = cog,
    metrics_file = if (export_csv) metrics_file else NA_character_,
    cog_file = if (export_csv) cog_file else NA_character_,
    plot_file = if (make_plot) plot_file else NA_character_
  )
}

#' Postprocess forest outputs into QSM, metrics, summary, CSVs and optional map
#'
#' @param x QSM table, info file, treefile, or pipeline result
#' @param info Optional info file path or data.frame
#' @param out_dir Output directory
#' @param prefix Output file prefix
#' @param merch_min_diam Minimum merchantable diameter in metres
#' @param density_trunk Wood density for trunk cylinders (kg/m^3)
#' @param density_branch Wood density for branch cylinders (kg/m^3)
#' @param branch_factor Optional multiplier for branch mass
#' @param export_csv If TRUE, write CSVs
#' @param make_plot If TRUE, write forest map
#' @param overwrite Overwrite existing output files
#'
#' @return Named list containing qsm, metrics, summary and output paths
#' @export
ray_postprocess_forest <- function(
  x,
  info = NULL,
  out_dir = file.path("D:/packageR", "output"),
  prefix = NULL,
  merch_min_diam = 0.07,
  density_trunk = 600,
  density_branch = 600,
  branch_factor = 1,
  export_csv = TRUE,
  make_plot = TRUE,
  overwrite = TRUE
) {
  resolved <- .resolve_postprocess_input(x, info = info)
  qsm <- resolved$qsm
  info_use <- resolved$info

  qsm2 <- ray_classify_stem(qsm)
  metrics <- ray_tree_metrics(
    qsm = qsm2,
    info = info_use,
    merch_min_diam = merch_min_diam,
    density_trunk = density_trunk,
    density_branch = density_branch,
    branch_factor = branch_factor
  )
  summary_df <- ray_forest_summary(metrics)

  if (is.null(prefix)) {
    if (is.character(x) && length(x) == 1L && file.exists(x)) {
      prefix <- tools::file_path_sans_ext(basename(x))
    } else {
      prefix <- "forest"
    }
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  metrics_file <- file.path(out_dir, paste0(prefix, "_metrics.csv"))
  summary_file <- file.path(out_dir, paste0(prefix, "_summary.csv"))
  map_file <- file.path(out_dir, paste0(prefix, "_forest_map.png"))

  if (isTRUE(export_csv)) {
    ray_export_metrics(metrics, metrics_file, overwrite = overwrite)
    utils::write.csv(summary_df, summary_file, row.names = FALSE)
  }

  if (isTRUE(make_plot)) {
    ray_plot_forest_map(metrics, file = map_file)
  }

  list(
    qsm = qsm2,
    metrics = metrics,
    summary = summary_df,
    metrics_file = if (export_csv) metrics_file else NA_character_,
    summary_file = if (export_csv) summary_file else NA_character_,
    map_file = if (make_plot) map_file else NA_character_
  )
}
