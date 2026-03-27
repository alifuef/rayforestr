# Export helpers for rayforestr

#' Export tree metrics to CSV
#'
#' @param x A data.frame of metrics, or a QSM-like table
#' @param file Output CSV path
#' @param info Optional info file path or data.frame, only used when `x` is a QSM table
#' @param overwrite Overwrite existing file
#' @param ... Additional arguments passed to ray_tree_metrics() when `x` is a QSM table
#'
#' @return Invisibly returns the written data.frame
#' @export
ray_export_metrics <- function(
  x,
  file,
  info = NULL,
  overwrite = FALSE,
  ...
) {
  if (file.exists(file) && !overwrite) {
    stop("File already exists: ", file, call. = FALSE)
  }

  out <- x

  # If x looks like a QSM table, compute metrics first
  qsm_cols <- c("tree_id", "start_x", "start_y", "start_z", "end_x", "end_y", "end_z", "radius", "segment_length")
  if (is.data.frame(x) && all(qsm_cols %in% names(x))) {
    out <- ray_tree_metrics(qsm = x, info = info, ...)
  }

  if (!is.data.frame(out)) {
    stop("x must be a data.frame or QSM-like table", call. = FALSE)
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, file, row.names = FALSE)
  invisible(out)
}

#' Export COG summary to CSV
#'
#' @param x Output of ray_compute_cog() / ray_compute_cog_single(), or a QSM-like table
#' @param file Output CSV path
#' @param overwrite Overwrite existing file
#' @param ... Additional arguments passed to ray_compute_cog() when `x` is a QSM table
#'
#' @return Invisibly returns the written data.frame
#' @export
ray_export_cog <- function(
  x,
  file,
  overwrite = FALSE,
  ...
) {
  if (file.exists(file) && !overwrite) {
    stop("File already exists: ", file, call. = FALSE)
  }

  out <- x

  qsm_cols <- c("tree_id", "start_x", "start_y", "start_z", "end_x", "end_y", "end_z", "radius", "segment_length")
  if (is.data.frame(x) && all(qsm_cols %in% names(x))) {
    out <- ray_compute_cog(qsm = x, ...)
  }

  if (!is.data.frame(out)) {
    stop("x must be a data.frame or QSM-like table", call. = FALSE)
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, file, row.names = FALSE)
  invisible(out)
}

#' Export both metrics and COG CSVs
#'
#' @param qsm QSM-like table
#' @param out_dir Output directory
#' @param prefix File prefix
#' @param info Optional info file path or data.frame
#' @param overwrite Overwrite existing files
#' @param ... Additional arguments passed to ray_tree_metrics() / ray_compute_cog()
#'
#' @return Named list with paths and written data.frames
#' @export
ray_export_all <- function(
  qsm,
  out_dir,
  prefix = "rayforestr",
  info = NULL,
  overwrite = FALSE,
  ...
) {
  if (!is.data.frame(qsm)) {
    stop("qsm must be a data.frame", call. = FALSE)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  metrics_file <- file.path(out_dir, paste0(prefix, "_metrics.csv"))
  cog_file <- file.path(out_dir, paste0(prefix, "_cog.csv"))

  metrics_df <- ray_export_metrics(
    x = qsm,
    file = metrics_file,
    info = info,
    overwrite = overwrite,
    ...
  )

  cog_df <- ray_export_cog(
    x = qsm,
    file = cog_file,
    overwrite = overwrite,
    ...
  )

  list(
    metrics_file = metrics_file,
    cog_file = cog_file,
    metrics = metrics_df,
    cog = cog_df
  )
}
