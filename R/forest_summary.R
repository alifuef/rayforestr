#' Summarise forest-level metrics
#'
#' @param x A tree-level metrics table, or a QSM-like table
#' @param info Optional info file path or data.frame if `x` is a QSM table
#' @param ... Additional arguments passed to ray_tree_metrics() when `x` is a QSM table
#'
#' @return One-row data.frame with forest-level summary values
#' @export
ray_forest_summary <- function(x, info = NULL, ...) {
  df <- x

  qsm_cols <- c(
    "tree_id", "start_x", "start_y", "start_z",
    "end_x", "end_y", "end_z", "radius", "segment_length"
  )

  if (is.data.frame(x) && all(qsm_cols %in% names(x))) {
    df <- ray_tree_metrics(qsm = x, info = info, ...)
  }

  if (!is.data.frame(df)) {
    stop("x must be a metrics table or a QSM-like table", call. = FALSE)
  }

  req <- c(
    "tree_id", "total_volume", "trunk_volume", "branch_volume",
    "merchantable_volume", "trunk_mass", "branch_mass", "total_mass"
  )
  miss <- req[!req %in% names(df)]
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  one_num <- function(name, fun = mean, na.rm = TRUE) {
    if (name %in% names(df)) fun(df[[name]], na.rm = na.rm) else NA_real_
  }

  out <- data.frame(
    n_trees = length(unique(df$tree_id)),
    total_volume = sum(df$total_volume, na.rm = TRUE),
    total_trunk_volume = sum(df$trunk_volume, na.rm = TRUE),
    total_branch_volume = sum(df$branch_volume, na.rm = TRUE),
    total_merchantable_volume = sum(df$merchantable_volume, na.rm = TRUE),
    total_trunk_mass = sum(df$trunk_mass, na.rm = TRUE),
    total_branch_mass = sum(df$branch_mass, na.rm = TRUE),
    total_mass = sum(df$total_mass, na.rm = TRUE),
    mean_height = one_num("height", mean),
    max_height = one_num("height", max),
    mean_DBH = if ("DBH" %in% names(df)) {
      mean(df$DBH, na.rm = TRUE)
    } else if ("dbh" %in% names(df)) {
      mean(df$dbh, na.rm = TRUE)
    } else {
      NA_real_
    },
    mean_crown_radius = one_num("crown_radius", mean),
    total_crown_projection_area = if ("crown_projection_area" %in% names(df)) {
      sum(df$crown_projection_area, na.rm = TRUE)
    } else {
      NA_real_
    },
    mean_horizontal_distance = one_num("horizontal_distance", mean)
  )

  out
}
