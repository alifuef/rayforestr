# Plot helpers ------------------------------------------------------------

#' Plot COG compass
#'
#' @param res Output of ray_compute_cog()/ray_compute_cog_single(), or a QSM table
#' @param max_radius Optional radius to use for the compass
#' @param file Optional output PNG path
#' @param width Width in pixels for PNG export
#' @param height Height in pixels for PNG export
#' @param pad_factor Extra padding around the outer circle
#' @param label_offset Relative label offset from the COG point
#'
#' @return Invisibly returns the plotting data.frame
#' @export
ray_plot_cog_compass <- function(
  res,
  max_radius = NULL,
  file = NULL,
  width = 800,
  height = 800,
  pad_factor = 1.12,
  label_offset = 0.08
) {
  df <- res

  qsm_cols <- c("tree_id", "start_x", "start_y", "start_z", "end_x", "end_y", "end_z", "radius", "segment_length")
  if (is.data.frame(res) && all(qsm_cols %in% names(res))) {
    df <- ray_compute_cog(res)
  }

  if (!is.data.frame(df) || nrow(df) < 1) {
    stop("res must be a data.frame with at least one row.", call. = FALSE)
  }

  df <- df[1, , drop = FALSE]

  if (all(c("cog_x", "cog_y") %in% names(df))) {
    dx <- df$cog_x
    dy <- df$cog_y
  } else if (all(c("CG_x", "CG_y", "BaseStem_X", "BaseStem_Y") %in% names(df))) {
    dx <- df$CG_x - df$BaseStem_X
    dy <- df$CG_y - df$BaseStem_Y
  } else {
    stop("Could not find COG/base columns.", call. = FALSE)
  }

  tree_name <- if ("TreeName" %in% names(df)) {
    as.character(df$TreeName[1])
  } else if ("tree_id" %in% names(df)) {
    paste0("Tree ", df$tree_id[1])
  } else {
    "Tree"
  }

  r <- sqrt(dx^2 + dy^2)
  bearing_deg <- (atan2(dx, dy) * 180 / pi) %% 360

  if (is.null(max_radius)) {
    max_radius <- max(1, r * 1.4)
  }

  plot_radius <- max_radius * pad_factor
  theta <- seq(0, 2 * pi, length.out = 300)

  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    grDevices::png(filename = file, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  graphics::plot(
    NA, NA,
    xlim = c(-plot_radius, plot_radius),
    ylim = c(-plot_radius, plot_radius),
    asp = 1,
    xlab = "East-West offset (m)",
    ylab = "North-South offset (m)",
    main = paste0("COG compass view: ", tree_name)
  )

  for (rr in pretty(c(0, max_radius))) {
    graphics::lines(rr * cos(theta), rr * sin(theta), lty = 3)
  }

  graphics::abline(h = 0, v = 0, lty = 2)
  graphics::text(0,  max_radius * 1.03, "N", pos = 3)
  graphics::text(max_radius * 1.03, 0,  "E", pos = 4)
  graphics::text(0, -max_radius * 1.03, "S", pos = 1)
  graphics::text(-max_radius * 1.03, 0, "W", pos = 2)

  graphics::points(0, 0, pch = 16, cex = 1.3)
  graphics::text(0, 0, "Base", pos = 3)

  graphics::arrows(0, 0, dx, dy, lwd = 2, length = 0.1)
  graphics::points(dx, dy, pch = 17, cex = 1.5)

  off <- max_radius * label_offset
  text_x <- dx + ifelse(dx >= 0, off, -off)
  text_y <- dy + ifelse(dy >= 0, off, -off)

  graphics::text(
    text_x, text_y,
    labels = paste0("COG\n", round(r, 2), " m\n", round(bearing_deg, 1), "\\u00B0"),
    cex = 0.9
  )

  invisible(df)
}

#' Plot forest map from metrics or info-based data
#'
#' @param x Metrics table or QSM-like table
#' @param info Optional info file path/data.frame if x is QSM
#' @param file Optional output PNG path
#' @param width Width in pixels for PNG export
#' @param height Height in pixels for PNG export
#'
#' @return Invisibly returns the plotting data.frame
#' @export
ray_plot_forest_map <- function(
  x,
  info = NULL,
  file = NULL,
  width = 900,
  height = 700
) {
  df <- x

  qsm_cols <- c("tree_id", "start_x", "start_y", "start_z", "end_x", "end_y", "end_z", "radius", "segment_length")
  if (is.data.frame(x) && all(qsm_cols %in% names(x))) {
    df <- ray_tree_metrics(qsm = x, info = info)
  }

  if (!is.data.frame(df)) {
    stop("x must be a metrics table or a QSM-like table", call. = FALSE)
  }

  if (all(c("info_x", "info_y") %in% names(df))) {
    plot_x <- df$info_x
    plot_y <- df$info_y
  } else if (all(c("base_x", "base_y") %in% names(df))) {
    plot_x <- df$base_x
    plot_y <- df$base_y
  } else {
    stop("Need info_x/info_y or base_x/base_y columns to plot forest map", call. = FALSE)
  }

  crown_r <- if ("crown_radius" %in% names(df)) df$crown_radius else rep(0, nrow(df))

  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    grDevices::png(filename = file, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  graphics::plot(
    plot_x, plot_y,
    xlab = "X",
    ylab = "Y",
    asp = 1,
    pch = 19,
    main = "Forest map"
  )

  if (any(crown_r > 0, na.rm = TRUE)) {
    theta <- seq(0, 2 * pi, length.out = 200)
    for (i in seq_along(plot_x)) {
      if (is.finite(crown_r[i]) && crown_r[i] > 0) {
        graphics::lines(
          plot_x[i] + crown_r[i] * cos(theta),
          plot_y[i] + crown_r[i] * sin(theta)
        )
      }
    }
  }

  graphics::text(plot_x, plot_y, labels = df$tree_id, pos = 3)

  invisible(df)
}
