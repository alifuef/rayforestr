#' Compute centre of gravity for one or more trees
#'
#' @param qsm QSM-like data.frame from ray_treefile_to_qsm()
#' @param density_trunk Wood density for trunk cylinders (kg/m^3)
#' @param density_branch Wood density for branch cylinders (kg/m^3)
#' @param branch_factor Optional multiplier for branch mass
#' @param tree_id_col Name of tree id column
#' @param part_col Name of trunk/branch classification column
#' @param shift_to_base If TRUE, return COG relative to stem base
#'
#' @return data.frame with one row per tree
#' @export
ray_compute_cog <- function(
  qsm,
  density_trunk = 600,
  density_branch = 600,
  branch_factor = 1,
  tree_id_col = "tree_id",
  part_col = "part",
  shift_to_base = TRUE
) {
  if (!is.data.frame(qsm)) {
    stop("qsm must be a data.frame", call. = FALSE)
  }

  req <- c(
    tree_id_col,
    "start_x", "start_y", "start_z",
    "end_x", "end_y", "end_z",
    "radius", "segment_length"
  )

  miss <- req[!req %in% names(qsm)]
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  x <- qsm

  if (!part_col %in% names(x)) {
    x[[part_col]] <- "trunk"
    warning("No part column found; all cylinders treated as trunk.")
  }

  x$cyl_volume <- pi * (x$radius^2) * x$segment_length

  x$density_used <- ifelse(
    x[[part_col]] == "branch",
    density_branch * branch_factor,
    density_trunk
  )

  x$mass <- x$cyl_volume * x$density_used

  x$mid_x <- (x$start_x + x$end_x) / 2
  x$mid_y <- (x$start_y + x$end_y) / 2
  x$mid_z <- (x$start_z + x$end_z) / 2

  split_qsm <- split(x, x[[tree_id_col]])

  out <- lapply(split_qsm, function(df) {
    base_idx <- which.min(df$start_z)

    base_x <- df$start_x[base_idx]
    base_y <- df$start_y[base_idx]
    base_z <- df$start_z[base_idx]

    if (shift_to_base) {
      mid_x <- df$mid_x - base_x
      mid_y <- df$mid_y - base_y
      mid_z <- df$mid_z - base_z
    } else {
      mid_x <- df$mid_x
      mid_y <- df$mid_y
      mid_z <- df$mid_z
    }

    total_mass <- sum(df$mass, na.rm = TRUE)

    if (is.na(total_mass) || total_mass == 0) {
      cog_x <- NA_real_
      cog_y <- NA_real_
      cog_z <- NA_real_
    } else {
      cog_x <- stats::weighted.mean(mid_x, df$mass, na.rm = TRUE)
      cog_y <- stats::weighted.mean(mid_y, df$mass, na.rm = TRUE)
      cog_z <- stats::weighted.mean(mid_z, df$mass, na.rm = TRUE)
    }

    trunk_mass <- sum(df$mass[df[[part_col]] == "trunk"], na.rm = TRUE)
    branch_mass <- sum(df$mass[df[[part_col]] == "branch"], na.rm = TRUE)

    horizontal_distance <- sqrt(cog_x^2 + cog_y^2)
    azimuth_deg <- (atan2(cog_y, cog_x) * 180 / pi) %% 360

    data.frame(
      tree_id = df[[tree_id_col]][1],
      base_x = base_x,
      base_y = base_y,
      base_z = base_z,
      cog_x = cog_x,
      cog_y = cog_y,
      cog_z = cog_z,
      cog_abs_x = if (shift_to_base) base_x + cog_x else cog_x,
      cog_abs_y = if (shift_to_base) base_y + cog_y else cog_y,
      cog_abs_z = if (shift_to_base) base_z + cog_z else cog_z,
      horizontal_distance = horizontal_distance,
      azimuth_deg = azimuth_deg,
      trunk_mass = trunk_mass,
      branch_mass = branch_mass,
      total_mass = total_mass
    )
  })

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

#' Compute COG for a single tree
#'
#' @param qsm_single QSM-like data.frame for one tree
#' @param ... Passed to ray_compute_cog()
#'
#' @return one-row data.frame
#' @export
ray_compute_cog_single <- function(qsm_single, ...) {
  if (!is.data.frame(qsm_single)) {
    stop("qsm_single must be a data.frame", call. = FALSE)
  }

  if (!"tree_id" %in% names(qsm_single)) {
    qsm_single$tree_id <- 1
  }

  ray_compute_cog(qsm_single, ...)
}

#' Describe centre-of-gravity output in a compact way
#'
#' @param cog_df Output from ray_compute_cog() or ray_compute_cog_single()
#'
#' @return data.frame with rounded reporting fields
#' @export
ray_describe_cog <- function(cog_df) {
  if (!is.data.frame(cog_df)) {
    stop("cog_df must be a data.frame", call. = FALSE)
  }

  req <- c(
    "tree_id", "cog_x", "cog_y", "cog_z",
    "horizontal_distance", "azimuth_deg",
    "trunk_mass", "branch_mass", "total_mass"
  )
  miss <- req[!req %in% names(cog_df)]
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  out <- cog_df[, req, drop = FALSE]
  num_cols <- setdiff(names(out), "tree_id")
  out[num_cols] <- lapply(out[num_cols], function(x) round(x, 4))
  out
}

#' Classify cylinders into trunk vs branch using the highest-tip path
#'
#' @param qsm QSM-like data.frame from ray_treefile_to_qsm()
#' @param tree_id_col Name of tree id column
#'
#' @return qsm with added column `part`
#' @export
ray_classify_stem <- function(qsm, tree_id_col = "tree_id") {
  if (!is.data.frame(qsm)) {
    stop("qsm must be a data.frame", call. = FALSE)
  }

  req <- c(tree_id_col, "node_id", "parent_id", "end_z")
  miss <- req[!req %in% names(qsm)]
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  split_qsm <- split(qsm, qsm[[tree_id_col]])

  classify_one <- function(df) {
    df <- df[order(df$node_id), , drop = FALSE]
    df$part <- "branch"

    child_nodes <- unique(df$parent_id[df$parent_id != -1])
    terminal_nodes <- df$node_id[!df$node_id %in% child_nodes]

    if (!length(terminal_nodes)) {
      df$part <- "trunk"
      return(df)
    }

    term_rows <- df[df$node_id %in% terminal_nodes, , drop = FALSE]
    best_tip <- term_rows$node_id[which.max(term_rows$end_z)]

    trunk_nodes <- integer(0)
    current <- best_tip

    while (!is.na(current) && length(current) == 1L) {
      trunk_nodes <- c(trunk_nodes, current)
      this_row <- df[df$node_id == current, , drop = FALSE]
      if (!nrow(this_row)) break
      parent <- this_row$parent_id[1]
      if (is.na(parent) || parent == -1) break
      current <- parent
    }

    df$part[df$node_id %in% trunk_nodes] <- "trunk"
    df
  }

  out <- do.call(rbind, lapply(split_qsm, classify_one))
  rownames(out) <- NULL
  out
}
