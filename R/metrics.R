# Patch for rayforestr metrics layer
# Replace your existing merchantable/metrics functions with these.

ray_merchantable_volume <- function(
  qsm,
  merch_min_diam = 0.07,
  tree_id_col = "tree_id",
  part_col = "part"
) {
  if (!is.data.frame(qsm)) {
    stop("qsm must be a data.frame", call. = FALSE)
  }

  req <- c(tree_id_col, "radius", "segment_length")
  miss <- req[!req %in% names(qsm)]
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  x <- qsm

  if (!part_col %in% names(x)) {
    if (exists("ray_classify_stem", mode = "function")) {
      x <- ray_classify_stem(x, tree_id_col = tree_id_col)
    } else {
      x[[part_col]] <- "trunk"
    }
  }

  if (!"diameter" %in% names(x)) {
    x$diameter <- x$radius * 2
  }

  x$cyl_volume <- pi * (x$radius^2) * x$segment_length

  keep <- x[[part_col]] == "trunk" & x$diameter >= merch_min_diam
  y <- x[keep, , drop = FALSE]

  ids <- data.frame(tree_id = unique(x[[tree_id_col]]))

  if (!nrow(y)) {
    out <- ids
    out$merchantable_volume <- 0
    rownames(out) <- NULL
    return(out)
  }

  out <- stats::aggregate(
    y$cyl_volume,
    by = list(tree_id = y[[tree_id_col]]),
    FUN = sum,
    na.rm = TRUE
  )
  names(out)[2] <- "merchantable_volume"

  out <- merge(ids, out, by = "tree_id", all.x = TRUE)
  out$merchantable_volume[is.na(out$merchantable_volume)] <- 0
  out <- out[order(out$tree_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

ray_tree_metrics <- function(
  qsm,
  info = NULL,
  density_trunk = 600,
  density_branch = 600,
  branch_factor = 1,
  merch_min_diam = 0.07,
  tree_id_col = "tree_id",
  part_col = "part"
) {
  if (!is.data.frame(qsm)) {
    stop("qsm must be a data.frame", call. = FALSE)
  }

  req <- c(
    tree_id_col, "radius", "segment_length",
    "start_x", "start_y", "start_z",
    "end_x", "end_y", "end_z"
  )
  miss <- req[!req %in% names(qsm)]
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  x <- qsm

  if (!part_col %in% names(x)) {
    if (exists("ray_classify_stem", mode = "function")) {
      x <- ray_classify_stem(x, tree_id_col = tree_id_col)
    } else {
      x[[part_col]] <- "trunk"
    }
  }

  if (!"diameter" %in% names(x)) {
    x$diameter <- x$radius * 2
  }

  x$cyl_volume <- pi * (x$radius^2) * x$segment_length

  ids <- data.frame(tree_id = unique(x[[tree_id_col]]))

  total_vol <- stats::aggregate(
    x$cyl_volume,
    by = list(tree_id = x[[tree_id_col]]),
    FUN = sum,
    na.rm = TRUE
  )
  names(total_vol)[2] <- "total_volume"

  trunk_idx <- x[[part_col]] == "trunk"
  if (any(trunk_idx, na.rm = TRUE)) {
    trunk_vol <- stats::aggregate(
      x$cyl_volume[trunk_idx],
      by = list(tree_id = x[[tree_id_col]][trunk_idx]),
      FUN = sum,
      na.rm = TRUE
    )
    names(trunk_vol)[2] <- "trunk_volume"
  } else {
    trunk_vol <- ids
    trunk_vol$trunk_volume <- 0
  }

  branch_idx <- x[[part_col]] == "branch"
  if (any(branch_idx, na.rm = TRUE)) {
    branch_vol <- stats::aggregate(
      x$cyl_volume[branch_idx],
      by = list(tree_id = x[[tree_id_col]][branch_idx]),
      FUN = sum,
      na.rm = TRUE
    )
    names(branch_vol)[2] <- "branch_volume"
  } else {
    branch_vol <- ids
    branch_vol$branch_volume <- 0
  }

  vol_df <- Reduce(
    function(a, b) merge(a, b, by = "tree_id", all = TRUE),
    list(ids, total_vol, trunk_vol, branch_vol)
  )

  vol_df$total_volume[is.na(vol_df$total_volume)] <- 0
  vol_df$trunk_volume[is.na(vol_df$trunk_volume)] <- 0
  vol_df$branch_volume[is.na(vol_df$branch_volume)] <- 0

  merch_df <- ray_merchantable_volume(
    qsm = x,
    merch_min_diam = merch_min_diam,
    tree_id_col = tree_id_col,
    part_col = part_col
  )

  cog_df <- ray_compute_cog(
    qsm = x,
    density_trunk = density_trunk,
    density_branch = density_branch,
    branch_factor = branch_factor,
    tree_id_col = tree_id_col,
    part_col = part_col,
    shift_to_base = TRUE
  )

  out <- Reduce(
    function(a, b) merge(a, b, by = "tree_id", all = TRUE),
    list(vol_df, merch_df, cog_df)
  )

  if (!is.null(info)) {
    info_df <- info
    if (is.character(info) && length(info) == 1L) {
      info_df <- ray_read_treeinfo(info)
    }

    if (!is.data.frame(info_df)) {
      stop("info must be a data.frame or path to an info file", call. = FALSE)
    }

    if (exists("ray_treeinfo_summary", mode = "function")) {
      info_sum <- ray_treeinfo_summary(info_df)
    } else {
      info_sum <- info_df
    }

    cols <- c("tree_id", "height", "crown_radius", "dbh", "DBH", "x", "y", "z", "volume")
    cols <- intersect(cols, names(info_sum))
    info_sum <- info_sum[, cols, drop = FALSE]

    if ("dbh" %in% names(info_sum) && !"DBH" %in% names(info_sum)) {
      info_sum$DBH <- info_sum$dbh
    }

    if ("crown_radius" %in% names(info_sum)) {
      info_sum$crown_projection_area <- pi * info_sum$crown_radius^2
    }

    if ("x" %in% names(info_sum)) names(info_sum)[names(info_sum) == "x"] <- "info_x"
    if ("y" %in% names(info_sum)) names(info_sum)[names(info_sum) == "y"] <- "info_y"
    if ("z" %in% names(info_sum)) names(info_sum)[names(info_sum) == "z"] <- "info_z"
    if ("volume" %in% names(info_sum)) names(info_sum)[names(info_sum) == "volume"] <- "info_volume"

    out <- merge(out, info_sum, by = "tree_id", all.x = TRUE)
  } else {
    geom_h <- stats::aggregate(
      x$end_z,
      by = list(tree_id = x[[tree_id_col]]),
      FUN = max,
      na.rm = TRUE
    )
    names(geom_h)[2] <- "top_z"
    out <- merge(out, geom_h, by = "tree_id", all.x = TRUE)
    out$height <- out$top_z - out$base_z
    out$top_z <- NULL
  }

  out <- out[order(out$tree_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}
