# Treefile -> QSM-like table ---------------------------------------------
#' Convert parsed RayExtract treefile/treeinfo data to a QSM-like table
#'
#' @param x A data.frame returned by ray_read_treefile() or ray_read_treeinfo(),
#'   or a path to a treefile / info file.
#'
#' @return A data.frame with start/end coordinates, geometric segment length,
#'   and direction vectors.
#' @export
ray_treefile_to_qsm <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    if (grepl("_info\\.txt$", basename(x), perl = TRUE)) {
      x <- ray_read_treeinfo(x, drop_root = FALSE)
    } else {
      x <- ray_read_treefile(x)
    }
  }

  if (!is.data.frame(x)) {
    stop("x must be a data.frame or a path to treefile/info file", call. = FALSE)
  }

  if (!nrow(x)) {
    return(x)
  }

  req <- c("tree_id", "node_id", "parent_id", "x", "y", "z")
  miss <- req[!req %in% names(x)]
  if (length(miss)) {
    stop("Missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  }

  qsm_one_tree <- function(df) {
    df <- df[order(df$node_id), , drop = FALSE]
    rownames(df) <- NULL

    lookup <- df[, c("node_id", "x", "y", "z"), drop = FALSE]

    seg <- df[df$parent_id != -1, , drop = FALSE]
    rownames(seg) <- NULL

    if (!nrow(seg)) {
      return(seg)
    }

    if ("length" %in% names(seg)) {
      seg$reported_length <- seg$length
    } else if ("segment_length" %in% names(seg)) {
      seg$reported_length <- seg$segment_length
    } else {
      seg$reported_length <- NA_real_
    }

    seg$start_x <- NA_real_
    seg$start_y <- NA_real_
    seg$start_z <- NA_real_

    seg$end_x <- seg$x
    seg$end_y <- seg$y
    seg$end_z <- seg$z

    for (i in seq_len(nrow(seg))) {
      pid <- seg$parent_id[i]
      parent_row <- lookup[lookup$node_id == pid, , drop = FALSE]

      if (nrow(parent_row) == 1L) {
        seg$start_x[i] <- parent_row$x
        seg$start_y[i] <- parent_row$y
        seg$start_z[i] <- parent_row$z
      }
    }

    dx_raw <- seg$end_x - seg$start_x
    dy_raw <- seg$end_y - seg$start_y
    dz_raw <- seg$end_z - seg$start_z

    seg$segment_length <- sqrt(dx_raw^2 + dy_raw^2 + dz_raw^2)

    mag <- seg$segment_length
    seg$dx <- ifelse(is.na(mag) | mag == 0, 0, dx_raw / mag)
    seg$dy <- ifelse(is.na(mag) | mag == 0, 0, dy_raw / mag)
    seg$dz <- ifelse(is.na(mag) | mag == 0, 0, dz_raw / mag)

    if (!"diameter" %in% names(seg) && "radius" %in% names(seg)) {
      seg$diameter <- seg$radius * 2
    }

    seg
  }

  out <- do.call(rbind, lapply(split(x, x$tree_id), qsm_one_tree))
  rownames(out) <- NULL
  out
}
