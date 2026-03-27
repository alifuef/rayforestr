# treeinfo parsing --------------------------------------------------------

ray_normalize_treeinfo_names <- function(df) {
  nm <- names(df)
  nm <- sub("^DBH$", "dbh", nm)
  nm <- sub("^BranchOrder$", "branch_order", nm)
  nm <- sub("^PositionInBranch$", "pos_in_branch", nm)
  nm <- sub("^Length$", "length", nm)
  names(df) <- nm

  if ("length" %in% names(df) && !"segment_length" %in% names(df)) {
    df$segment_length <- df$length
  }
  if ("radius" %in% names(df) && !"diameter" %in% names(df)) {
    df$diameter <- df$radius * 2
  }
  df
}

ray_read_treeinfo <- function(file, drop_root = FALSE) {
  df <- ray_read_treefile(file)
  if (!nrow(df)) return(df)
  df <- ray_normalize_treeinfo_names(df)

  attr(df, "source_file") <- file

  if (isTRUE(drop_root) && "parent_id" %in% names(df)) {
    df <- df[df$parent_id != -1, , drop = FALSE]
    rownames(df) <- NULL
  }
  df
}

ray_treeinfo_summary <- function(x) {
  if (is.character(x) && length(x) == 1L) x <- ray_read_treeinfo(x, drop_root = FALSE)
  if (!is.data.frame(x)) stop("x must be a data.frame or a path to *_info.txt", call. = FALSE)
  if (!nrow(x)) return(x)

  pick_root <- function(df) {
    if ("parent_id" %in% names(df) && any(df$parent_id == -1, na.rm = TRUE)) {
      df[which(df$parent_id == -1)[1], , drop = FALSE]
    } else {
      df[1, , drop = FALSE]
    }
  }

  out <- do.call(rbind, lapply(split(x, x$tree_id), pick_root))
  rownames(out) <- NULL

  if ("crown_radius" %in% names(out)) {
    out$crown_projection_area <- pi * out$crown_radius^2
  }
  out
}
