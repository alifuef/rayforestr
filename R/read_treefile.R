# Treefile parsing --------------------------------------------------------

ray_parse_header <- function(header_line) {
  header_line <- trimws(header_line)

  # Native treeinfo format uses ", " between tree-level and segment-level groups.
  if (grepl(",\\s+", header_line, perl = TRUE)) {
    parts <- strsplit(header_line, ",\\s+", perl = TRUE)[[1]]
    tree_part <- trimws(parts[1])
    seg_part  <- trimws(paste(parts[-1], collapse = ", "))
    tree_headers <- trimws(strsplit(tree_part, ",", fixed = TRUE)[[1]])
    seg_headers  <- trimws(strsplit(seg_part, ",", fixed = TRUE)[[1]])
    tree_headers <- tree_headers[nzchar(tree_headers)]
    seg_headers  <- seg_headers[nzchar(seg_headers)]
  } else {
    tree_headers <- character()
    seg_headers  <- trimws(strsplit(header_line, ",", fixed = TRUE)[[1]])
    seg_headers  <- seg_headers[nzchar(seg_headers)]
  }

  list(
    raw = header_line,
    tree_headers = tree_headers,
    seg_headers = seg_headers,
    all_headers = c(tree_headers, seg_headers),
    has_tree_attrs = length(tree_headers) > 0L
  )
}

ray_parse_tree_line <- function(data_line, header_info, tree_id = 1L) {
  chunks <- strsplit(trimws(data_line), " +")[[1]]
  chunks <- trimws(sub(",$", "", chunks))
  chunks <- chunks[nzchar(chunks)]

  tree_vals <- numeric(0)
  if (header_info$has_tree_attrs) {
    if (!length(chunks)) stop("Malformed tree line: missing tree-level values.", call. = FALSE)
    tree_vals <- suppressWarnings(as.numeric(trimws(strsplit(chunks[1], ",", fixed = TRUE)[[1]])))
    chunks <- chunks[-1]
  }

  if (!length(chunks)) {
    return(data.frame())
  }

  parsed <- lapply(chunks, function(ch) {
    vals <- trimws(strsplit(ch, ",", fixed = TRUE)[[1]])
    vals <- vals[nzchar(vals)]
    suppressWarnings(as.numeric(vals))
  })

  max_len <- max(vapply(parsed, length, integer(1)), 0L)
  parsed <- lapply(parsed, function(x) {
    length(x) <- max_len
    x
  })

  seg <- as.data.frame(do.call(rbind, parsed), stringsAsFactors = FALSE)
  if (ncol(seg) > 0L) {
    nm <- header_info$seg_headers[seq_len(min(length(header_info$seg_headers), ncol(seg)))]
    names(seg)[seq_along(nm)] <- nm
  }

  if (header_info$has_tree_attrs) {
    tree_df <- as.list(rep(NA_real_, length(header_info$tree_headers)))
    names(tree_df) <- header_info$tree_headers
    if (length(tree_vals)) {
      for (i in seq_len(min(length(tree_vals), length(header_info$tree_headers)))) {
        tree_df[[i]] <- tree_vals[i]
      }
    }
    for (nm in names(tree_df)) seg[[nm]] <- tree_df[[nm]]
  }

  seg$tree_id <- tree_id
  seg$segment_index <- seq_len(nrow(seg))
  seg$node_id <- seg$segment_index - 1L
  seg$is_root <- if ("parent_id" %in% names(seg)) seg$parent_id == -1 else seg$node_id == 0L

  # Reorder useful columns to front
  front <- c("tree_id", "segment_index", "node_id", "is_root", header_info$tree_headers)
  front <- unique(front[front %in% names(seg)])
  seg <- seg[, c(front, setdiff(names(seg), front)), drop = FALSE]
  seg
}

ray_read_treefile <- function(file) {
  lines <- readLines(file, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < 3L) stop("File must contain at least 3 non-empty lines.", call. = FALSE)

  header_info <- ray_parse_header(lines[2])
  data_lines <- lines[-c(1, 2)]

  parsed <- lapply(seq_along(data_lines), function(i) {
    ray_parse_tree_line(data_lines[i], header_info = header_info, tree_id = i)
  })

  parsed <- parsed[vapply(parsed, nrow, integer(1)) > 0L]
  if (!length(parsed)) return(data.frame())

  out <- do.call(rbind, parsed)
  rownames(out) <- NULL

  attr(out, "comment") <- lines[1]
  attr(out, "header") <- header_info$all_headers
  attr(out, "tree_headers") <- header_info$tree_headers
  attr(out, "seg_headers") <- header_info$seg_headers
  attr(out, "source_file") <- file
  out
}

ray_read_treefile_single <- function(file) {
  df <- ray_read_treefile(file)
  if (!nrow(df)) stop("Parsed treefile contains no rows.", call. = FALSE)
  if (length(unique(df$tree_id)) != 1L) {
    stop("Expected a single-tree file but found multiple tree_id values.", call. = FALSE)
  }
  df
}

ray_make_tree_dataframe <- function(treefile_dir,
                                    id_mode = c("auto", "numeric_suffix", "filename")) {
  id_mode <- match.arg(id_mode)
  files <- list.files(treefile_dir, pattern = "\\.txt$", full.names = TRUE)
  if (!length(files)) return(data.frame())

  get_id <- function(path) {
    stem <- ray_stem_no_ext(path)
    if (id_mode == "filename") return(stem)
    m <- regexpr("_([0-9]+)$", stem, perl = TRUE)
    if (id_mode == "numeric_suffix" && m[1] < 0) stop("No numeric suffix found for: ", basename(path), call. = FALSE)
    if (m[1] > 0) return(sub("^.*_([0-9]+)$", "\\1", stem, perl = TRUE))
    stem
  }

  pick_base_row <- function(df) {
    if ("is_root" %in% names(df) && any(df$is_root, na.rm = TRUE)) {
      df[which(df$is_root)[1], , drop = FALSE]
    } else {
      df[1, , drop = FALSE]
    }
  }

  out <- lapply(files, function(f) {
    df <- ray_read_treefile_single(f)
    base <- pick_base_row(df)
    data.frame(
      filename = basename(f),
      id = get_id(f),
      x = if ("x" %in% names(base)) base$x[1] else NA_real_,
      y = if ("y" %in% names(base)) base$y[1] else NA_real_,
      d = if ("radius" %in% names(base)) base$radius[1] * 2 else NA_real_,
      selection = "undecided",
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out)
  if (id_mode == "numeric_suffix") {
    out <- out[order(as.numeric(out$id)), , drop = FALSE]
  } else {
    out <- out[order(out$filename), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
