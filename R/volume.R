# Volume helpers ----------------------------------------------------------

ray_compute_volume <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    x <- ray_treefile_to_qsm(x)
  }
  if (!is.data.frame(x)) stop("x must be a data.frame or a treefile/info path", call. = FALSE)
  if (!nrow(x)) return(data.frame(tree_id = integer(), volume = numeric()))
  if (!all(c("tree_id", "radius", "segment_length") %in% names(x))) {
    x <- ray_treefile_to_qsm(x)
  }
  if (!all(c("tree_id", "radius", "segment_length") %in% names(x))) {
    stop("Missing required columns after QSM conversion: tree_id, radius, segment_length", call. = FALSE)
  }

  x$segment_volume <- pi * x$segment_length * (x$radius^2)
  out <- stats::aggregate(segment_volume ~ tree_id, x, sum, na.rm = TRUE)
  names(out)[2] <- "volume"
  out
}

ray_compute_volume_dir <- function(input_dir, pattern = "\\.txt$") {
  files <- list.files(input_dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) return(data.frame())
  out <- lapply(files, function(f) {
    vol <- ray_compute_volume(f)
    data.frame(filename = basename(f), vol, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}


