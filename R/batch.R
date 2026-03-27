# Batch helpers -----------------------------------------------------------

ray_make_jobs <- function(input_files,
                          pipeline = c("forest", "single_tree", "corrected"),
                          out_root = NULL) {
  pipeline <- match.arg(pipeline)
  data.frame(
    input_file = ray_to_posix(normalizePath(input_files, winslash = "/", mustWork = FALSE)),
    pipeline = pipeline,
    out_root = if (is.null(out_root)) NA_character_ else out_root,
    stringsAsFactors = FALSE
  )
}

ray_batch <- function(jobs,
                      stop_on_error = FALSE,
                      echo = interactive(),
                      ...) {
  if (!is.data.frame(jobs) || !all(c("input_file", "pipeline") %in% names(jobs))) {
    stop("jobs must be a data.frame with at least input_file and pipeline columns", call. = FALSE)
  }

  res <- vector("list", nrow(jobs))
  for (i in seq_len(nrow(jobs))) {
    job <- jobs[i, , drop = FALSE]
    fn <- switch(job$pipeline,
      forest = ray_pipeline_forest,
      single_tree = ray_pipeline_single_tree,
      corrected = ray_pipeline_corrected,
      stop("Unknown pipeline: ", job$pipeline, call. = FALSE)
    )

    out_root <- if ("out_root" %in% names(job) && !is.na(job$out_root)) job$out_root else NULL
    try_res <- try(fn(input_file = job$input_file, out_root = out_root, echo = echo, ...), silent = TRUE)
    if (inherits(try_res, "try-error")) {
      if (stop_on_error) stop(attr(try_res, "condition")$message, call. = FALSE)
      res[[i]] <- list(error = as.character(try_res), input_file = job$input_file, pipeline = job$pipeline)
    } else {
      res[[i]] <- try_res
    }
  }
  res
}

ray_resume <- function(batch_results, ...) {
  failed <- vapply(batch_results, function(x) is.list(x) && !inherits(x, "ray_result") && !is.null(x$error), logical(1))
  if (!any(failed)) return(list())
  jobs <- data.frame(
    input_file = vapply(batch_results[failed], `[[`, character(1), "input_file"),
    pipeline = vapply(batch_results[failed], `[[`, character(1), "pipeline"),
    stringsAsFactors = FALSE
  )
  ray_batch(jobs, ...)
}

ray_collect_logs <- function(batch_results) {
  logs <- lapply(batch_results, function(x) {
    if (inherits(x, "ray_result")) {
      data.frame(input_file = x$input, step = x$step, status = x$status, log_file = x$log_file, stringsAsFactors = FALSE)
    } else {
      data.frame(input_file = x$input_file, step = x$pipeline, status = "failed", log_file = NA_character_, stringsAsFactors = FALSE)
    }
  })
  do.call(rbind, logs)
}
