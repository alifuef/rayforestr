# Core result helpers -----------------------------------------------------

# Core result helpers -----------------------------------------------------

#' Allowed `ray_result` statuses
#'
#' @return A character vector of valid status values.
#' @export
ray_result_statuses <- function() {
  c(
    "success",
    "dry_run",
    "skipped_existing",
    "failed",
    "existing_input",
    "external_input",
    "not_requested"
  )
}

new_ray_result <- function(step,
                           input = NULL,
                           outputs = list(),
                           command = NULL,
                           status = ray_result_statuses(),
                           log_file = NULL,
                           details = list()) {
  status <- match.arg(status)
  structure(
    list(
      step = step,
      input = input,
      outputs = outputs,
      command = command,
      status = status,
      log_file = log_file,
      details = details
    ),
    class = "ray_result"
  )
}



#' Print a `ray_result`
#'
#' @param x A `ray_result` object.
#' @param ... Unused.
#'
#' @return The input object, invisibly.
#' @export
print.ray_result <- function(x, ...) {
  cat("<ray_result>\n")
  cat("  step:   ", x$step, "\n", sep = "")
  cat("  status: ", x$status, "\n", sep = "")
  if (!is.null(x$input)) {
    cat("  input:  ", paste(x$input, collapse = ", "), "\n", sep = "")
  }
  if (length(x$outputs)) {
    cat("  outputs:\n")
    for (nm in names(x$outputs)) {
      cat("    - ", nm, ": ", paste(x$outputs[[nm]], collapse = ", "), "\n", sep = "")
    }
  }
  invisible(x)
}



#' Summarize a `ray_result`
#'
#' @param object A `ray_result` object.
#' @param ... Unused.
#'
#' @return A small summary list.
#' @export
summary.ray_result <- function(object, ...) {
  list(
    step = object$step,
    status = object$status,
    n_outputs = length(object$outputs),
    outputs = object$outputs,
    log_file = object$log_file
  )
}
