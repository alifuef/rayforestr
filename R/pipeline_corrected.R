# Corrected-tree pipeline -------------------------------------------------

ray_pipeline_corrected <- function(input_file,
                                   ...) {
  res <- ray_pipeline_single_tree(input_file = input_file, ...)
  res$step <- "ray_pipeline_corrected"
  res
}
