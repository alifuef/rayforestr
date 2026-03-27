# Setup helpers -----------------------------------------------------------

# Image helpers -----------------------------------------------------------

#' Default RayCloudTools image
#'
#' Returns the package default Docker image used for RayCloudTools commands.
#'
#' @return A length-1 character vector with a Docker image tag.
#' @export
ray_default_raycloud_image <- function() {
  getOption("rayforestr.raycloud_image", "ghcr.io/csiro-robotics/raycloudtools:latest")
}



#' Default TreeTools image
#'
#' Returns the TreeTools image configured for the package. When no dedicated
#' TreeTools image is set, this falls back to the RayCloudTools image.
#'
#' @param raycloud_image Optional RayCloudTools image to use as fallback.
#'
#' @return A length-1 character vector with a Docker image tag.
#' @export
ray_default_treetools_image <- function(raycloud_image = NULL) {
  img <- getOption("rayforestr.treetools_image", NULL)
  if (!is.null(img) && nzchar(img)) return(img)
  if (!is.null(raycloud_image) && nzchar(raycloud_image)) return(raycloud_image)
  ray_default_raycloud_image()
}



#' Resolve Docker images used by the package
#'
#' Combines explicit arguments and package options into the final image tags
#' used for RayCloudTools and TreeTools steps.
#'
#' @param raycloud_image Optional Docker image for RayCloudTools commands.
#' @param treetools_image Optional Docker image for TreeTools commands.
#'
#' @return A named list with elements `raycloud_image` and `treetools_image`.
#' @export
ray_get_images <- function(raycloud_image = NULL,
                           treetools_image = NULL) {
  rc <- if (!is.null(raycloud_image) && nzchar(raycloud_image)) raycloud_image else ray_default_raycloud_image()
  tt <- if (!is.null(treetools_image) && nzchar(treetools_image)) treetools_image else ray_default_treetools_image(rc)
  list(raycloud_image = rc, treetools_image = tt)
}



#' Set package Docker image options
#'
#' @param raycloud_image Docker image for RayCloudTools commands.
#' @param treetools_image Optional Docker image for TreeTools commands.
#'
#' @return Invisibly returns the resolved image list.
#' @export
ray_set_images <- function(raycloud_image,
                           treetools_image = NULL) {
  imgs <- ray_get_images(raycloud_image = raycloud_image, treetools_image = treetools_image)
  options(rayforestr.raycloud_image = imgs$raycloud_image)
  options(rayforestr.treetools_image = imgs$treetools_image)
  invisible(imgs)
}



#' Pull a Docker image
#'
#' @param image Docker image tag to pull.
#' @param echo Logical; print command output.
#'
#' @return Invisibly returns Docker command output.
#' @export
ray_pull_image <- function(image = ray_default_raycloud_image(),
                           echo = interactive()) {
  out <- tryCatch(system2("docker", c("pull", image), stdout = TRUE, stderr = TRUE), error = function(e) e)
  if (inherits(out, "error")) stop(conditionMessage(out), call. = FALSE)
  if (echo && length(out)) cat(paste(out, collapse = "\n"), "\n")
  invisible(out)
}



#' Build a local TreeTools image
#'
#' @param context_dir Docker build context directory.
#' @param image Image tag to build.
#' @param echo Logical; print command output.
#'
#' @return Invisibly returns Docker build output.
#' @export
ray_build_treetools <- function(context_dir,
                                image = ray_default_treetools_image(),
                                echo = interactive()) {
  context_dir <- ray_norm_path(context_dir, must_work = TRUE)
  out <- tryCatch(
    system2("docker", c("build", "-t", image, context_dir), stdout = TRUE, stderr = TRUE),
    error = function(e) e
  )
  if (inherits(out, "error")) stop(conditionMessage(out), call. = FALSE)
  if (echo && length(out)) cat(paste(out, collapse = "\n"), "\n")
  invisible(out)
}



#' Check whether a Docker image exposes required tools
#'
#' @param image Docker image tag.
#' @param tools Character vector of command names to check.
#' @param echo Logical; show command output while checking.
#'
#' @return A named list of logical values.
#' @export
ray_image_has_tools <- function(image = ray_default_raycloud_image(),
                                tools = c("rayimport", "rayextract", "raysplit", "treesmooth", "treemesh", "treeinfo"),
                                echo = FALSE) {
  checks <- vapply(tools, function(tool) {
    cmd <- sprintf("command -v %s >/dev/null 2>&1", tool)
    status <- suppressWarnings(system2(
      "docker",
      c("run", "--rm", image, "bash", "-lc", cmd),
      stdout = if (isTRUE(echo)) "" else FALSE,
      stderr = if (isTRUE(echo)) "" else FALSE
    ))
    identical(status, 0L)
  }, logical(1))
  setNames(as.list(checks), tools)
}
