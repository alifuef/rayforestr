.onAttach <- function(libname, pkgname) {
  packageStartupMessage("rayforestr loaded. Use ray_check_docker() before running pipelines.")
}
