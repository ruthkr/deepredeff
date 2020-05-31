.onLoad <- function(libname, pkgname) {
  reticulate::use_condaenv("tensorflow", conda = "/usr/local/Caskroom/miniconda/base/bin/conda")
  # reticulate::configure_environment(pkgname)
}

#' Title
#'
#' @param ... Path
#' @param pattern Pattern
#'
#' @return Glob
package_glob <- function(..., pattern) {
  file_list <- Sys.glob(paste0(system.file(..., package = "deepredeff"), "/", pattern))
  return(file_list)
}
