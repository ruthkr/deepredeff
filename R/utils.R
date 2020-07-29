tf <- NULL
heatmap <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  tf <<- reticulate::import("tensorflow", delay_load = TRUE)
  heatmap <<- reticulate::import_from_path(
    module = "calculate_heatmap",
    path = system.file("python", package = "deepredeff")
  )
}

#' Wildcard Expansion on File Paths
#'
#' @param ... Path
#' @param pattern Pattern
#'
#' @return Glob
package_glob <- function(..., pattern) {
  file_list <- Sys.glob(paste0(system.file(..., package = "deepredeff"), "/", pattern))
  return(file_list)
}
