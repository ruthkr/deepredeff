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


