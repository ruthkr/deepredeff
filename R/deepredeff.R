#' \code{deepredeff} package
#'
#' Effector protein predictor using Deep Learning models.
#'
#' See the README on
#' \href{https://github.com/ruthkr/deepredeff/}{GitHub}
#'
#' @docType package
#' @name deepredeff
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "gtape", "conv_output"))
