#' Ensemble weighted
#'
#' @param pred_list List of deep learning models.
#' @param weights Accuracy values from evaluating the validation dataset.
#'
#' @return Prediction.
#' @export
ensemble_weighted <- function(pred_list, weights) {
  ensemble_pred <- purrr::pmap(
    pred_list,
    .f = function(...) {
      res <- stats::weighted.mean(c(...), weights[names(c(...))])
    }
  )

  return(ensemble_pred)
}

#' Ensemble voting
#'
#' @param pred_list List of deep learning models.
#' @param weights Dummy weights variable.
#'
#' @return Binary prediction.
#' @export
ensemble_voting <- function(pred_list, weights = NULL) {
  round_bin <- function(x) {
    return(floor(x + 0.5))
  }

  ensemble_pred <- purrr::pmap(
    pred_list,
    .f = function(...) {
      res <- round_bin(mean(round_bin(c(...))))
    }
  )

  return(ensemble_pred)
}
