#' Ensemble Weighted
#'
#' @param pred_list List of deep learning models.
#' @param weights Accuracy values from evaluating the validation dataset.
#'
#' @return Prediction
#' @export
ensemble_weighted <- function(pred_list, weights) {
  ensemble_pred <- purrr::pmap(
    pred_list,
    .f = function(...) {
      res <- stats::weighted.mean(c(...), weights)
    }
  )

  return(ensemble_pred)
}
