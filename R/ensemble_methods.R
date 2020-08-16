#' Weighted ensemble
#'
#' @param pred_list List of deep learning models.
#' @param weights Accuracy values from evaluation on the validation dataset.
#'
#' @return Returns the prediction results from weighted ensemble.
ensemble_weighted <- function(pred_list, weights) {
  ensemble_pred <- purrr::pmap(
    pred_list,
    .f = function(...) {
      res <- stats::weighted.mean(
        as.numeric(c(...)),
        as.numeric(weights[names(c(...))])
      )
    }
  )

  return(ensemble_pred)
}

#' Get ensemble methods
#'
#' This function is used to get the ensemble methods used for each taxon group. If weights are needed for a particular ensemble, then the weights will automatically follow.
#'
#' @param taxon taxon group
#'
#' @return Returns ensemble method and weights.
get_ensemble_method <- function(taxon) {
  if (taxon == "bacteria") {
    ensemble_method <- ensemble_weighted
    weights <- bacteria_weights
  } else {
    ensemble_method <- function(x, y) {
      return(x[[1]])
    }
    weights <- NULL
  }

  output_list <- list(
    ensemble_method = ensemble_method,
    weights = weights
  )

  return(output_list)
}
