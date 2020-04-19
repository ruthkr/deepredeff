#' Predict on a single sequence
#'
#' @param sequence Sequence
#' @param model Trained model
#'
#' @return Prediction
#' @export
predict_single_effector <- function(sequence, model) {

  result <- stats::predict(model, sequence)

  return(result)
}
