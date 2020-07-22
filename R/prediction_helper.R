#' Prediction mapper helper
#'
#' @param sequence_list List of sequences input.
#' @param model_list List of models.
#'
#' @return Returns list of prediction result of each sequence.
prediction_mapper <- function(sequence_list, model_list) {
  pred_list <- purrr::map(
    .x = model_list,
    .f = function(model) {
      # Max length
      max_length <- unlist(model$layers[[1]]$input_shape)[[1]]

      # Select encoding method
      if (any(grep("embedding", as.character(model$layers[[1]])))) {
        encode_method <- encode_integer
        array_dim <- c(length(sequence_list), max_length)
      } else {
        encode_method <- encode_one_hot
        array_dim <- c(length(sequence_list), max_length, 20)
      }

      sequence_array <- sequence_list %>%
        purrr::map(
          .f = function(x) {
            sequence <- encode_method(x, max_length)
            return(sequence)
          }
        ) %>%
        keras::array_reshape(dim = array_dim)

      # Make prediction
      pred <- stats::predict(model, sequence_array)

      return(pred)
    }
  )

  return(pred_list)
}
