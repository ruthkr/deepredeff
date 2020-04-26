#' Predict on a single sequence
#'
#' @param sequence Sequence.
#' @param model Trained model.
#'
#' @return Prediction.
#' @export
predict_single_effector <- function(sequence, model) {

  result <- stats::predict(model, sequence)

  return(result)
}


#' Predict effector
#'
#' @param x FASTA path or dataframe.
#' @param pathogen Pathogen.
#' @param col_names Optional column names for dataframe. One of them must be named 'sequence'.
#'
#' @return Dataframe with sequences and effector predictions.
#' @export
#'
#' @importFrom rlang .data
predict_effector <- function(x, pathogen = "all", col_names) {
  stopifnot(pathogen %in% c("all", "bacteria", "fungi", "oomycete"))

  # Check if input is FASTA or dataframe
  if (is.character(x)) {
    data <- fasta_to_df(x) %>%
      dplyr::select(.data$name, .data$sequence)
  } else {
    data <- x %>%
      dplyr::select({{ col_names }})
  }

  # Load model
  model <- load_model(pathogen)
  max_length <- model$layers[[1]]$input_shape[[1]][[2]]

  # Process sequences
  # seq_list <- data %>%
  #   dplyr::pull(sequence) %>%
  #   as.list() %>%
  #   purrr::map(
  #     .f = function(x) {
  #       sequence <- encode_one_hot(x, max_length)
  #       return(sequence)
  #     }
  #   )

  # Make predictions
  pred_list <- data %>%
    dplyr::pull(sequence) %>%
    as.list() %>%
    purrr::map(
      .f = function(x) {
        pred <- encode_integer(x, max_length) %>%
          predict_single_effector(model) %>%
          as.numeric()
        return(pred)
      }
    )

  preds <- cbind(
    data,
    prob = unlist(pred_list)
  )

  return(preds)
}
