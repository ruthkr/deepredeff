#' Prediction mapper helper
#'
#' @param sequence_list List of sequences.
#' @param model_list List of models.
#'
#' @return List of predictions.
prediction_mapper <- function(sequence_list, model_list) {
  pred_list <- purrr::map(
    .x = model_list,
    .f = function(model) {
      purrr::map(
        .x = sequence_list,
        .f = function(sequence) {
          # Select encoding method
          if (model$layers[[1]] %>% stringr::str_detect("embedding")) {
            encode_method <- encode_integer
          } else {
            encode_method <- encode_one_hot
          }

          max_length <- unlist(model$layers[[1]]$input_shape)[[1]]

          # Make prediction
          pred <- encode_method(sequence, max_length) %>%
            stats::predict(model, .) %>%
            as.numeric()

          return(pred)
        }
      )
    }
  )

  return(pred_list)
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
    sequence_df <- fasta_to_df(x) %>%
      dplyr::select(.data$name, .data$sequence)
  } else {
    sequence_df <- x %>%
      dplyr::select({{ col_names }})
  }

  # Make list of sequences
  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Select ensemble method
  if (pathogen == "bacteria") {
    ensemble_method <- ensemble_weighted
  } else {
    ensemble_method <- function(x) { return(x[[1]]) }
  }

  # Load model
  model_list <- load_model(pathogen)

  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    sequence_df,
    prob = unlist(ensemble_method(pred_list))
  )

  return(preds)
}
