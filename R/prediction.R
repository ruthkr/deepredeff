#' Predict effector
#'
#' @param input FASTA path or dataframe that contains input sequences.
#' @param model Selected model.
#'
#' @return Dataframe with sequences and effector predictions.
#' @export
#'
#' @importFrom rlang .data
predict_effector <- function(input, model) {
  UseMethod("predict_effector", input)
}

#' @rdname predict_effector
#' @export
predict_effector.character <- function(input, model) {
  if (tolower(tools::file_ext(input)) %in% c("fasta", "fna", "ffn", "faa", "frn")) {
    sequence_df <- fasta_to_df(input) %>%
      dplyr::select(.data$name, .data$sequence)
  } else {
    sequence_df <- input %>%
      as.data.frame() %>%
      `colnames<-`(c("sequence"))
  }

  # Make list of sequences
  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Select ensemble method and weights
  ensemble_method <- get_ensemble_method(model)[[1]]
  weights <- get_ensemble_method(model)[[2]]

  # Load model
  model_list <- load_model(model)
  message("Loaded models successfully!")

  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    sequence_df,
    prob = unlist(ensemble_method(pred_list, weights))
  )

  return(preds)
}


#' @rdname predict_effector
#' @export
predict_effector.data.frame <- function(input, model) {
  # Make list of sequences
  sequence_list <- input %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Select ensemble method and weights
  ensemble_method <- get_ensemble_method(model)[[1]]
  weights <- get_ensemble_method(model)[[2]]

  # Load model
  model_list <- load_model(model)
  message("Loaded models successfully!")

  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    input,
    prob = unlist(ensemble_method(pred_list, weights))
  )

  return(preds)
}


#' @rdname predict_effector
#' @export
predict_effector.AAStringset <- function(input, model) {
  # Make list of sequences
  sequence_df <- aas_to_df(input) %>%
    dplyr::rename(sequence = seq)

  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Select ensemble method and weights
  ensemble_method <- get_ensemble_method(model)[[1]]
  weights <- get_ensemble_method(model)[[2]]

  # Load model
  model_list <- load_model(model)
  message("Loaded models successfully!")

  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    sequence_df,
    prob = unlist(ensemble_method(pred_list, weights))
  )

  return(preds)
}


#' @rdname predict_effector
#' @export
predict_effector.default <- function(input, model) {
  warning(paste("deepredeff does not know how to handle input", class(input), "and currecntly can only be used on fasta, AAStringset, and dataframe input"))
}
