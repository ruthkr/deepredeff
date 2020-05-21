#' Predict effector
#'
#' @param input Input data that contains amino acid sequence(s). It can be in fasta format, strings, AAString, AAStringset, and dataframe.
#' @param model Model to use in predicting the input sequence. Available models are bacteria, fungi, oomycete, and all.
#'
#' @return Deepredeff objects that contains sequences and effector predictions.
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
    sequence_df <- fasta_to_df(input)
  } else {
    stopifnot(grepl("^[A-Za-z]+$", input, perl = T))

    sequence_df <- input %>%
      toupper() %>%
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

  return(new_tbl_deepredeff(preds))
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

  return(new_tbl_deepredeff(preds))
}


#' @rdname predict_effector
#' @export
predict_effector.AAStringSet <- function(input, model) {
  # Make list of sequences
  sequence_df <- aasset_to_df(input) %>%
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

  return(new_tbl_deepredeff(preds))
}


#' @rdname predict_effector
#' @export
predict_effector.AAString <- function(input, model) {
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

  return(new_tbl_deepredeff(preds))
}


#' @rdname predict_effector
#' @export
predict_effector.default <- function(input, model) {
  warning(paste("deepredeff does not know how to handle input", class(input), "and currently can only be used on fasta, AAStringset, and dataframe input"))
}
