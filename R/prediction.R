#' Predict effector
#'
#' `predict_effector` is used to predict effector protein given amino acid protein sequences.
#'
#' @param input Input data that contains amino acid sequence(s). It can be in fasta format, strings, AAString, AAStringset, and dataframe.
#' @param taxon Taxon group of input data. Available taxons are bacteria, fungi, and oomycete.
#'
#' @return `predict_effector` returns an object of class "tbl_deepredeff" or for multiple responses of class c("tbl_deepredeff", "data.frame").
#'
#' An object of class "tbl_deepredeff" is a data frame containing at least the following components:
#'
#' \item{`sequence`}{the sequence(s) from the input data.}
#' \item{`s_score`}{score obtained from sigmoid function showing how likely the sequences to be an effector.}
#' \item{`prediction`}{class prediction for each sequence, obtained from s_score. If the value of `s_score` >= 0.5, it will be classified as an effector. Otherwise, it will be classified as a non-effector.}
#'
#' @export
#'
#' @examples
#' \donttest{
#' # FASTA input
#' input_fasta <- system.file("extdata/example/fungi_sample.fasta", package = "deepredeff")
#'
#' pred_result <- deepredeff::predict_effector(
#'   input = input_fasta,
#'   taxon = "fungi"
#' )
#' }
#'
#' @importFrom rlang .data
predict_effector <- function(input, taxon) {
  UseMethod("predict_effector", input)
}

#' @rdname predict_effector
#' @export
predict_effector.character <- function(input, taxon) {
  if (all(tolower(tools::file_ext(input)) %in% c("fasta", "fna", "ffn", "faa", "frn", "txt", "csv"))) {
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
  ensemble_method <- get_ensemble_method(taxon)[[1]]
  weights <- get_ensemble_method(taxon)[[2]]

  # Load model
  model_list <- load_model(taxon)
  message("Loaded models successfully!")

  model_names <- names(model_list)

  # Model names for list
  if (taxon == "bacteria") {
    model_names <- "ensemble_weighted"
  }

  message(paste0("Model used ", "for taxon ", taxon, ": ", paste(model_names, collapse = ", "), "."))


  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    sequence_df,
    s_score = unlist(ensemble_method(pred_list, weights))
  )

  preds <- preds %>%
    dplyr::mutate(
      prediction = factor(
        .data$s_score >= 0.5,
        levels = c(TRUE, FALSE),
        labels = c("effector", "non-effector")
      )
    )

  attr(preds, "model_names") <- model_names
  attr(preds, "taxon") <- taxon

  return(new_tbl_deepredeff(preds))
}


#' @rdname predict_effector
#' @export
predict_effector.data.frame <- function(input, taxon) {
  # Make list of sequences
  sequence_list <- input %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Select ensemble method and weights
  ensemble_method <- get_ensemble_method(taxon)[[1]]
  weights <- get_ensemble_method(taxon)[[2]]

  # Load model
  model_list <- load_model(taxon)
  message("Loaded models successfully!")

  model_names <- names(model_list)

  # Model names for list
  if (taxon == "bacteria") {
    model_names <- "ensemble_weighted"
  }

  message(paste0("Model used ", "for taxon ", taxon, ": ", paste(model_names, collapse = ", "), "."))

  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    input,
    s_score = unlist(ensemble_method(pred_list, weights))
  )

  preds <- preds %>%
    dplyr::mutate(
      prediction = factor(
        .data$s_score >= 0.5,
        levels = c(TRUE, FALSE),
        labels = c("effector", "non-effector")
      )
    )

  attr(preds, "model_names") <- model_names
  attr(preds, "taxon") <- taxon

  return(new_tbl_deepredeff(preds))
}


#' @rdname predict_effector
#' @export
predict_effector.AAStringSet <- function(input, taxon) {
  # Make list of sequences
  sequence_df <- aasset_to_df(input) %>%
    dplyr::rename(sequence = seq)

  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Select ensemble method and weights
  ensemble_method <- get_ensemble_method(taxon)[[1]]
  weights <- get_ensemble_method(taxon)[[2]]

  # Load model
  model_list <- load_model(taxon)
  message("Loaded models successfully!")

  model_names <- names(model_list)

  # Model names for list
  if (taxon == "bacteria") {
    model_names <- "ensemble_weighted"
  }

  message(paste0("Model used ", "for taxon ", taxon, ": ", paste(model_names, collapse = ", "), "."))

  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    sequence_df,
    s_score = unlist(ensemble_method(pred_list, weights))
  )

  preds <- preds %>%
    dplyr::mutate(
      prediction = factor(
        .data$s_score >= 0.5,
        levels = c(TRUE, FALSE),
        labels = c("effector", "non-effector")
      )
    )

  attr(preds, "model_names") <- model_names
  attr(preds, "taxon") <- taxon

  return(new_tbl_deepredeff(preds))
}


#' @rdname predict_effector
#' @export
predict_effector.AAString <- function(input, taxon) {
  # Make list of sequences
  sequence_df <- aas_to_df(input) %>%
    dplyr::rename(sequence = seq)

  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Select ensemble method and weights
  ensemble_method <- get_ensemble_method(taxon)[[1]]
  weights <- get_ensemble_method(taxon)[[2]]

  # Load model
  model_list <- load_model(taxon)
  message("Loaded models successfully!")

  model_names <- names(model_list)

  # Model names for list
  if (taxon == "bacteria") {
    model_names <- "ensemble_weighted"
  }

  message(paste0("Model used ", "for taxon ", taxon, ": ", paste(model_names, collapse = ", "), "."))

  # Make predictions
  pred_list <- prediction_mapper(sequence_list, model_list)

  preds <- dplyr::bind_cols(
    sequence_df,
    s_score = unlist(ensemble_method(pred_list, weights))
  )

  preds <- preds %>%
    dplyr::mutate(
      prediction = factor(
        .data$s_score >= 0.5,
        levels = c(TRUE, FALSE),
        labels = c("effector", "non-effector")
      )
    )

  attr(preds, "model_names") <- model_names
  attr(preds, "taxon") <- taxon

  return(new_tbl_deepredeff(preds))
}


#' @rdname predict_effector
#' @export
predict_effector.default <- function(input, taxon) {
  warning(paste("deepredeff does not know how to handle input", class(input), "and currently can only be used on fasta, AAStringset, and dataframe input"))
}
