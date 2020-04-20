#' Predict Effector
#'
#' @param x FASTA path or dataframe
#' @param pathogen Pathogen
#' @param col_names Optional column names for dataframe. One of them must be named 'sequence'
#'
#' @return Dataframe with sequences and effector predictions
#' @export
predict_effector <- function(x, pathogen = "all", col_names) {
  # Check if input is FASTA or dataframe
  if (is.character(x)) {
    message("Work in progress")
    data <- data.frame()
  } else {
    data <- x %>%
      dplyr::select({{ col_names }})
  }

  # Load model
  model <- load_model(pathogen)

  # Process sequences
  seq_list <- data %>%
    dplyr::pull(sequence) %>%
    as.list() %>%
    purrr::map(encode_one_hot)

  # Make predictions
  pred_list <- seq_list %>%
    purrr::map(
      .f = function(x) {
        pred <- predict_single_effector(x, model) %>%
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
