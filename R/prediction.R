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
        do.call(rbind, .) %>%
        array(dim = array_dim)

      # Make prediction
      pred <- stats::predict(model, sequence_array) # %>%
      # as.numeric()

      return(pred)
    }
  )

  return(pred_list)
}
# prediction_mapper <- function(sequence_array, model_list) {
#   pred_list <- purrr::map(
#     .x = model_list,
#     .f = function(model) {
#       purrr::map(
#         .x = sequence_list,
#         .f = function(sequence) {
#           # Select encoding method
#           if (any(grep("embedding", as.character(model$layers[[1]])))) {
#             encode_method <- encode_integer
#           } else {
#             encode_method <- encode_one_hot
#           }
#
#           max_length <- unlist(model$layers[[1]]$input_shape)[[1]]
#
#           # Make prediction
#           pred <- encode_method(sequence, max_length) %>%
#             stats::predict(model, .) %>%
#             as.numeric()
#
#           return(pred)
#         }
#       )
#     }
#   )
#
#   return(pred_list)
# }


#' Predict effector
#'
#' @param input FASTA path or dataframe that contains input sequences.
#' @param model Selected model.
#' @param col_names Optional column names for dataframe. One of them must be named 'sequence'.
#'
#' @return Dataframe with sequences and effector predictions.
#' @export
#'
#' @importFrom rlang .data
predict_effector <- function(input, model = "all", col_names) {
  stopifnot(model %in% c("all", "bacteria", "fungi", "oomycete"))

  # Check the class of the input
  class_input <- class(input) %>% .[1]

  if (class_input == "character") {
    sequence_df <- fasta_to_df(input) %>%
      dplyr::select(.data$name, .data$sequence)
  } else if (class_input == "AAString" | "AAStringSet"){
    sequence_df <- aas_to_df(input)
  } else {
    sequence_df <- input %>%
      dplyr::select({{ col_names }})
  }

  # Make list of sequences
  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  # sequence_array <- sequence_list %>%
  #   as.list() %>%
  #   purrr::map(
  #     .f = function(x) {
  #       sequence <- deepredeff::encode_one_hot(x, 4034)
  #       return(sequence)
  #     }
  #   ) %>%
  #   do.call(rbind, .) %>%
  #   array(dim = c(length(sequence_list), 4034, 20))

  # Select ensemble method
  if (model == "bacteria") {
    ensemble_method <- ensemble_weighted
    weights <- bacteria_weights
  } else if (model == "all") {
    ensemble_method <- ensemble_voting
    weights <- bacteria_weights
  } else {
    ensemble_method <- function(x, y) {
      return(x[[1]])
    }
    weights <- NULL
  }

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
