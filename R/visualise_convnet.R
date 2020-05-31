#' Get sum activation class
#'
#' This function will calculate sum of all each class activation value in a set of sequence data.
#'
#' @param input Input sequence data.
#' @param model Model to use.
#'
#' @return Sum of class activation.
#' @export
get_sum_activation <- function(input, model) {

  # Load model
  loaded_model <- load_model_to_visualise(model)

  # Get the dim of the data
  max_length <- unlist(loaded_model$layers[[1]]$input_shape)[[1]]
  array_dim <- c(length(input), max_length, 20)

  # Make list of sequences
  sequence_list <- input %>%
    dplyr::pull(sequence) %>%
    as.list()

  # Get the encoded sequences in array
  sequence_array <- sequence_list %>%
    purrr::map(
      .f = function(x) {
        sequence <- encode_one_hot(x, max_length)
        return(sequence)
      }
    ) %>%
    keras::array_reshape(dim = array_dim)

  sum_heatmap <- array(0, dim = max_length)

  for (i in 1:dim(sequence_array)[1]) {
    seq_each <- array(sequence_array[i, , ], dim = c(1, max_length, 20))
    print(seq_each %>% dim())

    heatmap_each_seq <- get_class_activation_each_seq(
      data = seq_each,
      loaded_model = loaded_model,
      layer = "conv1d_1"
    )

    sum_heatmap <- sum_heatmap + heatmap_each_seq
  }

  sum_heatmap <- as.numeric(sum_heatmap / max(sum_heatmap))

  return(new_heatmap_deepredeff(sum_heatmap))
}
