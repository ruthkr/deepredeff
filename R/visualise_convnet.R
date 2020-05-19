#' Get class activation value for each sequence
#'
#' This function is for getting the class activation value, which indicates how important each location with the respect to the class under consideration.
#'
#' @param data An encoded sequence as input of prediction.
#' @param loaded_model Model to use to predict.
#' @param layer Convnet layer to use.
#'
#' @return heatmap
#' @export
get_class_activation_each_seq <- function(data, loaded_model, layer) {

  # Get the output of the model
  data_output <- loaded_model$output[[1]]

  # Get particular layer
  conv_layer <- loaded_model %>% keras::get_layer(layer)

  # TGet the gradient with regard to the output feature map of "conv1d_1"
  grads <- keras::k_gradients(data_output, conv_layer$output)[[1]]

  # Get the mean intensity of the gradient over a specific feature map channel
  pooled_grads <- keras::k_mean(grads, axis = c(1, 2))

  # This function allows us to access the values of the quantities we just defined:
  # `pooled_grads` and the output feature map of `conv1d_1", given the sample input
  iterate <- keras::k_function(
    list(loaded_model$input),
    list(pooled_grads, conv_layer$output[1, , ])
  )

  # These are the values of these two quantities, as arrays, given data
  pooled_grads_value <- NULL
  conv_layer_output <- NULL
  c(pooled_grads_value, conv_layer_output) %<-% iterate(list(data))

  # We multiply each channel in the feature map arraywith regard to the elephant class
  for (i in 1:16) {
    conv_layer_output[, i] <-
      pooled_grads_value[[i]] * pooled_grads_value[[i]]
  }

  # The channel-wise mean of the resulting feature map is our heatmap of class activation
  heatmap <- apply(conv_layer_output, c(0, 1), mean)

  # Normalising the heatmap
  heatmap <- pmax(heatmap, 0)
  heatmap <- heatmap / max(heatmap)

  return(heatmap)
}


#' Load model that will be visualised
#'
#' @param model type model to visualise
#'
#' @return loaded model
#' @export
load_model_to_visualise <- function(model = c("all", "bacteria", "fungi", "oomycete")) {
  model <- match.arg(model)

  type <- dplyr::case_when(
    model %in% c("bacteria", "all") ~ "cnn_gru",
    model %in% c("oomycete", "fungi") ~ "cnn_lstm"
  )

  model_paths <- package_glob("extdata/weights", pattern = paste0(model, "_", type, ".hdf5"))

  loaded_model <- keras::load_model_hdf5(model_paths)

  return(loaded_model)
}

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
  tensorflow::tf$compat$v1$disable_eager_execution()

  # Load model
  loaded_model <- load_model_to_visualise(model)

  # Get the dim of the data
  max_length <- unlist(loaded_model$layers[[1]]$input_shape)[[1]]
  array_dim <- c(length(input), max_length, 20)

  # Get the encoded sequences in array
  sequence_array <- input %>%
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
    print(sum_heatmap %>% dim())
    print(heatmap_each_seq %>% dim())

    sum_heatmap <- sum_heatmap + heatmap_each_seq
  }

  return(sum_heatmap)
  # return(sequence_array)
}
