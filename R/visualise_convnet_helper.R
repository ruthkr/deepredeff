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

  # Get particular layer
  conv_layer <- loaded_model %>% keras::get_layer(layer)

  # Define the heatmap model
  heatmap_model <- tensorflow::tf$keras$models$Model(list(loaded_model$input), list(conv_layer$output, loaded_model$output))

  with(tensorflow::tf$GradientTape() %as% gtape, {
    c(conv_output, predictions) %<-% heatmap_model(data)
    loss <- predictions[,1]
  })

  # Get the gradient with regard to the output feature map of "conv1d_1"
  grads <- gtape$gradient(loss, conv_output)

  pooled_grads <- keras::k_mean(grads, axis = c(1, 2))

  heatmap <- tensorflow::tf$reduce_mean(tensorflow::tf$multiply(pooled_grads, conv_output), axis = -1L)

  heatmap <- pmax(as.numeric(heatmap), 0)
  heatmap <- as.numeric(heatmap / max(heatmap))

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



#' Get the smooth function using fourier transformation
#'
#' @param x the input data
#' @param n the number of harmonics
#'
#' @return
#' @export
get_fft_smooth <- function(x = NULL, n = NULL) {
  # The direct transformation
  dff <- stats::fft(x)

  # Select harmonic
  dff[seq(n + 2, length(x) - n, 1)] <- 0

  # The inverses
  indff <- stats::fft(dff / length(x), inverse = TRUE)
  x_smooth <- Mod(indff)

  return(x_smooth)
}
