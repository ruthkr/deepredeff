#' Get class activation value
#'
#' This function is for getting the class activation value, which indicates how important each location with the respect to the class under consideration.
#'
#' @param data Encoded sequence data as input of prediction.
#' @param model Model to use to predict.
#' @param layer Convnet layer to use.
#'
#' @return heatmap
#' @export
get_class_activation_value <- function(data, model, layer){

  # Get the output of the model
  data_output <- model$output[1]

  # Get particular layer
  conv_layer <- model %>% keras::get_layer(layer)

  # TGet the gradient with regard to the output feature map of "conv1d_1"
  grads <- keras::k_gradients(data_output, conv_layer$output)[[1]]

  # Get the mean intensity of the gradient over a specific feature map channel
  pooled_grads <- keras::k_mean(grads, axis = c(1, 2))

  # This function allows us to access the values of the quantities we just defined:
  # `pooled_grads` and the output feature map of `conv1d_1", given the sample input
  iterate <- keras::k_function(list(model$input),
                        list(pooled_grads, conv_layer$output[1,,]))

  # These are the values of these two quantities, as arrays, given data
  pooled_grads_value <- NULL
  conv_layer_output <- NULL
  c(pooled_grads_value, conv_layer_output) %<-% iterate(list(data))

  # We multiply each channel in the feature map arraywith regard to the elephant class
  for (i in 1:16) {
    conv_layer_output[,i] <-
      pooled_grads_value[[i]] * pooled_grads_value[[i]]
  }

  # The channel-wise mean of the resulting feature map is our heatmap of class activation
  heatmap <- apply(conv_layer_output, c(0,1), mean)

  # Normalising the heatmap
  heatmap <- pmax(heatmap, 0)
  heatmap <- heatmap / max(heatmap)

  return(heatmap)
}
