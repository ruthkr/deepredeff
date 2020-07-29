#' Load model based on the taxon options
#'
#' @param taxon type taxon depends on the data
#'
#' @return loaded model
#' @export
load_model_to_visualise <- function(taxon = c("bacteria", "fungi", "oomycete")) {
  taxon <- match.arg(taxon)

  type <- dplyr::case_when(
    taxon %in% c("bacteria") ~ "cnn_gru",
    taxon %in% c("oomycete", "fungi") ~ "cnn_lstm"
  )

  model_paths <- package_glob("extdata/weights", pattern = paste0(taxon, "_", type, ".hdf5"))

  loaded_model <- keras::load_model_hdf5(model_paths)

  return(loaded_model)
}

#' Visualise convnet
#'
#' @param input_data amino acid protein sequence input
#' @param taxon taxon of the data
#'
#' @return smoothed value of convnet visualisation
#' @export
get_heatmap_value <- function(input_data, taxon){

  # Load model to visualise
  model <- load_model_to_visualise(taxon)

  # Encode the data
  encoded_data <- heatmap$get_encoding(input_data, max_length = 4034L)

  sample_len <- as.integer(dim(encoded_data)[1])

  heatmap_all <- heatmap$get_append_heatmap(
    data_name = encoded_data,
    model = model,
    layer = "conv1d_1",
    from_sample = 1L,
    sample_length = sample_len
  )

  final_heatmap <- as.numeric(heatmap$get_sum_heatmap(heatmap_all))

  # smooth_heatmap <- heatmap$get_smooth(final_heatmap, 50L)

  return(final_heatmap)

}


#' Get the smooth function using fourier transformation
#'
#' @param x Input data.
#' @param n Number of harmonics.
#'
#' @return Smooth input using FFT.
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
