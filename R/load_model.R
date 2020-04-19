#' Load model in HDF5 format
#'
#' @param pathogen the name of pathogen, example: bacteria, fungi, and oomycete
#'
#' @return all of the hyperparamaters and parameters of particula model from specific chosen pathogen
#' @export
load_model <- function(pathogen = "all") {
  model_path <- system.file("extdata", "weights", paste0(pathogen, ".hdf5"), package = "effectorpred")
  model <- keras::load_model_hdf5(model_path)

  return(model)
}
