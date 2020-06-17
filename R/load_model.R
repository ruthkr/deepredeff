#' Load model in HDF5 format
#'
#' @param model Name of model.
#'
#' @return all of the hyperparamaters and parameters of particula model from specific chosen model.
#' @export
load_model <- function(model = c("all", "bacteria", "fungi", "oomycete")) {
  model <- match.arg(model)

  model_paths <- package_glob("extdata/weights", pattern = paste0(model, "*.hdf5"))

  # Model names for list
  model_names <- gsub(paste0(".*\\/", model, "_|.hdf5"), "", model_paths)

  # Clear Keras session graph
  keras::k_clear_session()

  # Load models
  model_list <- model_paths %>%
    purrr::map(
      keras::load_model_hdf5
    ) %>%
    `names<-`(model_names)

  message(paste0("Loaded models: ", paste(model_names, collapse = ", "), " for ", model, "."))

  return(model_list)
}
