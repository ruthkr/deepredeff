#' Load model in HDF5 format
#'
#' @param pathogen Name of pathogen.
#'
#' @return all of the hyperparamaters and parameters of particula model from specific chosen pathogen.
#' @export
load_model <- function(pathogen = c("all", "bacteria", "fungi", "oomycete")) {
  pathogen <- match.arg(pathogen)

  model_paths <- Sys.glob(paste0(system.file("extdata", "weights", package = "deepredeff"), "/", pathogen, "*.hdf5"))

  # Model names for list
  model_names <- gsub(paste0(".*\\/", pathogen, "_|.hdf5"), "", model_paths)

  # Clear Keras session graph
  keras::k_clear_session()

  # Load models
  model_list <- model_paths %>%
    purrr::map(
      keras::load_model_hdf5
    ) %>%
    `names<-`(model_names)

  message(paste0("Loaded models: ", paste(model_names, collapse = ", "), " for pathogen ", pathogen, "."))

  return(model_list)
}
