#' Load model in HDF5 format
#'
#' @param taxon Name of taxon.
#'
#' @return all of the hyperparamaters and parameters of particular model from specific chosen taxon.
#' @export
load_model <- function(taxon = c("all", "bacteria", "fungi", "oomycete")) {
  taxon <- match.arg(taxon)

  model_paths <- package_glob("extdata/weights", pattern = paste0(taxon, "*.hdf5"))

  model_names <- gsub(paste0(".*\\/", taxon, "_|.hdf5"), "", model_paths)

  # Model names for list
  # if (taxon == "bacteria"){
  #   model_names_print <- "ensemble_weighted"
  # } else if (taxon == "all"){
  #   model_names_print <- "ensemble_voting"
  # } else {
  #   model_names_print <- model_names
  # }

  # Clear Keras session graph
  keras::k_clear_session()

  # Load models
  model_list <- model_paths %>%
    purrr::map(
      keras::load_model_hdf5
    ) %>%
    `names<-`(model_names)

  # message(paste0("Model used ", "for taxon ", taxon, ": ", paste(model_names_print, collapse = ", "), "."))

  return(model_list)
}
