#' Load model
#'
#' `load_model()` loads model saved in hdf5 format
#'
#' @param taxon Name of taxon.
#'
#' @return Returns all of the hyperparamaters and parameters of particular model from specific taxon chosen.
load_model <- function(taxon = c("bacteria", "fungi", "oomycete")) {
  taxon <- match.arg(taxon)

  model_paths <- package_glob("extdata/weights", pattern = paste0(taxon, "*.hdf5"))

  model_names <- gsub(paste0(".*\\/", taxon, "_|.hdf5"), "", model_paths)

  # Clear Keras session graph
  keras::k_clear_session()

  # Load models
  model_list <- model_paths %>%
    purrr::map(
      keras::load_model_hdf5
    ) %>%
    `names<-`(model_names)

  return(model_list)
}
