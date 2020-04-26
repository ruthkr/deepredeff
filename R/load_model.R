#' Load model in HDF5 format
#'
#' @param pathogen Name of pathogen.
#'
#' @return all of the hyperparamaters and parameters of particula model from specific chosen pathogen.
#' @export
load_model <- function(pathogen = c("all", "bacteria", "fungi", "oomycete")) {
  pathogen <- match.arg(pathogen)

  model_paths <- Sys.glob(paste0(system.file("extdata", "weights", package = "effectorpred"), "/", pathogen, "*.hdf5"))

  model_names <- model_paths %>%
    stringr::str_split("/") %>%
    unlist() %>%
    grep(".hdf5", x = ., value = TRUE) %>%
    stringr::str_remove_all(paste0(pathogen, "_|.hdf5"))

  model_list <- model_paths %>%
    purrr::map(
      keras::load_model_hdf5
    ) %>%
    `names<-`(model_names)

  return(model_list)
}
