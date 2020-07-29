#' Visualise Convnet
#'
#' `visualise_convnet` is used to visualise convolutional network filters while predicting effector proteins given amino acid protein sequences.
#'
#' @param input Input data that contains amino acid sequence(s). It can be in fasta format, strings, AAString, AAStringset, and dataframe.
#' @param taxon Taxon group of input data. Available taxons are bacteria, fungi, and oomycete.
#'
#'
#' @return `visualise_convnet` returns an object of class "heatmap_deepredeff" or for multiple responses of class c("heatmap_deepredeff", "array").
#' @export
visualise_convnet <- function(input, taxon){
  UseMethod("visualise_convnet", input)
}


#' @rdname visualise_convnet
#' @export
visualise_convnet.character <- function(input, taxon) {
  if (all(tolower(tools::file_ext(input)) %in% c("fasta", "fna", "ffn", "faa", "frn", "txt", "csv"))) {
    sequence_df <- fasta_to_df(input)

    # Make list of sequences
    sequence_list <- sequence_df %>%
      dplyr::pull(sequence) %>%
      as.list()

  } else {
    stopifnot(grepl("^[A-Za-z]+$", input, perl = T))

    sequence_list <- input
  }

  heatmap_value <- get_heatmap_value(sequence_list, taxon)

  return(new_heatmap_deepredeff(heatmap_value))
}


#' @rdname visualise_convnet
#' @export
visualise_convnet.data.frame <- function(input, taxon) {
  # Make list of sequences
  sequence_list <- input %>%
    dplyr::pull(sequence) %>%
    as.list()

  heatmap_value <- get_heatmap_value(sequence_list, taxon)

  return(new_heatmap_deepredeff(heatmap_value))
}


#' @rdname visualise_convnet
#' @export
visualise_convnet.AAStringSet <- function(input, taxon) {
  # Make list of sequences
  sequence_df <- aasset_to_df(input) %>%
    dplyr::rename(sequence = seq)

  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  heatmap_value <- get_heatmap_value(sequence_list, taxon)

  return(new_heatmap_deepredeff(heatmap_value))
}

#' @rdname visualise_convnet
#' @export
visualise_convnet.AAString <- function(input, taxon) {
  # Make list of sequences
  sequence_df <- aas_to_df(input) %>%
    dplyr::rename(sequence = seq)

  sequence_list <- sequence_df %>%
    dplyr::pull(sequence) %>%
    as.list()

  heatmap_value <- get_heatmap_value(sequence_list, taxon)

  return(new_heatmap_deepredeff(heatmap_value))
}

#' @rdname visualise_convnet
#' @export
visualise_convnet.default <- function(input, taxon) {
  warning(paste("deepredeff does not know how to handle input", class(input), "and currently can only be used on fasta, AAStringset, and dataframe input"))
}

