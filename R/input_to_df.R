#' Convert fasta format to dataframe
#'
#' @param fasta_path Path of FASTA file.
#'
#' @return Returns data in data frame.
#' @export
#' @examples
#' input <- system.file("extdata/example/fungi_sample.fasta", package = "deepredeff")
#' input_df <- fasta_to_df(input)
#' @importFrom rlang .data
fasta_to_df <- function(fasta_path) {
  data_list <- seqinr::read.fasta(fasta_path)

  data <- data_list %>%
    purrr::map(
      .f = function(x) {
        data.frame(
          c(
            x %>%
              attributes() %>%
              unlist(),
            x %>%
              paste0(collapse = "") %>%
              toupper() %>%
              `names<-`("sequence")
          ) %>%
            dplyr::bind_rows()
        )
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows)

  # Drop the column name
  data <- data %>%
    dplyr::mutate(name = gsub(">", "", .data$Annot)) %>%
    dplyr::select(c(.data$name, .data$sequence))

  return(data)
}

#' Convert AAStringset class to dataframe
#'
#' @param aas AAStringset class object.
#'
#' @return Returns data in data frame.
#' @export
#'
#' @examples
#' input <- "MSHMTFNTWKAGLWRLAAAAVLSLLPVVARAAVPGITGPTFDLTAQPGRANQPDGASVYSWGYGCNPRTVPGFLPSVNPLAGQ"
#' input_aas <- Biostrings::AAString(input)
#' input_df <- aas_to_df(input_aas)
aasset_to_df <- function(aas) {
  data <- data.frame(
    name = names(aas),
    seq = as.character(aas),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  return(data)
}

#' Convert AAString class to dataframe
#'
#' @param aas AAString class object.
#'
#' @return Returns data in data frame.
#' @export
#'
#' @examples
#' input_fasta <- system.file("extdata/example/fungi_sample.fasta", package = "deepredeff")
#' input_aas <- Biostrings::readAAStringSet(input_fasta)
#' input_df <- aasset_to_df(input_aas)
aas_to_df <- function(aas) {
  aas <- Biostrings::AAStringSet(aas)
  data <- data.frame(
    name = as.character(aas),
    seq = as.character(aas),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  return(data)
}
