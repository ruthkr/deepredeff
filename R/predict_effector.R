#' Predict Effector
#'
#' @param x FASTA path or dataframe
#' @param pathogen Pathogen
#' @param col_names Optional column names for dataframe. One of them must be named 'sequence'
#'
#' @return Dataframe with sequences and effector predictions
#' @export
predict_effector <- function(x, pathogen = "all", col_names) {
  # Check if input is FASTA or dataframe
  if (is.character(x)) {

    data_list <- seqinr::read.fasta(x)

    data_raw <-  data_list %>%
      purrr::map(
        .f = function(x) {
          data.frame(
            c(
              x %>% attributes() %>% unlist(),
              x %>%
                paste0(collapse = "") %>%
                stringr::str_to_upper() %>%
                `names<-`("sequence")
            ) %>%
              rbind()
          )
        }
      ) %>%
      purrr::reduce(dplyr::bind_rows)

    data <- data_raw %>%
      dplyr::select(name, sequence)

  } else {
    data <- x %>%
      dplyr::select({{ col_names }})
  }

  # Get the max length of encoding based on the pathogen model

  if (pathogen == "bacteria") {
    max_length <- 2574
  } else if (pathogen == "oomycete"){
    max_length <- 934
  } else if (pathogen == "fungi"){
    max_length <- 4034
  } else {
    max_length <- 4034
  }

  # Load model
  model <- load_model(pathogen)


  # Process sequences
  seq_list <- data %>%
    dplyr::pull(sequence) %>%
    as.list() %>%
    purrr::map(
      .f = function(x) {
        sequence <- encode_one_hot(x, max_length)
        return(sequence)
      }
    )

  # Make predictions
  pred_list <- seq_list %>%
    purrr::map(
      .f = function(x) {
        pred <- predict_single_effector(x, model) %>%
          as.numeric()
        return(pred)
      }
    )

  preds <- cbind(
      data,
      prob = unlist(pred_list)
    )

  return(preds)
}
