#' TOne-hot Encoder
#'
#' @param sequence Sequence
#' @param max_length Maximum sequence length of the used training data
#'
#' @return Encoded sequence
#' @export
encode_one_hot <- function(sequence, max_length = 4034) {
  keys <- as.list(1:20)

  names(keys) <- c(
    "R", "K", "D", "E", "Q", "N", "H", "S", "T", "Y",
    "C", "W", "A", "I", "L", "M", "F", "V", "P", "G"
  )

  sequence <- sequence %>%
    toupper() %>%
    strsplit("") %>%
    unlist()

  encoded_sequence <- matrix(0, nrow = max_length, ncol = length(keys))

  count <- 1
  for (letter in sequence) {
    encoded_sequence[count, keys[[letter]]] <- 1
    count <- count + 1
  }

  encoded_sequence <- encoded_sequence %>%
    array(dim = c(1, max_length, length(keys)))

  return(encoded_sequence)
}
