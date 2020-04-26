#' One-hot Encoder
#'
#' @param sequence Sequence
#' @param max_length Maximum sequence length used on the training data
#'
#' @return One-hot encoded sequence
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

#' Integer Encoder
#'
#' @param sequence Sequence
#' @param max_length Maximum sequence length used on the training data
#'
#' @return Integer encoded sequence
#' @export
encode_integer <- function(sequence, max_length = 4034) {
  # Define the list of the letters
  keys <- as.list(1:26)

  names(keys) <- c(
    "A", "C", "D", "E", "F", "G", "H", "I",
    "K", "L", "M", "N", "P", "Q", "R", "S",
    "T", "V", "W", "Y", "X", "B", "U", "J",
    "Z", "O"
  )

  # Split the amino acid letter in the sequence
  sequence <- sequence %>%
    toupper() %>%
    strsplit("") %>%
    unlist()

  # Initialize the array of zeros
  encoded_sequence <- array(0, c(1, max_length))

  index <- 1
  for (amino_acid in sequence) {
    encoded_sequence[, index] <- keys[[amino_acid]]
    index <- index + 1
  }

  return(encoded_sequence)
}
