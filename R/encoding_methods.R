#' One-hot encoder
#'
#' `encode_one_hot` one-hot-encodes sequence in a string format.
#'
#' @param sequence Sequence in a string format.
#' @param max_length Maximum length of sequence to encode.
#'
#' @return One-hot encoded sequence.
#' @export
#'
#' @examples
#'sample_seq <- "MSHMTFNTWKAGLWRLAAAAVLSLLPVVARAAVPGITGPTFDLTAQPGRANQPDGASVYSWGYGCNPRTVPGFLPSVNPLAGQ"
#'encoded_seq <- encode_one_hot(sample_seq)
encode_one_hot <- function(sequence, max_length = 4034) {
  keys <- as.list(1:20)

  names(keys) <- c(
    "R", "K", "D", "E", "Q", "N", "H", "S", "T", "Y",
    "C", "W", "A", "I", "L", "M", "F", "V", "P", "G"
  )

  # Split the amino acid letter in the sequence
  sequence <- sequence %>%
    # Cut or pad sequence
    substr(1, max_length) %>%
    sprintf(paste0("%-", max_length, "s"), .) %>%
    # Capitalize and split
    toupper() %>%
    strsplit("") %>%
    unlist()

  encoded_sequence <- matrix(
    0,
    nrow = max_length,
    ncol = length(keys)
  )

  count <- 1
  for (letter in sequence) {
    encoded_sequence[count, keys[[letter]]] <- 1
    count <- count + 1
  }

  return(encoded_sequence)
}

#' Integer encoder
#'
#' `encode_integer` integer-encodes sequence in a string format.
#'
#' @param sequence Sequence in a string format.
#' @param max_length Maximum length of sequence to encode.
#'
#' @return Integer encoded sequence.
#' @export
#'
#' @examples
#'sample_seq <- "MSHMTFNTWKAGLWRLAAAAVLSLLPVVARAAVPGITGPTFDLTAQPGRANQPDGASVYSWGYGCNPRTVPGFLPSVNPLAGQ"
#'encoded_seq <- encode_integer(sample_seq)
encode_integer <- function(sequence, max_length = 4034) {
  # Define the list of the letters
  keys <- as.list(0:26)

  names(keys) <- c(
    " ", "A", "C", "D", "E", "F", "G", "H", "I",
    "K", "L", "M", "N", "P", "Q", "R", "S",
    "T", "V", "W", "Y", "X", "B", "U", "J",
    "Z", "O"
  )

  # Split the amino acid letter in the sequence
  sequence <- sequence %>%
    # Capitalize
    toupper() %>%
    # Remove non-letter chars
    gsub("[^A-Z]", "", .) %>%
    # Cut or pad sequence
    substr(1, max_length) %>%
    sprintf(paste0("%-", max_length, "s"), .) %>%
    # Split
    strsplit("") %>%
    unlist()

  # Initialize the array of zeros
  encoded_sequence <- matrix(
    0,
    nrow = 1,
    ncol = max_length
  )

  index <- 1
  for (amino_acid in sequence) {
    encoded_sequence[1, index] <- keys[[amino_acid]]
    index <- index + 1
  }

  return(encoded_sequence)
}
