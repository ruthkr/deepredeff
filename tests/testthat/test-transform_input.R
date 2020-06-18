test_that("FASTA conversion to data frame works", {
  expect_equal(
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      class(),
    "data.frame"
  )
  expect_equal(
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      dplyr::pull(sequence) %>%
      class(),
    "character"
  )
})

test_that("AAStringset conversion to data frame works", {
  expect_equal(
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      Biostrings::readAAStringSet() %>%
      aasset_to_df() %>%
      class(),
    "data.frame"
  )
  expect_equal(
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      Biostrings::readAAStringSet() %>%
      aasset_to_df() %>%
      dplyr::pull(seq) %>%
      class(),
    "character"
  )
})

test_that("AAString conversion to data frame works", {
  expect_equal(
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      dplyr::slice(1) %>%
      dplyr::pull(sequence) %>%
      Biostrings::AAString() %>%
      aas_to_df() %>%
      class(),
    "data.frame"
  )
  expect_equal(
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      dplyr::slice(1) %>%
      dplyr::pull(sequence) %>%
      Biostrings::AAString() %>%
      aas_to_df() %>%
      dplyr::pull(seq) %>%
      class(),
    "character"
  )
})

test_that("FASTA and AAStringset conversion are identical", {
  expect_equal(
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      .[["sequence"]],
    system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      Biostrings::readAAStringSet() %>%
      aasset_to_df() %>%
      .[["seq"]]
  )
})
