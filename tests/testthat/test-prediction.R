# helper function to skip tests if we don't have the 'tensorflow' module
skip_if_no_tf <- function() {
  have_tf <- reticulate::py_module_available("tensorflow")

  if (!have_tf) {
    skip(paste0(
      "TensorFlow is not available for testing",
      " (using Python from ",
      reticulate::conda_binary(),
      ")"
    ))
  }
}

test_that("Prediction function works", {
  skip_if_no_tf()
  expect_equal(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff"),
      model = "fungi"
    ) %>%
      class() %>%
      .[[1]],
    "tbl_deepredeff"
  )
})

test_that("Prediction with input FASTA return S3 class", {
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff"),
      model = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input AAStringset return S3 class", {
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        Biostrings::readAAStringSet(),
      model = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input AAString return S3 class", {
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        fasta_to_df() %>%
        dplyr::slice(1) %>%
        dplyr::pull(sequence) %>%
        Biostrings::AAString(),
      model = "fungi"
    ),
    "tbl_deepredeff"
  )
})


