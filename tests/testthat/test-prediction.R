# helper function to skip tests if we don't have the 'tensorflow' module
skip_if_no_tf <- function() {
  have_tf <- reticulate::py_module_available("tensorflow")

  if (!have_tf) {
    skip(paste0(
      "TensorFlow is not available for testing",
      " (using Python from ",
      reticulate::py_config()$python,
      ")"
    ))
  }
}

test_that("Prediction function works", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_equal(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff"),
      taxon = "fungi"
    ) %>%
      class() %>%
      .[[1]],
    "tbl_deepredeff"
  )
})

test_that("Detection of non-aminoacid sequence works", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_error(
    deepredeff::predict_effector(
      input = "VERYWRONGSEQUENCE123+",
      taxon = "fungi"
    )
  )
})

test_that("Detection of valid input class works", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_warning(
    deepredeff::predict_effector(
      input = NULL,
      taxon = "fungi"
    )
  )
})

test_that("Prediction with input data frame returns S3 class", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        fasta_to_df(),
      taxon = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input string returns S3 class", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        fasta_to_df() %>%
        dplyr::slice(1) %>%
        dplyr::pull(sequence),
      taxon = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input FASTA returns S3 class", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff"),
      taxon = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input AAStringset returns S3 class", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        Biostrings::readAAStringSet(),
      taxon = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input AAString returns S3 class", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        fasta_to_df() %>%
        dplyr::slice(1) %>%
        dplyr::pull(sequence) %>%
        Biostrings::AAString(),
      taxon = "fungi"
    ),
    "tbl_deepredeff"
  )
})



test_that("Summary of prediction result works", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  pred <- deepredeff::predict_effector(
    input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      dplyr::slice(1) %>%
      dplyr::pull(sequence),
    taxon = "fungi"
  )

  summary_print <- capture_output(print(summary(pred)))

  expect_equal(summary_print, "Total sequences in input data: 1\n---\nTaxon chosen: fungi\nModel type used: cnn_lstm\n\nTotal sequences predicted as effector: 1\nTotal sequences predicted as non-effector: 0")
})


test_that("Plot of prediction result return gg/ggplot object", {
  testthat::skip_on_cran()
  skip_if_no_tf()
  class_plot <- deepredeff::predict_effector(
    input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      dplyr::slice(1) %>%
      dplyr::pull(sequence),
    taxon = "fungi"
  ) %>%
    plot() %>%
    class()

  expect_equal(class_plot[1], "gg")
  expect_equal(class_plot[2], "ggplot")
})
