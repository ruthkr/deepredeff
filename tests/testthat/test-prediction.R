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
  # skip_if_no_tf()
  skip("skip 1")
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

test_that("Detection of non-aminoacid sequence works", {
  # skip_if_no_tf()
  skip("skip 2")
  expect_error(
    deepredeff::predict_effector(
      input = "VERYWRONGSEQUENCE123+",
      model = "fungi"
    )
  )
})

test_that("Detection of valid input class works", {
  # skip_if_no_tf()
  skip("skip 3")
  expect_warning(
    deepredeff::predict_effector(
      input = NULL,
      model = "fungi"
    )
  )
})

test_that("Prediction with input data frame returns S3 class", {
  # skip_if_no_tf()
  skip("skip 4")
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        fasta_to_df(),
      model = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input string returns S3 class", {
  # skip_if_no_tf()
  skip("skip 5")
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        fasta_to_df() %>%
        dplyr::slice(1) %>%
        dplyr::pull(sequence),
      model = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input FASTA returns S3 class", {
  # skip_if_no_tf()
  skip("skip 6")
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff"),
      model = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input AAStringset returns S3 class", {
  # skip_if_no_tf()
  skip("skip 7")
  expect_s3_class(
    deepredeff::predict_effector(
      input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
        Biostrings::readAAStringSet(),
      model = "fungi"
    ),
    "tbl_deepredeff"
  )
})

test_that("Prediction with input AAString returns S3 class", {
  # skip_if_no_tf()
  skip("skip 8")
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

test_that("Summary of prediction result return data frame", {
  # skip_if_no_tf()
  skip("skip 9")
  class_summary <- deepredeff::predict_effector(
    input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      dplyr::slice(1) %>%
      dplyr::pull(sequence),
    model = "fungi"
  ) %>%
    summary() %>%
    class()

  expect_equal(class_summary[1], "tbl_df")
  expect_equal(class_summary[2], "tbl")
  expect_equal(class_summary[3], "data.frame")
})


test_that("Plot of prediction result return gg/ggplot object", {
  # skip_if_no_tf()
  skip("skip 10")
  class_plot <- deepredeff::predict_effector(
    input = system.file("extdata/example/fungi_sample.fasta", package = "deepredeff") %>%
      fasta_to_df() %>%
      dplyr::slice(1) %>%
      dplyr::pull(sequence),
    model = "fungi"
  ) %>%
    ggplot2::autoplot() %>%
    class()

  expect_equal(class_plot[1], "gg")
  expect_equal(class_plot[2], "ggplot")
})
