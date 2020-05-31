test_that("One-hot encoding works", {
  expect_equal(
    "RKDEQNHSTYCWAILMFVPG" %>%
      deepredeff::encode_one_hot(max_length = 20) %>%
      diag(),
    rep(1, 20)
  )
})

test_that("One-hot encoding returns 2D matrix", {
  expect_equal(
    sample(LETTERS, 50, replace = TRUE) %>%
      paste0(collapse = "") %>%
      deepredeff::encode_one_hot(max_length = 100) %>%
      dim() %>%
      length(),
    2
  )
})

test_that("One-hot encoding returns no missing values", {
  expect_equal(
    sample(LETTERS, 50, replace = TRUE) %>%
      paste0(collapse = "") %>%
      deepredeff::encode_one_hot(max_length = 100) %>%
      is.na() %>%
      any(),
    FALSE
  )
})

test_that("One-hot encoding returns numeric values", {
  expect_equal(
    sample(LETTERS, 50, replace = TRUE) %>%
      paste0(collapse = "") %>%
      deepredeff::encode_one_hot(max_length = 100) %>%
      is.numeric(),
    TRUE
  )
})

test_that("Integer encoding works", {
  expect_equal(
    "ACDEFGHIKLMNPQRSTVWYXBUJZO" %>%
      deepredeff::encode_integer(max_length = 26),
    matrix(seq(1, 26, 1), nrow = 1)
  )
})

test_that("Integer encoding returns 2D matrix", {
  expect_equal(
    sample(LETTERS, 50, replace = TRUE) %>%
      paste0(collapse = "") %>%
      deepredeff::encode_integer(max_length = 100) %>%
      dim() %>%
      length(),
    2
  )
})

test_that("Integer encoding returns no missing values", {
  expect_equal(
    sample(LETTERS, 50, replace = TRUE) %>%
      paste0(collapse = "") %>%
      deepredeff::encode_integer(max_length = 100) %>%
      is.na() %>%
      any(),
    FALSE
  )
})

test_that("Integer encoding returns numeric values", {
  expect_equal(
    sample(LETTERS, 50, replace = TRUE) %>%
      paste0(collapse = "") %>%
      deepredeff::encode_integer(max_length = 100) %>%
      is.numeric(),
    TRUE
  )
})
