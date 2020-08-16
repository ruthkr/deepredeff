test_that("Ensemble weighted works", {
  pred_list_length <- 10L
  ens_results <- deepredeff:::ensemble_weighted(
    pred_list = list(
      cnn_gru = as.list(runif(pred_list_length)),
      cnn_lstm = as.list(runif(pred_list_length)),
      gru_emb = as.list(runif(pred_list_length)),
      lstm_emb = as.list(runif(pred_list_length))
    ),
    weights = deepredeff:::bacteria_weights
  )

  expect_equal(
    length(ens_results),
    pred_list_length
  )

  expect_true(
    all(ens_results >= 0 & ens_results <= 1)
  )
})


test_that("Ensemble method selection works", {
  expect_equal(
    deepredeff:::get_ensemble_method("bacteria") %>% class(),
    "list"
  )
})
