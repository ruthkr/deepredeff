new_tbl_deepredeff <- function(x, description = names(x)) {
  if (is.null(description)) {
    description <- rep("", length(x))
  }
  names(x) <- description
  structure(x, class = c("tbl_deepredeff", "data.frame"), description = description)
}

#' Get the summary of the prediction results
#'
#' @param object Results of prediction from deepredeff::predict_effector().
#' @param cutoff Cutoff value to determine the prediction class (1 for effector and 0 for non-effector). The default value is 0.5.
#' @param ... Additional arguments ignored.
#'
#' @export
#' @importFrom rlang .data
summary.tbl_deepredeff <- function(object, cutoff = 0.5, ...) {
  object <- object %>%
    dplyr::mutate(pred_class = dplyr::case_when(
      .data$prob >= cutoff ~ 1,
      .data$prob < cutoff ~ 0
    ))

  count_per_class <- object %>%
    dplyr::group_by(.data$pred_class) %>%
    dplyr::summarise(count = dplyr::n())

  ratio_per_class <- object %>%
    dplyr::group_by(.data$pred_class) %>%
    dplyr::summarise(ratio = dplyr::n() / nrow(object))

  df_summary <- dplyr::left_join(count_per_class, ratio_per_class, by = "pred_class")

  return(df_summary)
}


#' @export
`[.tbl_deepredeff` <- function(x, i, ...) {
  new_x <- NextMethod("[")
  new_tbl_deepredeff(new_x)
}
