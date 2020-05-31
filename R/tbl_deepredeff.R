new_tbl_deepredeff <- function(x, description = names(x)) {
  if (is.null(description)) {
    description <- rep("", length(x))
  }
  names(x) <- description
  structure(x, class = c("tbl_deepredeff", "data.frame"), description = description)
}

#' @export
`[.tbl_deepredeff` <- function(x, i, ...) {
  new_x <- NextMethod("[")
  new_tbl_deepredeff(new_x)
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






#' Plot the results of prediction
#'
#' @param object Results of prediction from deepredeff::predict_effector().
#' @param cutoff Cutoff value to determine the prediction class (1 for effector and 0 for non-effector). The default value is 0.5.
#' @param type Type plot to generate. Type class will generate histogram of each prediction class, while prob will show each sequence prediction value and the cutoff value. Default type is class.
#' @param ... Additional arguments ignored.
#'
#' @return Plot depending on the chosen type.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
autoplot.tbl_deepredeff <- function(object, cutoff = 0.5, type = "class", ...) {

  object <- object %>%
    dplyr::mutate(
      pred_class = dplyr::case_when(
        .data$prob >= cutoff ~ 1,
        .data$prob < cutoff ~ 0
      ) %>%
        as.factor(),
        name = substr(.data$name, 1, 20)
    )

  switch(
    type,
    "class" = gg_class(object, cutoff),
    "prob" = gg_prob(object, cutoff)
  )
}

#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
gg_class <- function(object, cutoff) {
  `%+%` <- ggplot2::`%+%`

  object %>%
    ggplot2::ggplot() %+%
    ggplot2::aes(.data$pred_class) %+%
    ggplot2::geom_bar() %+%
    ggplot2::labs(x = "Prediction class")
}

#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
gg_prob <- function(object, cutoff) {
  `%+%` <- ggplot2::`%+%`

  object %>%
    ggplot2::ggplot() %+%
    ggplot2::aes(
      x = .data$name,
      y = .data$prob,
      ymin = 0,
      ymax = .data$prob
    ) %+%
    ggplot2::geom_point() %+%
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = cutoff),
      colour = "blueviolet",
      linetype = "dashed"
    ) %+%
    ggplot2::geom_linerange() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}
