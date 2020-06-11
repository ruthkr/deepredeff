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

  proportion_per_class <- object %>%
    dplyr::group_by(.data$pred_class) %>%
    dplyr::summarise(proportion = dplyr::n() / nrow(object))

  df_summary <- dplyr::left_join(count_per_class, proportion_per_class, by = "pred_class")

  return(df_summary)
}


#' Plot the results of prediction
#'
#' @param object Results of prediction from deepredeff::predict_effector().
#' @param cutoff Cutoff value to determine the prediction class (1 for effector and 0 for non-effector). The default value is 0.5.
#' @param ... Additional arguments ignored.
#'
#' @return Plot depending on the chosen type.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
autoplot.tbl_deepredeff <- function(object, cutoff = 0.5, ...) {
  `%+%` <- ggplot2::`%+%`
  object <- object %>%
    dplyr::mutate(class = factor(ifelse(.data$prob >= cutoff, 1, 0), levels = c(1,0), labels = c("effector", "non-effector")))

  plot <- object %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$prob,
      fill = .data$class
    ) +
    ggplot2::geom_density() +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.025)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1000, 1), minor_breaks = NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::theme_bw()

  return(plot)
}




