new_heatmap_deepredeff <- function(x, description = names(x)) {
  if (is.null(description)) {
    description <- rep("", length(x))
  }
  names(x) <- description
  structure(x, class = c("heatmap_deepredeff", "numeric"), description = description)
}

#' @export
`[.heatmap_deepredeff` <- function(x, i, ...) {
  new_x <- NextMethod("[")
  new_heatmap_deepredeff(new_x)
}

#' Plot the heatmap
#'
#' @param object Results from deepredeff::get_sum_activation().
#' @param n Number of harmonics.
#' @param ... Additional arguments ignored.
#'
#' @return Plot of raw heatmap data and its smooth version.
#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 autoplot
autoplot.heatmap_deepredeff <- function(object, n = 50, ...) {
  data <- data.frame(
    index = seq_along(object),
    value_raw = object,
    value_smooth = get_fft_smooth(object, n)
  )

  gg <- data %>%
    # tidyr::pivot_longer(
    #   cols = -c(.data$index),
    #   names_to = "curve"
    # ) %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$index
    ) +
    # ggplot2::geom_point(aes(y = .data$value_raw, color = "value_raw")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$value_raw, color = "value_raw")) +
    # ggplot2::geom_point(aes(y = .data$value_smooth, color = "value_smooth")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$value_smooth, color = "value_smooth"))

  return(gg)
}
