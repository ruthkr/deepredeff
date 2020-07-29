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

#' Plot the smooth heatmap value
#'
#' This function will smooth the heatmap value using fourier transform.
#'
#' @param x The heatmap value results.
#' @param n Number of harmonics.
#' @param ... Additional arguments ignored.
#'
#' @return Plot of heatmap value.
#' @export
#' @importFrom rlang .data
plot.heatmap_deepredeff <- function(x, n = 50, ...) {
  `%+%` <- ggplot2::`%+%`

  data <- data.frame(
    index = seq_along(x),
    value_smooth = get_fft_smooth(x, n)
  )

  gg <- data %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$index,
      y = .data$value_smooth,
      color = "whatevr"
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Sequence position", y = "Value") +
    ggthemes::scale_color_ptol()

  return(gg)
}
