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
#' @param ... Additional arguments ignored.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # FASTA input
#' input_fasta <- system.file("extdata/example/fungi_sample.fasta", package = "deepredeff")
#'
#' pred_result <- deepredeff::predict_effector(
#'   input = input_fasta,
#'   taxon = "fungi"
#' )
#'
#' summary(pred_result)
#' }
#'
#' @importFrom rlang .data
summary.tbl_deepredeff <- function(object, ...) {
  table_summary <- table(factor(object$prediction, levels = c("effector", "non-effector")))

  res <- list(
    total_seq_input = nrow(object),
    model_names = attr(object, "model_names"),
    taxon = attr(object, "taxon"),
    num_effector = table_summary[["effector"]],
    num_non_effector = table_summary[["non-effector"]]
  )

  class(res) <- "summary.tbl_deepredeff"

  return(res)
}

#' @export
print.summary.tbl_deepredeff <- function(x, ...) {
  cat(
    paste0(
      "Total sequences in input data: ", x$total_seq_input,
      "\n---\n",
      "Taxon chosen: ", x$taxon,
      "\n",
      "Model type used: ", x$model_names,
      "\n\n",
      "Total sequences predicted as effector: ", x$num_effector,
      "\n",
      "Total sequences predicted as non-effector: ", x$num_non_effector
    )
  )
}


#' Plot the results of prediction
#'
#' @param x tbl_deepredeff object
#' @param ... additional arguments ignored.
#'
#' @return class distribution plot
#' @export
#'
#' @examples
#' \donttest{
#' # FASTA input
#' input_fasta <- system.file("extdata/example/fungi_sample.fasta", package = "deepredeff")
#'
#' pred_result <- deepredeff::predict_effector(
#'   input = input_fasta,
#'   taxon = "fungi"
#' )
#'
#' plot(pred_result)
#' }
#'
#' @importFrom rlang .data
plot.tbl_deepredeff <- function(x, ...) {
  `%+%` <- ggplot2::`%+%`

  # Set message either group has fewer than two data points
  cond_effector <- (table(x$prediction)[["effector"]] < 2)
  cond_non_effector <- (table(x$prediction)[["non-effector"]] < 2)

  cond <- c(cond_effector, cond_non_effector)
  names(cond) <- c("effector", "non-effector")

  if (cond_effector | cond_non_effector) {
    message(paste0("Plot can not be generated for group ", paste(names(cond[which(cond)]), collapse = " and group "), ". Each group needs to have at least two data points."))
  }

  plot <- x %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$s_score,
      fill = .data$prediction
    ) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::labs(x = "S-score", y = "Density", fill = "Prediction") +
    ggthemes::scale_fill_ptol()


  return(plot)
}
