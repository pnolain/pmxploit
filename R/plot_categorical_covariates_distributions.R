#' Categorical covariates distributions bar charts
#'
#' Plots the distributions of the selected categorical covariates.
#'
#' @param covariates character vector of categorical covariates names. Default is \code{NULL},
#'   returning all categorical covariates.
#' @param split_by character. Name of a categorical covariate used to split the
#'   distributions.
#' @param frequency logical. Plot frequency instead of count on the y-axis.
#' @param bar_adjustment character. \code{ggplot2} \code{position} argument. One
#'   of \code{identity}, \code{dodge}, \code{jitterdodge}, \code{jitter},
#'   \code{nudge} and \code{stack}. (see \code{\link{ggplot2}} documentation).
#'   Default is \code{dodge}.
#' @param baseline_only logical. Consider only the baseline (= first) values of the subjects. Default is
#'   \code{TRUE}.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>%
#'   plot_categorical_covariates_distributions(covariates = c("SEX", "STUD"))
#'
#' EXAMPLERUN %>%
#'   group_by(SEX) %>%
#'   plot_categorical_covariates_distributions(covariates = "STUD",
#'                                             frequency = TRUE)
plot_categorical_covariates_distributions <- function(run,
                                                      covariates = NULL,
                                                      frequency = FALSE,
                                                      order = TRUE,
                                                      drop = FALSE,
                                                      bar_adjustment = "dodge",
                                                      baseline_only = TRUE,
                                                      auto_legend = TRUE) {
  stopifnot(!is.null(run))

  tab <- summarize_categorical_covariates(run, covariates = covariates, baseline_only = baseline_only)

  if (drop) {
    tab <- tab %>% filter(n > 0)
  }

  facet_formula <- NULL
  lblr <- "label_value"
  cols <- NULL
  if (ncol(tab) > 4) { # grouping columns before "covariate" column
    cols <- colnames(tab)[1:(ncol(tab) - 4)]
    facet_formula <- as.formula(sprintf("~covariate + %s", str_c(syms(cols), collapse = "+")))

    lblr_map <- map(set_names(cols, cols), ~label_both)
    lblr <- do.call(labeller, args = lblr_map)
  } else {
    facet_formula <- ~covariate
  }

  if (order) {
    tab <- tab %>%
      group_by(!!!(syms(c("covariate", cols)))) %>%
      arrange(n) %>%
      ungroup() %>%
      mutate(order = as.factor(row_number()))

    g <- ggplot(tab, aes_string(x = "order", y = ifelse(frequency, "frequency", "n"), group = "covariate")) +
      geom_col() +
      scale_x_discrete(
        breaks = tab$order,
        labels = tab$value,
        expand = c(0, 0)
      )
  } else {
    g <- ggplot(tab, aes_string(x = "value", y = ifelse(frequency, "frequency", "n"), group = "covariate")) +
      geom_col()
  }

  g <- g + facet_wrap(facet_formula, scales = "free", labeller = lblr)

  if (auto_legend) {
    g <- g +
      labs(x = NULL, y = ifelse(frequency, "frequency", "count"), caption = str_c("Path: ", run$info$path))

    if (frequency) {
      g <- g + scale_y_continuous(labels = scales::percent)
    }
  }

  g
}
