#' Plot parameters vs categorical covariates correlations
#'
#' Plots the selected parameters and categorical covariates correlations.
#'
#' @inheritParams plot_parameters_correlations
#' @inheritParams plot_categorical_covariates_distributions
#' @inheritParams summarize_parameters_correlations
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>%
#'   plot_parameters_vs_categorical_covariates(parameters = c("ETCL", "ETV1", "ETKA"),
#'                                             covariates = c("ISC", "SEX", "STUD"))
plot_parameters_vs_categorical_covariates <-
  function(run, parameters = NULL, covariates = NULL,
           baseline_only = TRUE, auto_legend = TRUE) {
    indiv_parameters <- run$model$parameters %>% filter(type %in% c("eta", "individual") & !is.na(column))
    cat_covs <- run$model$covariates %>% filter(type == "categorical")

    if (is.null(parameters)) {
      mixed_parameters <- indiv_parameters %>% filter(type == "individual" & !is.na(column))
      parameters <- setNames(mixed_parameters$column, mixed_parameters$name)
    } else if (length(parameters) == 1 && parameters %in% c("eta", "individual")) {
      selected_parameters <- indiv_parameters %>% filter(type == parameters & !is.na(column))
      parameters <- setNames(selected_parameters$column, selected_parameters$name)
    } else {
      parameters <- get_selected_parameters(indiv_parameters, parameters)
    }

    if (is.null(covariates)) {
      covariates <- setNames(cat_covs$column, cat_covs$name)
    } else {
      covariates <- get_selected_covariates(cat_covs, covariates)
    }

    if (length(parameters) == 0) stop(simpleError("No parameter found."))
    if (length(covariates) == 0) stop(simpleError("No covariate found."))

    df <- run$tables$pmxploitab %>%
      get_reduced_dataset(baseline_only = baseline_only)

    if (nrow(df) == 0 & !is.null(attr(df, "filters"))) {
      stop(simpleError("Data is empty after filtering."))
    }

    df <- df %>%
      select(ID, one_of(parameters), one_of(unname(covariates))) %>%
      mutate_at(vars(one_of(covariates)), as.factor)

    df <- df %>%
      rename(!!!parameters) %>%
      rename(!!!covariates)

    g_df <- df %>%
      gather(parameter, param_value, one_of(names(parameters)), factor_key = TRUE) %>%
      gather(covariate, cov_value, one_of(names(covariates)), factor_key = TRUE)

    g <- ggplot(g_df) +
      geom_boxplot(aes(x = cov_value, y = param_value, group = cov_value, fill = cov_value)) +
      facet_wrap(parameter ~ covariate, scales = "free") # , nrow = length(parameters))

    if (auto_legend) {
      g <- g +
        labs(x = NULL, y = NULL, caption = str_c("Path: ", run$info$path)) +
        scale_fill_discrete(guide = FALSE)
    }

    g
  }
