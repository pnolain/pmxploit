#' Individual parameters distributions summary
#'
#' Summarizes the selected individual parameters: number of values, mean, median,
#' quantiles and range.
#'
#' @inheritParams summarize_continuous_covariates
#' @inheritParams summarize_parameters_correlations
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% summarize_parameters_distributions()
#'
#' EXAMPLERUN %>% summarize_parameters_distributions(quantiles = seq(0.05, 0.95, 0.05))
#' EXAMPLERUN %>% summarize_parameters_distributions(quantiles = NULL)
#'
#' EXAMPLERUN %>% group_by(STUD) %>% summarize_parameters_distributions()
#' EXAMPLERUN %>% group_by(STUD) %>% summarize_parameters_distributions(quantiles = NULL)
summarize_parameters_distributions <- function(run,
                                               parameters = NULL,
                                               quantiles = c(0.05, 0.25, 0.75, 0.95),
                                               baseline_only = TRUE) {
  stopifnot(!is.null(run))

  indiv_parameters <- run$model$parameters %>% filter(type %in% c("eta", "individual") & !is.na(column))

  if (is.null(parameters)) {
    mixed_parameters <- indiv_parameters %>% filter(type == "individual" & !is.na(column))
    parameters <- setNames(mixed_parameters$column, mixed_parameters$name)
  } else if (length(parameters) == 1 && parameters %in% c("eta", "individual")) {
    selected_parameters <- indiv_parameters %>% filter(type == parameters & !is.na(column))
    parameters <- setNames(selected_parameters$column, selected_parameters$name)
  } else {
    parameters <- get_selected_parameters(indiv_parameters, parameters)
  }

  if (length(parameters) == 0) stop(simpleError("No parameter found."))

  df <- run$tables$pmxploitab %>%
    get_reduced_dataset(baseline_only = baseline_only)

  if (nrow(df) == 0 & !is.null(attr(df, "filters"))) {
    stop(simpleError("Data is empty after filtering."))
  }

  split_by <- NULL
  if (!is.null(groups(df)) && length(groups(df)) > 0) {
    split_by <- as.character(groups(df))
    df <- ungroup(df)
  }

  if (!is.null(split_by)) {
    split_cov <- subset(run$model$covariates, column %in% split_by | name %in% split_by)
    if (nrow(split_cov) == 0) {
      stop(simpleError(paste("Missing splitting column(s):", paste(split_by, collapse = ", "))))
    }

    split_cov <- split_cov %>%
      mutate(matching_order = match(column, split_by)) %>%
      mutate(matching_order = ifelse(is.na(matching_order), match(name, split_by), matching_order)) %>%
      arrange(matching_order)

    split_by <- setNames(split_cov$column, nm = split_cov$name)
  }

  cols <- c(as.character(parameters), split_by)
  cols_names <- c(names(parameters), names(split_by))

  if (!is.null(split_by)) {
    for (i in seq_along(split_by)) {
      current_split <- split_by[[i]]
      if (current_split %in% colnames(df) & current_split %in% names(run$model$categorical_covariates_levels)) {
        levels <- run$model$categorical_covariates_levels[[current_split]]
        df[[current_split]] <- plyr::mapvalues(df[[current_split]],
          from = levels,
          to = names(levels)
        )
      }
    }
  }

  df <- df %>%
    select(ID, one_of(cols)) %>%
    rename(!!!setNames(cols, cols_names))

  if (!is.null(split_by)) {
    g_df <- df %>% gather(parameter, value, -ID, -one_of(names(split_by)), factor_key = TRUE)
  } else {
    g_df <- df %>% gather(parameter, value, -ID, factor_key = TRUE)
  }

  q_dots <- map(quantiles, function(x) {
    ~quantile(value, x)
  })
  names(q_dots) <- scales::percent(quantiles)

  dots <- c(
    n = ~length(value),
    n_distinct = ~n_distinct(value),
    mean = ~mean(value),
    median = ~median(value),
    sd = ~sd(value),
    q_dots,
    min = ~min(value),
    max = ~max(value)
  ) %>% map(as_quosure)

  if (!is.null(split_by)) {
    grps <- map(c("parameter", names(split_by)), as.name)

    g_df <- g_df %>% group_by(!!!grps)
  } else {
    g_df <- g_df %>% group_by(parameter)
  }

  summed_df <- g_df %>%
    summarise(!!!dots)

  summed_df
}
