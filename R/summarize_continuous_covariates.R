#' Continuous covariates distributions summary
#'
#' Summarizes the selected continuous covariates: number of values, mean, median,
#' quantiles and range.
#'
#' @param quantiles numeric vector of quantiles. Default are 5th, 25th, 75th and 95th percentiles.
#' @inheritParams plot_continuous_covariates_distributions
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% summarize_continuous_covariates()
#'
#' EXAMPLERUN %>% summarize_continuous_covariates(quantiles = seq(0.05, 0.95, 0.05))
#' EXAMPLERUN %>% summarize_continuous_covariates(quantiles = NULL)
#'
#' EXAMPLERUN %>% group_by(STUD) %>%  summarize_continuous_covariates()
#' EXAMPLERUN %>% group_by(STUD) %>% summarize_continuous_covariates(quantiles = NULL)
summarize_continuous_covariates <- function(run,
                                            covariates = NULL,
                                            quantiles = c(0.05, 0.25, 0.75, 0.95),
                                            baseline_only = TRUE) {
  stopifnot(!is.null(run))

  cont_covs <- run$model$covariates %>% filter(type == "continuous")

  if (is.null(covariates)) {
    covariates <- setNames(cont_covs$column, cont_covs$name)
  } else {
    covariates <- get_selected_covariates(cont_covs, covariates)
  }

  if (length(covariates) == 0) stop(simpleError("No covariate found."))

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
      stop(simpleError(paste("Missing splitting column(s):", paste(split_by, collapse = ", "))), call. = FALSE)
    }

    split_cov <- split_cov %>%
      mutate(matching_order = match(column, split_by)) %>%
      mutate(matching_order = ifelse(is.na(matching_order), match(name, split_by), matching_order)) %>%
      arrange(matching_order)

    split_by <- setNames(split_cov$column, nm = split_cov$name)
  }

  cols <- c(as.character(covariates), split_by)
  cols_names <- c(names(covariates), names(split_by))

  df <- df %>%
    select(ID, one_of(cols))

  for (cat_name in names(run$model$categorical_covariates_levels)) {
    if (cat_name %in% colnames(df)) {
      levels <- run$model$categorical_covariates_levels[[cat_name]]
      df[[cat_name]] <- plyr::mapvalues(df[[cat_name]],
        from = levels,
        to = names(levels)
      )
    }
  }

  df <- df %>%
    rename(!!!setNames(cols, cols_names))

  if (!is.null(split_by)) {
    g_df <- df %>% gather(covariate, value, -ID, -one_of(names(split_by)), factor_key = TRUE)
  } else {
    g_df <- df %>% gather(covariate, value, -ID, factor_key = TRUE)
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
    grps <- map(c("covariate", names(split_by)), as.name)

    g_df <- g_df %>% group_by(!!!grps)
  } else {
    g_df <- g_df %>% group_by(covariate)
  }

  summed_df <- g_df %>%
    filter(!is.na(value)) %>%
    summarise(!!!dots)


  summed_df
}
