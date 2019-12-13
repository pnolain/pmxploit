#' Categorical covariates summary
#'
#' Summarizes the selected categorical covariates, returning counts and frequencies
#' of each levels.
#'
#' @inheritParams plot_categorical_covariates_distributions
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% summarize_categorical_covariates()
summarize_categorical_covariates <- function(run,
                                             covariates = NULL,
                                             baseline_only = TRUE) {
  stopifnot(!is.null(run))

  cont_covs <- run$model$covariates %>% filter(type == "categorical")

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
  if (!is.null(groups(df))) {
    split_by <- as.character(groups(df))
    df <- ungroup(df)
  }

  if (!is.null(split_by)) {
    split_cov <- filter(run$model$covariates, column %in% split_by | name %in% split_by)
    if (nrow(split_cov) == 0) {
      stop(simpleError(paste("Missing splitting column:", paste(split_by, collapse = ", "))))
    }

    split_by <- setNames(split_cov$column, nm = split_cov$name)
  }

  cols <- c(as.character(covariates), split_by)

  df <- df %>%
    select(ID, one_of(cols)) %>%
    mutate_at(vars(-ID), funs(as.integer(as.character(.))))

  if (!is.null(split_by)) {
    if (all(covariates %in% split_by)) {
      stop(simpleError("No covariate found, you may need to ungroup the data first."))
    }

    # group_dots <- list(~covariate, ~value, eval(parse(text = sprintf("~%s", names(split_by)))))
    grps <- map(c(unname(split_by), "covariate", "value"), as.name)

    # groups <- g_df %>% group_by(!!!grps) %>% summarise() %>% ungroup()

    g_df <- df %>%
      gather(covariate, value, -ID, -one_of(split_by), factor_key = TRUE) %>%
      group_by(!!!(grps))
  } else {
    g_df <- df %>%
      gather(covariate, value, -ID, convert = TRUE, factor_key = TRUE) %>%
      group_by(covariate, value)
  }

  summed_df <- g_df %>%
    summarise(n = dplyr::n()) %>%
    mutate(frequency = n / sum(n)) %>%
    ungroup()

  if (!is.null(split_by)) {
    cargs <- syms(unname(split_by))

    summed_df <- do.call(complete,
      args = append(list(
        data = summed_df,
        quote(nesting(covariate, value)),
        fill = list(n = 0, frequency = 0)
      ),
      cargs,
      after = 1
      )
    )
  }

  for (cn in split_by) {
    levels <- run$model$categorical_covariates_levels[[cn]]

    summed_df[[cn]] <- plyr::mapvalues(summed_df[[cn]],
      from = levels,
      to = names(levels), warn_missing = FALSE
    )
  }

  summed_df <- summed_df %>%
    mutate(value = map2_chr(covariate, value, function(cov, val) {
    lvls <- run$model$categorical_covariates_levels[[as.character(cov)]]
    if(!(as.character(val) %in% lvls))
      return("NA")
    names(lvls[lvls == as.character(val)])
  }))

  summed_df <- summed_df %>%
    rename(!!!setNames(split_by, names(split_by)))

  summed_df
}
