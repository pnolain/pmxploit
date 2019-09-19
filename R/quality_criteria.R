#' Quality criteria (QC) computations
#'
#' Computation of common quality criteria for evaluation of model predictions
#' performance: standard qc (average fold error, maximal error), bias (mean predictions error), precision
#' (root mean square error), Student's t-Test, correlation test and linear
#' regression.
#'
#' @param predictions character. Name of the predictions column in the result
#'   tables. Default is \code{"PRED"} for population predictions.
#' @param alpha numeric. Alpha risk. Used for bias confidence interval
#'   computation and T-test comparing observations and predictions. Default is
#'   \code{5\%}.
#' @param drop_empty_splits logical. Drop empty split groups from QC results.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @details For quality criteria computations, residuals are computed based on
#'   the formula:
#'
#'   \eqn{pred_err_i = pred_i - obs_i} \itemize{ \item Standard QC \itemize{ \item Maximal
#'   Error:
#'   \deqn{ME=max(|obs-pred|)}
#'   \item Absolute Average Fold Error:
#'
#'   \deqn{AAFE=10^(mean(log10(pred/obs)))}
#'   For reference, see \url{https://www.ncbi.nlm.nih.gov/pubmed/26696327}}
#'   \item Bias: Mean Prediction Error (MPE) \itemize{ \item Absolute:
#'   \code{mean(pred_err)} \item Confidence interval for a given \code{alpha} \item
#'   Relative: \code{mean(pred_err/obs)} } \item Precision: Root Mean Square
#'   Error (RMSE) \itemize{ \item Absolute: Student's t-Test estimate of
#'   \code{t.test((obs - pred)^2)} \item Confidence interval for a given
#'   \code{alpha} \item Relative: \code{rmse/mean(obs)} } \item Student's
#'   t-Test: observations vs predictions (paired, two-sided)
#'
#'   Returns estimate, statistic, p-value, degrees of freedom (parameter) and
#'   confidence interval given \code{alpha}. \item Correlation test between
#'   observations and predictions
#'
#'   Returns estimate, statistic, p-value, degrees of freedom (parameter) and
#'   confidence interval given \code{alpha}. \item Linear regression: \eqn{pred
#'   = intercept + slope * obs}
#'
#'   Returns intercept and slope estimates, standard errors, statistics and
#'   p-values given \code{alpha}. }
#'
#' @return A list containing the quality criteria analysis results.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% quality_criteria()
#'
#' EXAMPLERUN %>% quality_criteria(alpha = 0.01)
#'
#' EXAMPLERUN %>% group_by(SEX) %>% quality_criteria()
#'
#' EXAMPLERUN %>% filter(STUD == 2) %>% quality_criteria()
#'
#' EXAMPLERUN %>% filter(STUD != 2) %>% quality_criteria()
#'
#' EXAMPLERUN %>% filter(AGE <= 24) %>% quality_criteria()
quality_criteria <- function(run,
                             predictions = "PRED",
                             log_data = FALSE,
                             alpha = 0.05,
                             drop_empty_splits = FALSE) {
  df <- run$tables$pmxploitab %>%
    filter(!is.na(DV))

  split_by <- NULL
  if (!is.null(groups(df))) {
    split_by <- as.character(groups(df))
    df <- ungroup(df)
  }

  if ("EVID" %in% colnames(run$tables$pmxploitab)) {
    df <- filter(df, EVID == 0)
  }

  if (nrow(df) == 0 & !is.null(attr(df, "filters"))) {
    stop(simpleError("Data is empty after filtering."))
  }

  if (!is.null(split_by)) {
    possible_splits <- run$model$covariates %>%
      filter(type == "categorical") %>%
      select(column, name) %>%
      add_row(column = "CMT", name = "CMT")

    split_col <- subset(possible_splits, column %in% split_by | name %in% split_by)
    if (nrow(split_col) == 0) {
      stop(simpleError(paste("Missing splitting column(s):", paste(split_by, collapse = ", "))))
    }

    split_col <- split_col %>%
      mutate(matching_order = match(column, split_by)) %>%
      mutate(matching_order = ifelse(is.na(matching_order), match(name, split_by), matching_order)) %>%
      arrange(matching_order)

    split_by <- setNames(split_col$column, nm = split_col$name)

    for (i in seq_along(split_by)) {
      current_split <- split_by[[i]]
      if (current_split == "CMT") {
        df$CMT <- plyr::mapvalues(df$CMT, from = run$model$compartments$cmt, to = run$model$compartments$name, warn_missing = FALSE)
      } else if (current_split %in% colnames(df) & current_split %in% names(run$model$categorical_covariates_levels)) {
        levels <- run$model$categorical_covariates_levels[[current_split]]
        df[[current_split]] <- plyr::mapvalues(df[[current_split]],
          from = levels,
          to = names(levels)
        )
      }
    }

    if (any(split_by != names(split_by))) {
      df <- df %>% rename(!!!setNames(split_by, names(split_by)))
    }

    data_list <- df %>%
      split(.[names(split_by)], drop = drop_empty_splits, sep = "|")
  } else {
    data_list <- list(df)
  }

  qc_df <- data_list %>%
    purrr::map_df(.id = (if (length(split_by) > 0) ".split" else NULL), function(sub_df) {
      if (nrow(sub_df) == 0) {
        return(tibble(
          n_observations = 0,
          standard = list(tibble()),
          bias = list(tibble()),
          precision = list(tibble()),
          t_test_obs = NA,
          t_test_res = NA,
          correlation_test = NA,
          linear_regression = NA,
          data = list(sub_df)
        ))
      }

      observations <- sub_df$DV
      pred <- sub_df[[predictions]]

      if (log_data) {
        observations <- exp(observations)
        pred <- exp(pred)

        if(any(is.infinite(c(observations, pred))))
          stop(simpleError("Infinite values after exponential transformation."))
      }

      if (is.null(pred)) {
        stop(simpleError("No predictions found in output tables."))
      }

      #res <- observations - pred
      pred_err <- pred - observations
      obs_mean <- mean(observations)

      n <- nrow(sub_df)
      pred_err_mean <- mean(pred_err)
      pred_err_se <- sd(pred_err) / sqrt(n)

      # MPE = Mean Prediction Error (bias)
      bias_val <- qnorm(1 - alpha / 2) * pred_err_se
      bias <- tibble(
        value = pred_err_mean,
        ci_low = pred_err_mean - bias_val,
        ci_up = pred_err_mean + bias_val,
        # relative_value = value / obs_mean
        relative_value = mean(pred_err / observations)
      )

      #   # ME = Maximal Error
      #   max_e <- psred_err %>% abs %>% max
      #
      #   # AAFE = Absolute Average Fold Error
      #
      #   Reference:
      #   - https://www.ncbi.nlm.nih.gov/pubmed/26696327
      #   fe <- ifelse(predictions > observations, predictions / observations, observations / predictions)
      #   finite_fe <- fe[is.finite(fe)]
      #   aafe <- 10^(fe[is.finite(fe)] %>% log10() %>% abs() %>% mean

      # Standard QC table
      standard_qc <- tibble(observations, pred, pred_err) %>%
        filter(observations !=0 & pred != 0) %>%
        mutate(
          square_obs = (observations - obs_mean)^2,
          square_pred = (pred_err)^2,
          fe = (pred / observations)
        ) %>%
        summarise(
          max_err = max(abs(pred_err)),
          aafe = 10^(fe[is.finite(fe)] %>% log10() %>% abs() %>% mean())
        )

      # T-test observations predictions
      t_test_obs <- t.test(
        x = observations, y = pred, alternative = "two.sided", mu = 0, paired = TRUE, var.equal = TRUE,
        conf.level = 1 - alpha
      )

      # T-test residuals
      t_test_res <- NULL
      if (length(run$model$residuals) > 0) {
        t_test_res <- tibble(residuals = run$model$residuals) %>%
          mutate(t.test = map(run$model$residuals, function(res) {
            res_data <- sub_df[[res]]
            t.test(
              x = res_data, y = rep(0, length(res_data)), alternative = "two.sided", mu = 0, paired = TRUE, var.equal = TRUE,
              conf.level = 1 - alpha
            )
          }))
      }

      # Correlation test
      corr_test <- NULL
      OK <- complete.cases(observations, pred)
      n_ok <- length(observations[OK])
      if (n_ok >= 3) { # pearson method needs at least 3 finite observations
        corr_test <- cor.test(x = observations, y = pred, alternative = "two.sided", method = "pearson")
      }

      # Linear regression
      lin_reg <- lm(pred ~ observations)

      # RMSE
      t_test_square <- t.test(pred_err^2, conf.level = 1 - alpha) %>% broom::tidy()

      rmse <- tibble(
        value = sqrt(t_test_square$estimate),
        ci_low = sqrt(t_test_square$conf.low),
        ci_up = sqrt(t_test_square$conf.high),
        relative_value = value / obs_mean
      )

      tibble(
        n_observations = nrow(sub_df),
        standard = list(standard_qc),
        bias = list(bias),
        precision = list(rmse),
        t_test_obs = list(t_test_obs),
        t_test_res = list(t_test_res),
        correlation_test = list(corr_test),
        linear_regression = list(lin_reg),
        data = list(sub_df)
      )
    })

  if (length(split_by) > 0) {
    qc_df <- qc_df %>% separate(.split, into = names(split_by), sep = "\\|")

    splits <- qc_df[, names(split_by), ] %>%
      mutate(group = row_number()) %>%
      gather(key, value, -group) %>%
      unite(all, key, value, sep = ": ") %>%
      group_by(group) %>%
      summarise(split_name = paste(all, collapse = ", "))

    attr(qc_df, "splits") <- splits$split_name
  }

  qc_df
}
