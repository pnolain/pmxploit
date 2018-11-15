#' Parameters vs continuous covariates correlations
#'
#' Returns a matrix of the selected parameters and continuous covariates correlations.
#'
#'
#' @inheritParams plot_continuous_covariates_distributions
#' @inheritParams summarize_parameters_correlations
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% summarize_parameters_vs_continuous_covariates()
summarize_parameters_vs_continuous_covariates <-
  function(run,
             parameters = NULL,
             covariates = NULL,
           baseline_only = TRUE,
             correlation_method = NULL,
             auto_order = TRUE) {
    indiv_parameters <-
      run$model$parameters %>%
      filter(type %in% c("eta", "individual") &
        !is.na(column))
    cont_covs <-
      run$model$covariates %>%
      filter(type == "continuous")

    if (is.null(parameters)) {
      mixed_parameters <-
        indiv_parameters %>%
        filter(type == "individual" &
          !is.na(column))
      parameters <-
        setNames(mixed_parameters$column, mixed_parameters$name)
    } else if (length(parameters) == 1 &&
      parameters %in% c("eta", "individual")) {
      selected_parameters <-
        indiv_parameters %>%
        filter(type == parameters &
          !is.na(column))
      parameters <-
        setNames(selected_parameters$column, selected_parameters$name)
    } else {
      parameters <- get_selected_parameters(indiv_parameters, parameters)
    }

    if (is.null(covariates)) {
      covariates <- setNames(cont_covs$column, cont_covs$name)
    } else {
      covariates <- get_selected_covariates(cont_covs, covariates)
    }

    if (length(parameters) == 0) {
      stop(simpleError("No parameter found."))
    }
    if (length(covariates) == 0) {
      stop(simpleError("No covariate found."))
    }

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

    keep_cols <- c(parameters, covariates, split_by)

    df <- df %>%
      select(ID, one_of(keep_cols))

    fixed_values <- df %>%
      summarise_at(vars(one_of(parameters), one_of(covariates)), funs(length(unique(.)))) %>%
      gather(Value, N_unique) %>%
      filter(N_unique == 1)

    df <- df %>%
      rename(!!!setNames(parameters, names(parameters))) %>%
      rename(!!!setNames(covariates, names(covariates)))

    # subsets
    p_df <- df %>% select(ID, one_of(names(parameters)))
    c_df <- df %>% select(ID, one_of(names(covariates)))

    if (nrow(fixed_values) > 0) {
      fixed_values$type <-
        ifelse(fixed_values$Value %in% parameters,
          "parameter",
          "covariate"
        )
      fixed_params <- fixed_values %>% filter(type == "parameter")
      fixed_covs <- fixed_values %>% filter(type == "covariate")

      removed_params <-
        indiv_parameters %>%
        filter(column %in% fixed_params$Value)
      if (nrow(removed_params) > 0) {
        message(simpleMessage(
          sprintf(
            "Correlations are not computed for parameter(s) with one unique value: %s\n",
            paste(removed_params$name, collapse = ", ")
          )
        ))
        parameters <- parameters[parameters %in%
          setdiff(parameters, unique(c(
            as.character(removed_params$id),
            removed_params$column
          )))]

        p_df <- p_df %>% select(-one_of(removed_params$name))
      }

      removed_covs <-
        run$model$covariates %>%
        filter(column %in% fixed_covs$Value)
      if (nrow(removed_covs) > 0) {
        message(simpleMessage(
          sprintf(
            "Correlations are not computed for covariate(s) with one unique value: %s\n",
            paste(removed_covs$name, collapse = ", ")
          )
        ))
        covariates <- covariates[covariates %in%
          setdiff(covariates, unique(c(
            removed_covs$column, removed_covs$name
          )))]

        c_df <- c_df %>% select(-one_of(removed_covs$name))
      }
    }

    named_group <- split_by

    if (!is.null(split_by) &&
      split_by %in% run$model$covariates$column &&
      is.null(names(named_group))) {
      named_group <-
        setNames(
          split_by,
          filter(run$model$covariates, column == split_by)$name
        )

      levels <- run$model$categorical_covariates_levels[[split_by]]
      df[[split_by]] <-
        plyr::mapvalues(df[[split_by]], from = levels, to = names(levels))
    }

    if (!is.null(named_group)) {
      df <- df %>% rename(!!!named_group)
    }

    cor.matrix <- cor(
      x = select(p_df, one_of(names(parameters))),
      y = select(c_df, one_of(names(covariates))),
      method = correlation_method, use = "pairwise.complete.obs"
    )

    if (nrow(cor.matrix) == 0 | ncol(cor.matrix) == 0) {
      stop(simpleError("Correlation matrix could not be computed."))
    }

    if (auto_order) {
      if (ncol(cor.matrix) == nrow(cor.matrix)) {
        if (ncol(cor.matrix) >= 2) {
          abs_matrix <- abs(cor.matrix)
          dd <- dist((1 - abs_matrix) / 2)
          hc <- hclust(dd)
          cor.matrix <- cor.matrix[hc$order, hc$order]
        }
      } else {
        message(simpleMessage("Auto ordering not supported for non-square matrix.\n"))
      }
    }

    return(cor.matrix)
  }
