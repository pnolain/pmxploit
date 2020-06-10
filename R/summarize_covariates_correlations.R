#' Continuous covariates correlations summary
#'
#' Returns a matrix of the selected continuous covariates correlations.
#'
#' @param auto_order logical. If \code{type = "heatmap"}, auto-orders the
#'   matrix with respect to the distances between values. Default is
#'   \code{TRUE}.
#' @param correlation_method a character string indicating which correlation
#'   coefficient (or covariance) is to be computed. One of \code{"pearson"}
#'   (default), \code{"kendall"}, or \code{"spearman"}: can be abbreviated. If
#'   \code{type = "scatterplot"}, \code{"pearson"} method will be used.
#' @param split_by character. Categorical to split the summary by.
#'
#' @inheritParams plot_continuous_covariates_distributions
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% summarize_covariates_correlations()
#' EXAMPLERUN %>% summarize_covariates_correlations(auto_order = FALSE)
summarize_covariates_correlations <-
  function(run, covariates = NULL, baseline_only = TRUE,
             correlation_method = NULL, auto_order = TRUE) {
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

    keep_cols <- c(covariates, split_by)

    df <- df %>%
      select(ID, one_of(keep_cols))

    fixed_covariates <- df %>%
      summarise_at(vars(one_of(covariates)), funs(length(unique(.)))) %>%
      gather(Parameter, N_unique) %>%
      filter(N_unique == 1)

    if (nrow(fixed_covariates) > 0) {
      removed_covs <- cont_covs %>% filter(column %in% fixed_covariates$Parameter)
      message(sprintf("Correlations are not computed for covariate(s) with one unique value: %s\n", paste(removed_covs$name, collapse = ", ")))

      covariates <- covariates[covariates %in%
        setdiff(covariates, unique(c(removed_covs$column, removed_covs$name)))]

      df <- df %>%
        select(-one_of(fixed_covariates$Parameter))
    }

    df <- df %>%
      rename(!!!setNames(covariates, names(covariates)))

    named_group <- split_by

    if (!is.null(split_by) && split_by %in% run$model$covariates$column && is.null(names(named_group))) {
      named_group <- setNames(split_by, filter(run$model$covariates, column == split_by)$name)

      levels <- run$model$categorical_covariates_levels[[split_by]]
      df[[split_by]] <- plyr::mapvalues(df[[split_by]], from = levels, to = names(levels))
    }

    if (!is.null(named_group)) {
      df <- df %>% rename(!!!named_group)
    }

    cor.matrix <- cor(select(df, one_of(names(covariates))), method = correlation_method, use = "pairwise.complete.obs")

    if (auto_order & ncol(cor.matrix) >= 2) {
      abs_matrix <- abs(cor.matrix)

      dd <- dist((1 - abs_matrix) / 2)
      hc <- hclust(dd)
      cor.matrix <- cor.matrix[hc$order, hc$order]
    }

    cor.matrix
  }
