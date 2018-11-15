#' Parameters correlations
#'
#' Returns the selected parameters correlations in the form of either a matrix, a
#' data frame or a plot.
#'
#' @param parameters character vector of parameters names. Default is \code{NULL},
#'   returning all individual parameters (random and post-hoc).
#'
#' @inheritParams summarize_covariates_correlations
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% summarize_parameters_correlations()
#' EXAMPLERUN %>% summarize_parameters_correlations(auto_order = FALSE)
summarize_parameters_correlations <-
  function(run,
           parameters = NULL,
           baseline_only = TRUE,
           correlation_method = NULL,
           auto_order = TRUE) {
    indiv_parameters <-
      run$model$parameters %>%
      filter(type %in% c("eta", "individual") &
        !is.na(column))

    if (is.null(parameters)) {
      mixed_parameters <-
        indiv_parameters %>%
        filter(type == "individual" & !is.na(column))
      parameters <-
        setNames(mixed_parameters$column, mixed_parameters$name)
    } else if (length(parameters) == 1 &&
      parameters %in% c("eta", "individual")) {
      selected_parameters <-
        indiv_parameters %>%
        filter(type == parameters & !is.na(column))
      parameters <-
        setNames(selected_parameters$column, selected_parameters$name)
    } else {
      parameters <- get_selected_parameters(indiv_parameters, parameters)
    }

    if (length(parameters) == 0) {
      stop(simpleError("No parameter found."))
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


    keep_cols <- c(parameters, split_by)

    df <- df %>%
      select(ID, one_of(keep_cols))

    fixed_parameters <- df %>%
      summarise_at(vars(one_of(parameters)), funs(length(unique(.)))) %>%
      gather(Parameter, N_unique) %>%
      filter(N_unique == 1)

    if (nrow(fixed_parameters) > 0) {
      removed_params <-
        indiv_parameters %>%
        filter(column %in% fixed_parameters$Parameter)
      message(
        sprintf(
          "Correlations are not computed for parameters(s) with one unique value: %s\n",
          paste(removed_params$name, collapse = ", ")
        )
      )

      parameters <- parameters[parameters %in%
        setdiff(parameters, unique(c(
          as.character(removed_params$id), removed_params$column
        )))]

      df <- df %>%
        select(-one_of(fixed_parameters$Parameter))
    }

    df <- df %>%
      rename(!!!setNames(parameters, names(parameters)))

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
    cor.matrix <-
      cor(select(df, one_of(names(parameters))), method = correlation_method, use = "pairwise.complete.obs")

    if (auto_order & ncol(cor.matrix) >= 2) {
      abs_matrix <- abs(cor.matrix)

      dd <- dist((1 - abs_matrix) / 2)
      hc <- hclust(dd)
      cor.matrix <- cor.matrix[hc$order, hc$order]
    }

    cor.matrix
  }
