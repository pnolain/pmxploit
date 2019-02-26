#' Individual parameters distributions
#'
#' Plots the distributions of the selected individual parameters.
#'
#' @inheritParams plot_continuous_covariates_distributions
#' @inheritParams summarize_parameters_correlations
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% plot_parameters_distributions()
#' EXAMPLERUN %>% plot_parameters_distributions(type = "density")
#' EXAMPLERUN %>% plot_parameters_distributions(type = "boxplot")
#' EXAMPLERUN %>% plot_parameters_distributions(type = "qq")
#'
#' EXAMPLERUN %>% plot_parameters_distributions(parameters = "eta")
#'
#' EXAMPLERUN %>%
#'   plot_parameters_distributions(parameters = "ETCL",
#'                                 histogram_empirical_density = TRUE,
#'                                 histogram_reference_distribution = list(fun = dnorm,
#'                                                                         args = list(mean = 0, sd = 1)))
plot_parameters_distributions <- function(run, parameters = NULL,
                                          baseline_only = TRUE, type = "histogram", histogram_bins = 30L,
                                          histogram_empirical_density = FALSE,
                                          histogram_reference_distribution = list(fun = dnorm, args = list(mean = 0, sd = 1)),
                                          qq_reference_distribution = list(fun = qnorm, args = list(mean = 0, sd = 1)),
                                          boxplot_facets = TRUE, boxplot_drop_unused = FALSE,
                                          facet_scales = "free", overlay_splits = TRUE, auto_legend = TRUE) {
  indiv_parameters <- run$model$parameters %>% filter(type %in% c("eta", "individual") & !is.na(column))

  if (is.null(parameters)) {
    mixed_parameters <- indiv_parameters
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
  if (!is.null(groups(df))) {
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

  if (!is.null(split_by)) {
    wrap_formula <- (if (length(split_by) == 1) {
      as.formula(~parameter)
    } else {
      safe_names <- names(split_by[-1]) %>% purrr::map(as.name)
      as.formula(sprintf("parameter ~ %s", paste(safe_names, collapse = "+")))
    })
  }

  corrected_split_names <- sprintf("`%s`", names(split_by))

  if (type == "boxplot") {
    # boxplot
    if (!is.null(split_by)) {
      if (!boxplot_facets) {
        grouping_key <- paste(names(split_by), collapse = ".")
        if(length(split_by) > 1){
          g_df <- g_df %>%
            mutate(interaction_group = interaction(!!!syms(names(split_by)))) %>%
            rename(!!!setNames(nm = grouping_key, "interaction_group"))
        }

        g <- ggplot(g_df, aes_string(x = "parameter", y = "value", fill = as.name(grouping_key))) +
          geom_boxplot()
      } else {
        g <- ggplot(g_df, aes_string(x = corrected_split_names[1], y = "value", fill = corrected_split_names[1])) +
          geom_boxplot() +
          facet_wrap(wrap_formula, scales = facet_scales)
      }
    } else {
      g <- ggplot(g_df, aes(x = parameter, y = value)) +
        geom_boxplot()

      if (boxplot_facets) {
        g <- g + facet_wrap(~parameter, scales = facet_scales)
      }
    }
  } else if (type == "qq") {
    qq_dist <- qq_reference_distribution

    if (!is.null(split_by)) {
      grps <- map(c("parameter", names(split_by)), as.name)
      intsl <- g_df %>% group_by(!!!grps)
    } else {
      intsl <- g_df %>% group_by(parameter)
    }

    # compute theoretical qq int/slope
    intsl <- intsl %>% summarise(
      q25 = quantile(value, 0.25),
      q75 = quantile(value, 0.75),
      theo25 = do.call(qq_dist$fun,
        args = c(
          p = 0.25,
          qq_dist$args
        )
      ),
      theo75 = do.call(qq_dist$fun,
        args = c(
          p = 0.75,
          qq_dist$args
        )
      ),
      slope = (q25 - q75) / (theo25 - theo75),
      int = q25 - slope * theo25, n = dplyr::n()
    ) # %>%


    if (!is.null(split_by)) {
      if (overlay_splits) {
        g <- ggplot(g_df) +
          geom_qq(aes_string(sample = "value", colour = corrected_split_names[1]), distribution = qq_dist$fun) +
          geom_abline(data = intsl, aes_string(intercept = "int", slope = "slope", colour = corrected_split_names[1])) +
          facet_wrap(wrap_formula, scales = facet_scales)
      } else {
        wrap_formula <- as.formula(sprintf("parameter ~ %s", paste(corrected_split_names[1], collapse = "+")))

        g <- ggplot(g_df) +
          geom_qq(aes(sample = value), distribution = qq_dist$fun) +
          geom_abline(data = intsl, aes(intercept = int, slope = slope), colour = "blue") +
          facet_wrap(wrap_formula, scales = facet_scales)
      }
    } else {
      g <- ggplot(g_df) +
        geom_qq(aes(sample = value), distribution = qq_dist$fun) +
        geom_abline(data = intsl, aes(intercept = int, slope = slope), colour = "blue") +
        facet_wrap(~parameter, scales = facet_scales)
    }

    # g <- g + geom_abline(linetype = "dashed", colour = "red")
  } else {
    dist_geom <- switch(type,
      "histogram" = {
        geom_histogram(bins = histogram_bins)
      },
      "density" = {
        geom_density(alpha = 0.5)
      }, {
        stop(simpleError(paste("Unknow plot type:", type)))
      }
    )

    if (!is.null(split_by)) {
      if (overlay_splits) {
        g <- ggplot(g_df, aes_string(x = "value", fill = corrected_split_names[1])) +
          dist_geom +
          facet_wrap(wrap_formula, scales = facet_scales)
      } else {
        wrap_formula <- as.formula(sprintf("parameter ~ %s", paste(corrected_split_names[1], collapse = "+")))

        g <- ggplot(g_df, aes(x = value)) +
          dist_geom +
          facet_wrap(wrap_formula, scales = facet_scales)
      }
    } else {
      g <- ggplot(g_df, aes(x = value)) +
        dist_geom +
        facet_wrap(~parameter, scales = facet_scales)
    }

    if (histogram_empirical_density) {
      g$mapping$y <- as.name(quote(..density..))

      g <- g + stat_density(geom = "line", mapping = aes(colour = "Empirical", linetype = "Empirical"))

      if (!is.null(histogram_reference_distribution) &&
        all(c("fun", "args") %in% names(histogram_reference_distribution))) {
        g <- g + stat_function(aes(colour = "Reference", linetype = "Reference"),
          inherit.aes = FALSE,
          fun = histogram_reference_distribution$fun,
          args = histogram_reference_distribution$args
        )
      }

      g <- g + scale_colour_manual("Distributions",
        values = c(
          "Empirical" = getOption("pmxploit.parametersdistributionsplot.empiricaldistribution.colour"),
          "Reference" = getOption("pmxploit.parametersdistributionsplot.referencedistribution.colour")
        )
      ) +
        scale_linetype_manual("Distributions",
          values = c(
            "Empirical" = getOption("pmxploit.parametersdistributionsplot.empiricaldistribution.linetype"),
            "Reference" = getOption("pmxploit.parametersdistributionsplot.referencedistribution.linetype")
          )
        )
    }
  }

  if (auto_legend) {
    g <- g + labs(caption = str_c("Path: ", run$info$path))

    if (!is.null(split_by)) {
      if (type == "qq") {
        g <- g + scale_colour_discrete(name = paste(names(split_by), collapse = "."))
      } else {
        g <- g + scale_fill_discrete(name = paste(names(split_by), collapse = "."))

        if (type == "histogram") {
          g <- g + labs(y = ifelse(histogram_empirical_density, "Density", "Count"))
        }

        if (type == "boxplot") {
          g <- g + scale_x_discrete(drop = boxplot_drop_unused)
        }
      }
    }
  }

  g
}
