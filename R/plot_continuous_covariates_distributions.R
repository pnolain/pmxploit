
#' Continuous covariates distributions
#'
#' Plots the distributions of the selected continuous covariates.
#'
#' @param type character. Type of distribution representation. One of
#'   \code{histogram}, \code{density}, \code{boxplot} and \code{qq} (Q-Q plot of
#'   the Normal distribution). Default is \code{histogram}.
#' @param covariates character vector of continous covariates names. Default is
#'   \code{NULL}, returning all continuous covariates.
#' @param split_by character vector. Names of categorical covariates used to
#'   split the distributions.
#' @param histogram_bins integer. If \code{type = "histogram"}, sets the number
#'   of bins. Default is 30.
#' @param boxplot_facets logical. If \code{type = "boxplot"}, represent
#'   boxplots in different facets when \code{TRUE}.
#' @param boxplot_drop_unused logical. If \code{type = "boxplot"}, drops unused
#'   factor levels when \code{TRUE}.
#' @param facet_scales character. \code{ggplot2} facet scales. Default is
#'   \code{"free"}.
#' @param overlay_splits logical. If splitting is applied, \code{TRUE} will
#'   overlay splits on a same graph while \code{FALSE} will plot them on
#'   different facets. Default is \code{TRUE}.
#' @inheritParams plot_categorical_covariates_distributions
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% plot_continuous_covariates_distributions()
#' EXAMPLERUN %>% plot_continuous_covariates_distributions(type = "density")
#' EXAMPLERUN %>% plot_continuous_covariates_distributions(type = "boxplot")
#' EXAMPLERUN %>% plot_continuous_covariates_distributions(type = "qq")
#'
#' EXAMPLERUN %>%
#'   group_by(SEX) %>%
#'   plot_continuous_covariates_distributions(type = "boxplot", covariates = c("AGE", "WT"))
#'
#' EXAMPLERUN %>%
#'   group_by(SEX, STATIN) %>%
#'   plot_continuous_covariates_distributions(type = "boxplot", covariates = c("AGE", "WT"))
#'
#' EXAMPLERUN %>%
#'   group_by(SEX) %>%
#'   plot_continuous_covariates_distributions(type = "histogram", covariates = c("AGE", "WT"))
#'
#' EXAMPLERUN %>%
#'   group_by(SEX) %>%
#'   plot_continuous_covariates_distributions(type = "histogram",
#'                                            covariates = c("AGE", "WT"),
#'                                            overlay_splits = FALSE)
#'
#' EXAMPLERUN %>%
#'   group_by(SEX) %>%
#'   plot_continuous_covariates_distributions(type = "density")
plot_continuous_covariates_distributions <- function(run, covariates = NULL,
                                                     baseline_only = TRUE,
                                                     type = "histogram",
                                                     histogram_bins = 30L,
                                                     boxplot_facets = TRUE,
                                                     boxplot_drop_unused = FALSE,
                                                     qq_reference_distribution = qnorm,
                                                     facet_scales = "free",
                                                     overlay_splits = TRUE,
                                                     auto_legend = TRUE) {
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

  if (!is.null(split_by)) {
    wrap_formula <- (if (length(split_by) == 1) {
      as.formula(~covariate)
    } else {
      safe_names <- names(split_by[-1]) %>% purrr::map(as.name)
      as.formula(sprintf("covariate ~ %s", paste(safe_names, collapse = "+")))
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

        g <- ggplot(g_df, aes_string(x = "covariate", y = "value", fill = as.name(grouping_key))) +
          geom_boxplot()
      } else {
        g <- ggplot(g_df, aes_string(x = corrected_split_names[1], y = "value", fill = corrected_split_names[1])) +
          geom_boxplot() +
          facet_wrap(wrap_formula, scales = facet_scales)
      }
    } else {
      g <- ggplot(g_df, aes(x = covariate, y = value)) +
        geom_boxplot()

      if (boxplot_facets) {
        g <- g + facet_wrap(~covariate, scales = facet_scales)
      }
    }
  } else if (type == "qq") {
    qq_dist <- qq_reference_distribution

    if (!is.null(split_by)) {
      grps <- map(c("covariate", names(split_by)), as.name)
      intsl <- g_df %>% group_by(!!!grps)
    } else {
      intsl <- g_df %>% group_by(covariate)
    }

    # compute theoretical qq int/slope
    intsl <- intsl %>% summarise(
      q25 = quantile(value, 0.25),
      q75 = quantile(value, 0.75),
      theo25 = qq_dist(0.25),
      theo75 = qq_dist(0.75),
      slope = (q25 - q75) / (theo25 - theo75),
      int = q25 - slope * theo25, n = n()
    )


    if (!is.null(split_by)) {
      if (overlay_splits) {
        g <- ggplot(g_df) +
          geom_qq(aes_string(sample = "value", colour = corrected_split_names[1]), distribution = qq_dist) +
          geom_abline(
            data = intsl,
            aes_string(intercept = "int", slope = "slope", colour = corrected_split_names[1])
          ) +
          facet_wrap(wrap_formula, scales = facet_scales)
      } else {
        wrap_formula <- as.formula(sprintf("covariate ~ %s", paste(corrected_split_names[1], collapse = "+")))

        g <- ggplot(g_df) +
          geom_qq(aes(sample = value), distribution = qq_dist) +
          geom_abline(data = intsl, aes(intercept = int, slope = slope), colour = "blue") +
          facet_wrap(wrap_formula, scales = facet_scales)
      }
    } else {
      g <- ggplot(g_df) +
        geom_qq(aes(sample = value), distribution = qq_dist) +
        geom_abline(data = intsl, aes(intercept = int, slope = slope), colour = "blue") +
        facet_wrap(~covariate, scales = facet_scales)
    }
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
        wrap_formula <- as.formula(sprintf("covariate ~ %s", paste(corrected_split_names[1], collapse = "+")))

        g <- ggplot(g_df, aes(x = value)) +
          dist_geom +
          facet_wrap(wrap_formula, scales = facet_scales)
      }
    } else {
      g <- ggplot(g_df, aes(x = value)) +
        dist_geom +
        facet_wrap(~covariate, scales = facet_scales)
    }
  }

  if (auto_legend) {
    g <- g + labs(caption = str_c("Path: ", run$info$path))

    if (!is.null(split_by)) {
      if (type == "qq") {
        g <- g + scale_colour_discrete(name = paste(names(split_by), collapse = "."))
      } else {
        g <- g + scale_fill_discrete(name = paste(names(split_by), collapse = "."))

        if (type == "boxplot") {
          g <- g + scale_x_discrete(drop = boxplot_drop_unused)
        }
      }
    }
  }

  g
}
