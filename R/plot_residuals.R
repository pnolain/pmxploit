#' Residuals plot
#'
#' Plot residuals (e.g. \code{CWRES}, \code{IWRES}, \code{NPDE}) as either a
#' scatterplot (versus an independent variable), an histogram (distribution) or a quantile-quantile plot.
#'
#' @param residuals character vector of column names of residuals.
#' @param compartment integer vector of the numbers of the compartments of residuals.
#' @param reference_value numeric. Draws reference lines at +/- a particular
#'   value.
#' @param absolute_residuals logical. If \code{TRUE} residuals are transformed
#'   to absolute values.
#' @param type character. Type of representation of the residuals. One of
#'   \code{scatterplot}, \code{histogram} and \code{qq} (Q-Q plot of a distribution of reference).
#'   Default is \code{scatterplot}.
#' @param histogram_bins integer. If \code{type = "histogram"}, sets the number
#'   of bins. Default is 30.
#' @param histogram_empirical_density logical. If \code{TRUE}, draws the
#'   empirical density over the histogram.
#' @param histogram_reference_distribution logical. If \code{TRUE}, draws a
#'   theoretical density over the histogram.
#'
#' @inheritParams plot_dv_vs_predictions
#' @inheritParams plot_observed_profiles
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>%
#'   plot_residuals(compartment = 2, idv = c("TIME", "PRED"), residuals = "CWRES")
#'
#' EXAMPLERUN %>%
#'   group_by(CMT) %>%
#'   plot_residuals(compartment = 2:3, idv = c("TIME", "PRED"), residuals = "CWRES")
#'
#' EXAMPLERUN %>%
#'   plot_residuals(compartment = 2, idv = c("TIME", "PRED"),
#'                  residuals = "CWRES", smoothing_method = "lm")
#'
#' EXAMPLERUN %>%
#'   plot_residuals(compartment = 2, idv = c("TIME", "PRED"),
#'                  residuals = "CWRES", type = "histogram")
#' EXAMPLERUN %>%
#'   plot_residuals(compartment = 2, idv = c("TIME", "PRED"),
#'                  residuals = "CWRES", type = "qq")
#'
#' EXAMPLERUN %>%
#'   group_by(STUD) %>%
#'   plot_residuals(compartment = 2, idv = c("TIME", "PRED"),
#'                  residuals = "CWRES", type = "qq")
plot_residuals <- function(run,
                           compartment = NULL,
                           idv = "TIME",
                           residuals,
                           absolute_residuals = FALSE,
                           keep_time_zero = FALSE,
                           type = "scatterplot",
                           smoothing_method = NULL,
                           smoothing_se = TRUE,
                           reference_value = NULL,
                           histogram_bins = 30L,
                           histogram_empirical_density = TRUE,
                           histogram_reference_distribution = list(fun = dnorm, args = list(mean = 0, sd = 1)),
                           qq_reference_distribution = list(fun = qnorm, args = list(mean = 0, sd = 1)),
                           x_scale = "linear",
                           y_scale = "linear",
                           facet_scales = "free",
                           transparency = FALSE,
                           auto_legend = TRUE) {
  if (is.null(compartment)) stop(simpleError("Compartment must be specified."))

  cmt_selection <- NULL
  dv_cmts <- run$model$compartments %>% filter(dv_target)

  if (is.null(compartment)) {
    cmt_selection <- dv_cmts
  } else {
    cmt_selection <- dv_cmts %>% filter(cmt %in% compartment)
  }

  missing_residuals <- residuals[!(residuals %in% colnames(run$tables$pmxploitab))]
  missing_idv <- idv[!(idv %in% colnames(run$tables$pmxploitab))]

  if (length(missing_idv) > 0) {
    stop(simpleError(sprintf("Missing column(s): %s.", paste(missing_idv, collapse = ", "))))
  }
  if (length(missing_residuals) > 0) {
    stop(simpleError(sprintf("Missing column(s): %s.", paste(missing_residuals, collapse = ", "))))
  }

  df <- run$tables$pmxploitab

  if (nrow(df) == 0 & !is.null(attr(df, "filters"))) {
    stop(simpleError("Data is empty after filtering."))
  }

  split_by <- NULL
  if (!is.null(groups(df))) {
    split_by <- as.character(groups(df))
    df <- ungroup(df)
  }

  df <- df %>%
    filter(CMT %in% cmt_selection$cmt & MDV == 0)

  if (!keep_time_zero) {
    df <- df %>% filter(TIME > 0)
  }

  if (nrow(df) == 0) stop(simpleError(sprintf("No predictions in CMT %s.", paste(cmt_selection$cmt, collapse = ", "))))

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
  }

  g_df <- df %>%
    select(ID, TIME, CMT, one_of(c(idv, residuals, names(split_by)))) %>%
    gather(Residuals, Residuals_Value, one_of(residuals), factor_key = TRUE) %>%
    gather(idv, idv_Value, one_of(idv), factor_key = TRUE)

  mapping <- aes(x = idv_Value, y = Residuals_Value)

  # if(nrow(cmt_selection) > 1){
  #   mapping$colour <- quote(CMT)
  # }

  one_plot <- (length(idv) == 1 & length(residuals) == 1 & length(split_by) == 0)

  corrected_split_names <- sprintf("`%s`", names(split_by))

  g <- ggplot(data = g_df, mapping = mapping)

  if (type == "scatterplot") {
    if (absolute_residuals) {
      g$data$Residuals_Value <- abs(g_df$Residuals_Value)
    }

    g <- g +
      geom_point(alpha = ifelse(transparency, getOption("pmxploit.residualsplot.alpha"), 1)) +
      geom_hline(yintercept = 0)


    if (!is.null(smoothing_method)) {
      g <- g + geom_smooth(
        method = smoothing_method,
        size = getOption("pmxploit.residualsplot.smooth.size"),
        se = smoothing_se
      )
    }

    if (!one_plot) {
      plot_formula <- idv ~ Residuals

      if (!is.null(split_by)) {
        plot_formula <- as.formula(sprintf("idv ~ Residuals + %s", paste(corrected_split_names, collapse = "+")))
      }

      g <- g + facet_wrap(plot_formula, scales = facet_scales, labeller = label_both)
    }

    if (!is.null(reference_value)) {
      ref_lines <- data.frame(value = c(-reference_value, reference_value))

      if (absolute_residuals) {
        ref_lines <- ref_lines %>% slice(2)
      }

      g <- g + geom_hline(
        data = ref_lines,
        aes(yintercept = value), linetype = "dashed"
      )
    }
  } else if (type == "qq") {
    if (is.null(qq_reference_distribution)) {
      stop(simpleError("Reference distribution for the Q-Q plot must be specified."))
    }

    g_df <- g_df %>%
      select(-idv, -idv_Value) %>%
      slice(1:(n() / length(idv)))

    if (!is.null(split_by)) {
      grps <- map(c("Residuals", names(split_by)), as.name)

      intsl <- g_df %>% group_by(!!!grps)
    } else {
      intsl <- g_df %>% group_by(Residuals)
    }

    qq_dist <- qq_reference_distribution

    # compute theoretical qq int/slope
    intsl <- intsl %>% summarise(
      q25 = quantile(Residuals_Value, 0.25, na.rm = TRUE),
      q75 = quantile(Residuals_Value, 0.75, na.rm = TRUE),
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
      int = q25 - slope * theo25, n = n()
    )

    if (!is.null(split_by)) {
      wrap_formula <- as.formula(sprintf("Residuals ~ %s", paste(corrected_split_names, collapse = "+")))

      g <- ggplot(g_df) +
        geom_qq(aes(sample = Residuals_Value), distribution = qq_dist$fun) +
        geom_abline(data = intsl, aes_string(intercept = "int", slope = "slope"), colour = "blue") +
        facet_wrap(wrap_formula, scales = facet_scales, labeller = label_both)
    } else {
      g <- ggplot(g_df) +
        geom_qq(aes(sample = Residuals_Value), distribution = qq_dist$fun) +
        geom_abline(data = intsl, aes(intercept = int, slope = slope), colour = "blue") +
        facet_wrap(~Residuals, scales = facet_scales, labeller = label_both)
    }
  } else if (type == "histogram") {
    g_df <- g_df %>%
      select(-idv, -idv_Value) %>%
      slice(1:(n() / length(idv)))

    if (!is.null(split_by)) {
      grps <- map(c("Residuals", names(split_by)), as.name)
      limit_values <- g_df %>% group_by(!!!grps)
    } else {
      limit_values <- g_df %>% group_by(Residuals)
    }

    limit_values <- limit_values %>%
      summarise(
        pos_value = ceiling(max(abs(Residuals_Value), na.rm = TRUE)),
        neg_value = -pos_value
      ) %>%
      gather(limit, x, pos_value, neg_value)

    g <- ggplot(g_df, aes(x = Residuals_Value)) +
      geom_histogram(aes(y = ..density..), bins = histogram_bins)

    if (!is.null(split_by)) {
      wrap_formula <- as.formula(sprintf("Residuals ~ %s", paste(corrected_split_names, collapse = "+")))

      g <- g + facet_wrap(wrap_formula, scales = facet_scales, labeller = label_both)
    } else {
      g <- g + facet_wrap(~Residuals, scales = facet_scales)
    }

    if (histogram_empirical_density) {
      g <- g + stat_density(geom = "line", mapping = aes(colour = "Empirical", linetype = "Empirical"))
    }

    if (!is.null(histogram_reference_distribution) &&
      all(c("fun", "args") %in% names(histogram_reference_distribution))) {
      g <- g + stat_function(
        data = limit_values,
        aes(x = x, colour = "Reference", linetype = "Reference"),
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

  if (x_scale == "log") {
    g <- g + scale_x_log10()
  }

  if (y_scale == "log") {
    g <- g + scale_y_log10()
  }

  if (auto_legend) {
    g <- g + labs(caption = str_c("Path: ", run$info$path))

    if (type == "scatterplot") {
      g <- g +
        labs(title = sprintf(
          "%s (%s vs %s)", paste(cmt_selection$name, collapse = "/"),
          paste(residuals, collapse = "/"), paste(idv, collapse = "/")
        )) +
        scale_color_discrete(
          breaks = cmt_selection$cmt,
          labels = cmt_selection$name,
          name = "Compartment"
        )
    } else {
      g <- g +
        labs(title = sprintf(
          "%s (%s)", paste(cmt_selection$name, collapse = "/"),
          paste(residuals, collapse = "/")
        ))
    }

    y_lab <- ifelse(length(residuals) == 1, residuals, "Residuals")
    x_lab <- NULL

    if(one_plot) # Remove facetting
      g <- g + facet_null()

    if(type == "qq"){
      g <- g+labs(x = "Reference distribution quantiles", y = y_lab)
    } else if (one_plot) {
      g <- g+labs(x = idv,
             y = ifelse(absolute_residuals, sprintf("|%s|", residuals), residuals))
    } else {
      if (type == "scatterplot")
        x_lab <- ifelse(length(idv) == 1, idv, "Indepedent variable")

      g <- g + labs(
        x = x_lab,
        y = ifelse(absolute_residuals, sprintf("|%s|", y_lab), y_lab)
      )
    }
  }

  g
}
