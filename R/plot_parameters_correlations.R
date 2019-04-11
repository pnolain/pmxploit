#' Plot parameters correlations
#'
#' Returns selected parameters correlations in the form of either a matrix, a
#' data frame or a plot.
#'
#' @param parameters character vector of parameters names. Default is \code{NULL},
#'   returning all individual parameters (random and post-hoc).#'
#' @param type character. Type of plot. One of \code{heatmap} or \code{scatterplot}.
#' @param smoothing_method character. If \code{type = "scatterplot"}, corresponds
#'   to a \code{ggplot2} smoothing method.
#' @param fixed_ratio logical. If \code{type = "heatmap"}, plot scaled to a
#'   1:1 ratio. Default is \code{TRUE}.
#'
#' @inheritParams summarize_covariates_correlations
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% plot_parameters_correlations()
#'
#' EXAMPLERUN %>% plot_parameters_correlations(type = "heatmap")
#' EXAMPLERUN %>% plot_parameters_correlations(type = "heatmap", auto_order = FALSE)
#'
#' EXAMPLERUN %>% plot_parameters_correlations(type = "scatterplot")
#' EXAMPLERUN %>% plot_parameters_correlations(type = "scatterplot", smoothing_method = "lm")
#' EXAMPLERUN %>% plot_parameters_correlations(type = "scatterplot", smoothing_method = "loess")
#' EXAMPLERUN %>% group_by(STUD) %>% plot_parameters_correlations(type = "scatterplot")
plot_parameters_correlations <-
  function(run,
             parameters = NULL,
             type = "heatmap",
             correlation_method = NULL,
           baseline_only = TRUE,
             auto_order = TRUE,
             smoothing_method = NULL,
             smoothing_se = TRUE,
             fixed_ratio = TRUE,
             auto_legend = TRUE) {
    stopifnot(type %in% c("heatmap", "scatterplot"))

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

    keep_cols <- c(parameters, split_by)

    df <- df %>%
      select(ID, one_of(keep_cols))

    fixed_parameters <- df %>%
      summarise_at(vars(one_of(parameters)), funs(length(unique(.)))) %>%
      gather(Parameter, N_unique) %>%
      filter(N_unique == 1)

    if (nrow(fixed_parameters) > 0) {
      if (nrow(fixed_parameters) == length(parameters)) {
        stop(simpleError("No parameter have more than one unique value, correlations cannot be computed."))
      }

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

    if (type == "scatterplot") {
      if (length(parameters) <= 2) {
        safe_p1 <- as.name(names(parameters)[[1]])
        safe_p2 <- as.name(names(parameters)[[length(parameters)]])
        safe_color <- NULL

        if (!is.null(names(named_group))) {
          safe_color <- as.name(names(named_group))
        }

        g <- ggplot(df, aes_string(
          x = safe_p1,
          y = safe_p2,
          colour = safe_color
        )) +
          geom_point()
      } else {
        g <- GGally::ggscatmat(as.data.frame(select(df, -ID)))
      }

      if (!is.null(smoothing_method)) {
        g <-
          g + geom_smooth(method = smoothing_method, aes(colour = NULL), se = smoothing_se)
      }

      g
    } else {
      cor.matrix <-
        cor(select(df, one_of(names(parameters))), method = correlation_method, use = "pairwise.complete.obs")

      if (auto_order & ncol(cor.matrix) >= 2) {
        abs_matrix <- abs(cor.matrix)

        dd <- dist((1 - abs_matrix) / 2)
        hc <- hclust(dd)
        cor.matrix <- cor.matrix[hc$order, hc$order]
      }

      cor.matrix[lower.tri(cor.matrix)] <- NA
      # corr_table <-
      #   reshape2::melt(cor.matrix, varnames = c("parameter1", "parameter2"))

      corr_table <- crossing(
        parameter1 = rownames(cor.matrix),
        parameter2 = colnames(cor.matrix)
      ) %>%
        mutate(value = map2_dbl(parameter1, parameter2, ~cor.matrix[.x, .y]))


      corr_table <- corr_table %>%
        rename(correlation = value) %>%
        filter(!is.na(correlation)) %>%
        group_by(parameter2) %>%
        mutate(N = dplyr::n()) %>%
        ungroup() %>%
        arrange(desc(N)) %>%
        select(-N)

      corr_table$parameter2 <-
        factor(corr_table$parameter2, levels = unique(as.character(corr_table$parameter2)))

      if (all(colnames(cor.matrix) %in% rownames(cor.matrix))) {
        corr_table$parameter1 <-
          factor(corr_table$parameter1, levels = rev(levels(corr_table$parameter2)))
      } else {
        corr_table$parameter1 <-
          factor(corr_table$parameter1, levels = unique(as.character(corr_table$parameter1)))
      }

      g <-
        ggplot(corr_table, aes(parameter1, parameter2, fill = (correlation))) +
        geom_tile()

      g <- g +
        geom_text(
          aes(
            parameter1,
            parameter2,
            label = round(correlation, digits = getOption("pmxploit.correlationplot.digits"))
          ),
          color = getOption("pmxploit.correlationplot.text_color"),
          size = 4
        ) +
        scale_fill_gradient2(
          name = bquote(rho ~ plain("coefficient")),
          # sprintf("%s coefficient", correlation_method)
          low = getOption("pmxploit.correlationplot.dark_color2"),
          mid = "white",
          high = getOption("pmxploit.correlationplot.dark_color")
        )

      if (fixed_ratio) {
        g <- g + coord_fixed()
      }

      if (auto_legend) {
        g <- g +
          guides(fill = guide_colorbar(barwidth = getOption(
            "pmxploit.correlationplot.bandwidth"
          ))) +
          labs(x = NULL, y = NULL, caption = str_c("Path: ", run$info$path))
      }

      g
    }
  }
