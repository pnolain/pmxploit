#' Continuous covariates correlations
#'
#' Plots the selected continuous covariates correlations.
#'
#' @param correlation_method a character string indicating which correlation
#'   coefficient (or covariance) is to be computed. One of \code{"pearson"}
#'   (default), \code{"kendall"}, or \code{"spearman"}: can be abbreviated. If
#'   \code{type = "scatterplot"}, \code{"pearson"} method will be used.
#' @param auto_order logical. If \code{type = "heatmap"}, auto-orders the
#'   matrix with respect to the distances between values. Default is
#'   \code{TRUE}.
#' @param smoothing_method character. If \code{type = "scatterplot"}, corresponds
#'   to a \code{ggplot2} smoothing method.
#' @param fixed_ratio logical. If \code{type = "heatmap"}, plot scaled to a
#'   1:1 ratio. Default is \code{TRUE}.
#' @param split_by character. If \code{type = "scatterplot"}, categorical
#'   covariate name to colour observations by group.
#'
#' @inheritParams plot_parameters_correlations
#' @inheritParams plot_continuous_covariates_distributions
#'
#' @return A a ggplot2 object.
#' @export
#'
#' @examples
#'
#' cov <- c("AGE", "WT", "BSLDLC", "FBSPCSK", "TBSPCSK", "CLCR")
#'
#' EXAMPLERUN %>%
#'   plot_covariates_correlations(covariates = cov, type = "heatmap")+
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
#'   legend.position = "bottom")
#'
#' EXAMPLERUN %>% plot_covariates_correlations(covariates = cov, type = "heatmap", auto_order = FALSE)+
#'   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
#'   legend.position = "bottom")
#'
#' EXAMPLERUN %>%
#'   plot_covariates_correlations(covariates = c("AGE", "CLCR"), type = "scatterplot", smoothing_method = "lm")
#'
#' EXAMPLERUN %>% plot_covariates_correlations(covariates = cov, type = "scatterplot")
#' EXAMPLERUN %>% plot_covariates_correlations(covariates = cov, type = "scatterplot", smoothing_method = "lm")
#' EXAMPLERUN %>% plot_covariates_correlations(covariates = cov, type = "scatterplot", smoothing_method = "loess")
#' EXAMPLERUN %>% group_by(STUD) %>% plot_covariates_correlations(covariates = cov, type = "scatterplot")
plot_covariates_correlations <-
  function(run,
             covariates = NULL,
           baseline_only = TRUE,
             correlation_method = NULL,
             auto_order = TRUE,
             smoothing_method = NULL,
             smoothing_se = TRUE,
             type = "heatmap",
             fixed_ratio = TRUE,
             auto_legend = TRUE) {
    stopifnot(type %in% c("heatmap", "scatterplot"))

    cont_covs <-
      run$model$covariates %>%
      filter(type == "continuous")

    if (is.null(covariates)) {
      covariates <- setNames(cont_covs$column, cont_covs$name)
    } else {
      covariates <- get_selected_covariates(cont_covs, covariates)
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

    keep_cols <- c(covariates, split_by)

    df <- df %>%
      select(ID, one_of(keep_cols))

    fixed_covariates <- df %>%
      summarise_at(vars(one_of(covariates)), funs(length(unique(.)))) %>%
      gather(Parameter, N_unique) %>%
      filter(N_unique == 1)

    if (nrow(fixed_covariates) > 0) {
      if (nrow(fixed_covariates) == length(covariates)) {
        stop(simpleError("No continuous covariate have more than one unique value, correlations cannot be computed."))
      }

      removed_covs <-
        cont_covs %>%
        filter(column %in% fixed_covariates$Parameter)
      message(
        sprintf(
          "Correlations are not computed for covariate(s) with one unique value: %s\n",
          paste(removed_covs$name, collapse = ", ")
        )
      )

      covariates <- covariates[covariates %in%
        setdiff(covariates, unique(c(
          removed_covs$column, removed_covs$name
        )))]

      df <- df %>%
        select(-one_of(fixed_covariates$Parameter))
    }

    df <- df %>%
      rename(!!!setNames(covariates, names(covariates)))

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
      if (length(covariates) <= 2) {
        safe_cov1 <- as.name(names(covariates)[[1]])
        safe_cov2 <-
          as.name(names(covariates)[[length(covariates)]])
        safe_color <- NULL

        if (!is.null(names(named_group))) {
          safe_color <- as.name(names(named_group))
        }

        g <- ggplot(
          df,
          aes_string(
            x = safe_cov1,
            y = safe_cov2,
            colour = safe_color
          )
        ) +
          geom_point()
      } else {
        g <- GGally::ggscatmat(as.data.frame(select(df, -ID)))
      }
      if (!is.null(smoothing_method)) {
        g <-
          g + geom_smooth(method = smoothing_method, aes(colour = NULL), smoothing_se)
      }

      g
    } else {
      cor.matrix <-
        cor(select(df, one_of(names(covariates))), method = correlation_method, use = "pairwise.complete.obs")

      if (auto_order & ncol(cor.matrix) >= 2) {
        abs_matrix <- abs(cor.matrix)

        dd <- dist((1 - abs_matrix) / 2)
        hc <- hclust(dd)
        cor.matrix <- cor.matrix[hc$order, hc$order]
      }

      cor.matrix[lower.tri(cor.matrix)] <- NA
      corr_table <- crossing(
        covariate1 = rownames(cor.matrix),
        covariate2 = colnames(cor.matrix)
      ) %>%
        mutate(value = map2_dbl(covariate1, covariate2, ~cor.matrix[.x, .y]))
      # reshape2::melt(cor.matrix, varnames = c("covariate1", "covariate2"))

      corr_table <- corr_table %>%
        rename(correlation = value) %>%
        filter(!is.na(correlation)) %>%
        group_by(covariate2) %>%
        mutate(N = dplyr::n()) %>%
        arrange(desc(N)) %>%
        select(-N)


      corr_table$covariate2 <-
        factor(corr_table$covariate2, levels = unique(as.character(corr_table$covariate2)))

      if (all(colnames(cor.matrix) %in% rownames(cor.matrix))) {
        corr_table$covariate1 <-
          factor(corr_table$covariate1, levels = rev(levels(corr_table$covariate2)))
      } else {
        corr_table$covariate1 <-
          factor(corr_table$covariate1, levels = unique(as.character(corr_table$covariate1)))
      }

      g <-
        ggplot(corr_table, aes(covariate1, covariate2, fill = (correlation))) +
        geom_tile()

      g <- g +
        geom_text(
          aes(
            covariate1,
            covariate2,
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
