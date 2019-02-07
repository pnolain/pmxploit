#' Individual profiles (observations, predictions) versus independent variable
#'
#' @param ids integer vector of the IDs of the individuals to plot. Default is
#'   \code{NULL}, plotting all the individuals.
#' @param facetted logical. If \code{TRUE} (default), plots each individual inside a separate facet.
#' @param n_row integer. Number of rows of facets.
#' @param n_col integer. Number of columns of facets.
#' @param idv character. Name of the independent variable column to plot on the x-axis. Default is \code{"TIME"}.
#' @param predictions_dots logical. If \code{TRUE} plot predictions as dots on top of the profile lines.
#' @param show_observations logical. If \code{TRUE} plot observations.
#' @param categorical_covariate character. A categorical covariate to split the data. May be useful for covariates that vary within an individual.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% plot_individual_profiles(ids = 1:4, compartment = 2, predictions = "PRED")
#'
#' EXAMPLERUN %>% plot_individual_profiles(compartment = 2, predictions = "PRED", facetted = FALSE)
plot_individual_profiles <- function(run, ids = NULL, compartment, predictions = "PRED",
                                     idv = "TIME", log_dv = FALSE,
                                     predictions_dots = TRUE, show_observations = TRUE,
                                     categorical_covariate = NULL,
                                     x_scale = "linear", y_scale = "linear", logticks_annotation = TRUE,
                                     facetted = TRUE, facet_scales = "free", n_row = NULL, n_col = NULL,
                                     keep_time_zero = FALSE, auto_legend = TRUE) {
  if (is.null(compartment)) stop(simpleError("Compartment must be specified."))

  cmt_selection <- NULL
  dv_cmts <- run$model$compartments %>% filter(dv_target)

  if (is.null(compartment)) {
    cmt_selection <- dv_cmts
  } else {
    cmt_selection <- dv_cmts %>% filter(cmt %in% compartment)
  }

  missing_y <- predictions[!(predictions %in% colnames(run$tables$pmxploitab))]

  if (length(missing_y) > 0) {
    stop(simpleError(sprintf("Missing column(s): %s.", paste(missing_y, collapse = ", "))))
  }

  df <- run$tables$pmxploitab

  if (nrow(df) == 0 & !is.null(attr(df, "filters"))) {
    stop(simpleError("Data is empty after filtering."))
  }

  df <- df %>% filter(CMT %in% compartment & !(EVID %in% c(1, 4)))

  if (log_dv) {
    col_match <- paste(predictions, collapse = "|")

    df <- df %>% mutate_at(vars(DV, matches(col_match)), exp)
  }

  if (!is.null(ids)) {
    df <- df %>% filter(ID %in% ids)
  }

  if (!keep_time_zero) {
    df <- df %>% filter(TIME > 0)
  }

  if (nrow(df) == 0) {
    stop(simpleError(sprintf(
      "No predictions in CMT %s for subject(s) %s.",
      compartment, paste(ids, collapse = ", ")
    )))
  }

  if (!is.null(categorical_covariate)) {
    cat_covs <- run$model$covariates %>% filter(type == "categorical")

    if (categorical_covariate %in% cat_covs$column) {
      cov_col <- cat_covs %>% filter(column == categorical_covariate)

      split_col <- cov_col %>%
        mutate(matching_order = match(column, categorical_covariate)) %>%
        mutate(matching_order = ifelse(is.na(matching_order), match(name, categorical_covariate), matching_order)) %>%
        arrange(matching_order)

      categorical_covariate <- setNames(cov_col$column, nm = cov_col$name)

      for (i in seq_along(categorical_covariate)) {
        current_split <- categorical_covariate[[i]]
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

      if (categorical_covariate != names(categorical_covariate)) {
        df <- df %>% rename(!!!setNames(categorical_covariate, names(categorical_covariate)))
        categorical_covariate <- names(categorical_covariate)
      }
    } else {
      categorical_covariate <- NULL
    }
  }

  df <- df %>%
    select(ID, one_of(idv), CMT, DV, MDV, one_of(c(predictions, categorical_covariate))) %>%
    mutate(CMT = plyr::mapvalues(CMT, run$model$compartments$cmt, run$model$compartments$name, warn_missing = FALSE))

  g_df <- df %>% gather(Prediction, Value, one_of(predictions))

  reg_name <- as.name(idv)

  mapping <- aes(y = Value, colour = Prediction, group = interaction(ID, Prediction))
  mapping$x <- as.symbol(idv)

  g <- ggplot(data = g_df, mapping = mapping) +
    geom_line()

  if (predictions_dots) {
    g <- g + geom_point(size = getOption("pmxploit.indivplot.prediction.size"))
  }

  if (show_observations) {
    obs_mapping <- aes(y = DV, shape = MDV, group = ID)
    obs_mapping$x <- as.symbol(idv)

    df_obs <- df %>%
      filter(!is.na(DV)) %>%
      mutate(MDV = factor(MDV, levels = c(0, 1), ordered = TRUE))

    g <- g + geom_point(
      data = df_obs, mapping = obs_mapping, inherit.aes = FALSE,
      size = getOption("pmxploit.indivplot.dv.size")
    ) +
      scale_shape_manual(
        values = c(
          "0" = getOption("pmxploit.indivplot.dv.shape"),
          "1" = getOption("pmxploit.indivplot.mdv.shape")
        ),
        labels = c("0" = "DV", "1" = "MDV"), name = "Observation"
      )
  }

  facet_formula <- NULL

  if (length(compartment) > 1) {
    facet_formula <- ~CMT
  }

  if (facetted) {
    facet_formula <- if (is.null(facet_formula)) (~ID) else (ID ~ CMT)

    if (!is.null(categorical_covariate)) {
      if (length(compartment) == 1) {
        facet_formula[[3]] <- as.name(categorical_covariate)
      } else {
        facet_formula[[3]] <- as.formula(paste("~", paste(facet_formula[[3]], categorical_covariate, sep = "+")))
      }
    }
  }

  if (!is.null(facet_formula)) {
    g <- g + facet_wrap(facet_formula, scales = facet_scales, nrow = n_row, ncol = n_col, labeller = label_both)
  }

  if (x_scale == "log") {
    g <- g + scale_x_log10()
  }

  if (y_scale == "log") {
    g <- g + scale_y_log10()
  }

  if (any(c(x_scale, y_scale) == "log") & logticks_annotation) {
    g <- g + annotation_logticks(sides = paste0(
      ifelse(y_scale == "log", "l", ""),
      ifelse(x_scale == "log", "b", "")
    ))
  }

  if (auto_legend) {
    g <- g +
      labs(
        title = sprintf("%s - Individual profiles", paste(cmt_selection$name, collapse = "/")),
        caption = str_c("Path: ", run$info$path)
      )
  }

  g
}
