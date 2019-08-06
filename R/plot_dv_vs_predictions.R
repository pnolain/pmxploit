#' Dependent variables vs Predictions plot
#'
#' @param run \code{pmxploit} NONMEM run object.
#' @param compartment integer. Number of the compartment of the dependent variable.
#' @param dv character. Name of the dependent variable column in the output tables.
#'   Default is \code{"DV"}.
#' @param predictions character vector. Name of the predictions column(s) in the
#'   dataset. Default is \code{"PRED"} for both population predictions and
#'   individual predictions.
#' @param log_dv logical. Set it to \code{TRUE} if the source dependent variable are in log_scale.
#' @param facetted logical. If \code{TRUE} (default), plots each "DV vs predictions" subset inside a separate facet.
#' @param split_by character vector. Name of categorical covariates used to split the
#'   data.
#' @param smoothing_method character. \code{ggplot2} smoothing method.
#' @param smoothing_se logical. Display confidence interval around smooth?
#' @param identity_line logical. Draws an identity line. Default is \code{TRUE}.
#' @param x_scale character. X-axis scale, one of \code{"linear"} or
#'   \code{"log"}. Default is \code{"linear"}.
#' @param y_scale character. Y-axis scale, one of \code{"linear"} or
#'   \code{"log"}. Default is \code{"linear"}.
#' @param logticks_annotation logical. If \code{x_scale} and/or \code{y_scale} are \code{"log"}, adds ticks annotation on the axis.
#' @param facet_scales character. \code{ggplot2} facet scales. Default is
#'   \code{"free"}.
#' @param keep_time_zero logical. If \code{TRUE}, does not ignore values at initiation (when TIME = 0).
#' Default is \code{FALSE}.
#' @param transparency logical. Plot scatterplot dots with transparency, useful
#'   to avoid overplotting with large datasets. Default is {FALSE}.
#' @param auto_legend logical. When \code{TRUE} (default), default \code{pmxploit} plotting
#'   options are applied (title, labels, axis...).
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#'
#' EXAMPLERUN %>%
#'   plot_dv_vs_predictions(compartment = 2, predictions = "PRED")
#'
#' EXAMPLERUN %>%
#'   group_by(CMT) %>%
#'   plot_dv_vs_predictions(compartment = 2:3, predictions = "PRED")
#'
#' EXAMPLERUN %>%
#'   plot_dv_vs_predictions(compartment = 2, predictions = "PRED", x_scale = "log", y_scale = "log")
#'
#' EXAMPLERUN %>%
#'   plot_dv_vs_predictions(compartment = 2, predictions = "PRED", smoothing_method = "lm")
#' EXAMPLERUN %>%
#'   plot_dv_vs_predictions(compartment = 2, predictions = "PRED", smoothing_method = "loess")
#' EXAMPLERUN %>%
#'   plot_dv_vs_predictions(compartment = 2, predictions = "PRED", identity_line = FALSE)
#' EXAMPLERUN %>%
#'   plot_dv_vs_predictions(compartment = 2, predictions = "PRED", transparency = TRUE)
#'
#' EXAMPLERUN %>%
#'   group_by(STUD) %>%
#'   plot_dv_vs_predictions(compartment = 2, predictions = "PRED")
plot_dv_vs_predictions <- function(run = NULL, compartment = NULL, dv = "DV", predictions = "PRED", log_dv = FALSE,
                                   facetted = TRUE,
                                   smoothing_method = NULL,
                                   smoothing_se = TRUE,
                                   identity_line = TRUE,
                                   x_scale = "linear", y_scale = "linear", logticks_annotation = TRUE, facet_scales = "free", transparency = FALSE,
                                   keep_time_zero = FALSE, auto_legend = TRUE) {
  stopifnot(!is.null(run))
  if (is.null(compartment)) stop(simpleError("Compartment must be specified."))

  cmt_selection <- NULL
  dv_cmts <- run$model$compartments %>% filter(dv_target)

  if (is.null(compartment)) {
    cmt_selection <- dv_cmts
  } else {
    cmt_selection <- dv_cmts %>% filter(cmt %in% compartment)
  }


  missing_pred <- predictions[!(predictions %in% colnames(run$tables$pmxploitab))]
  missing_dv <- dv[!(dv %in% colnames(run$tables$pmxploitab))]

  if (length(missing_dv) > 0) {
    stop(simpleError(sprintf("Missing column(s): %s.", paste(missing_dv, collapse = ", "))))
  }
  if (length(missing_pred) > 0) {
    stop(simpleError(sprintf("Missing column(s): %s.", paste(missing_pred, collapse = ", "))))
  }

  df <- run$tables$pmxploitab

  split_by <- NULL
  if (!is.null(groups(df))) {
    split_by <- as.character(groups(df))
    df <- ungroup(df)
  }

  df <- df %>%
    filter(CMT %in% cmt_selection$cmt & !is.na(.[[dv]]) & MDV == 0)

  if (log_dv) {
    col_match <- paste(predictions, collapse = "|")

    df <- df %>% mutate_at(vars(DV, matches(col_match)), exp)
  }

  if (!keep_time_zero) {
    df <- df %>% filter(TIME > 0)
  }

  if (nrow(df) == 0) {
    stop(simpleError(sprintf("No prediction in CMT %s.", paste(cmt_selection$cmt, collapse = ", "))))
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
  }

  g_df <- df %>%
    select(ID, TIME, CMT, one_of(c(dv, predictions, names(split_by)))) %>%
    gather(Y, Y_Value, one_of(predictions), factor_key = TRUE) %>%
    gather(X, X_Value, one_of(dv), factor_key = TRUE)

  mapping <- aes(x = X_Value, y = Y_Value)

  one_plot <- (length(predictions) == 1 & length(split_by) == 0)

  if (!facetted & length(predictions) > 1) {
    mapping$colour <- quote(Y)
  }

  g <- ggplot(data = g_df, mapping = mapping)

  g <- g +
    geom_point(alpha = ifelse(transparency, getOption("pmxploit.dvvspred.dv.alpha"), 1))

  if (!facetted & length(predictions) > 1) {
    g <- g + scale_colour_discrete(name = "Predictions")
  }

  if (!one_plot & (facetted | !is.null(split_by))) {
    plot_formula <- X ~ Y

    if (!is.null(split_by)) {
      base_formula <- ifelse(facetted, "X ~ Y + %s", "X ~ %s")
      plot_formula <- as.formula(sprintf(base_formula, paste(sprintf("`%s`", names(split_by)), collapse = "+")))
    }

    g <- g + facet_wrap(plot_formula, scales = facet_scales, labeller = label_both)
  }

  if (identity_line) {
    if (facet_scales == "free" | !facetted) {
      grps <- map(names(split_by), as.name)

      groups <- g_df %>% group_by(!!!grps) %>% summarise() %>% ungroup()

      if (facetted) {
        facets <- crossing(predictions, dv, groups)
      } else {
        facets <- crossing(dv, groups)
      }

      facets <- facets %>%
        mutate(row = row_number())


      funcs <- facets %>%
        split(.$row) %>%
        map(function(x) {
          t_df <- g_df

          if (length(split_by) > 0) {
            t_df <- g_df %>% semi_join(x, by = names(split_by))
          }

          if (facetted) {
            t_df <- t_df %>% filter(Y == x$predictions, X == x$dv)
          }

          all_values <- c(t_df$X_Value, t_df$Y_Value)

          if(x_scale == "log")
            all_values <- all_values[all_values > 0]

          xlim_line <- c(
            min(all_values),# + 0.0000001,
            max(all_values) * 1.01
          )

          if (x_scale == "log")
            xlim_line <- log10(xlim_line)

          stat_function(
            data = t_df, fun = function(x) x,
            xlim = xlim_line,
            inherit.aes = FALSE,
            linetype = getOption("pmxploit.dvvspred.identityline.linetype"),
            colour = getOption("pmxploit.dvvspred.identityline.colour")
          )
        })

      g <- g + funcs


      # xlim_line <- c(min(c(g_df$X_Value, g_df$Y_Value)),
      #                max(c(g_df$X_Value, g_df$Y_Value)) * 1.01)
      #
      # if(x_scale == "log")
      #   xlim_line <- log10(xlim_line)
      # g<- g+stat_function(fun = function(x) x,
      #                     xlim = xlim_line,
      #                     inherit.aes = FALSE,
      #                     linetype = getOption("pmxploit.dvvspred.identityline.linetype"),
      #                     colour = getOption("pmxploit.dvvspred.identityline.colour"))
    } else {
      g <- g + geom_abline(
        intercept = 0, slope = 1,
        linetype = getOption("pmxploit.dvvspred.identityline.linetype"),
        colour = getOption("pmxploit.dvvspred.identityline.colour")
      )
    }
  }

  if (!is.null(smoothing_method)) {
    g <- g + geom_smooth(
      method = smoothing_method, fullrange = TRUE,
      size = getOption("pmxploit.dvvspred.smooth.size"),
      colour = getOption("pmxploit.dvvspred.smooth.colour"),
      se = smoothing_se
    )

    if (!is.null(split_by)) {
      exp_df <- g_df %>%
        group_by_at(vars(CMT, Y, one_of(names(split_by))))
    } else {
      exp_df <- g_df %>%
        group_by(CMT, Y)
    }

    exp_df <- exp_df %>%
      summarise(
        min_val = min(Y_Value),
        need_expansion_low = min_val < min(X_Value),
        max_val = max(Y_Value),
        need_expansion_up = max_val > max(X_Value)
      ) %>%
      ungroup() %>%
      filter(need_expansion_low | need_expansion_up)

    exp_up <- exp_df %>%
      filter(need_expansion_up) %>%
      select(-min_val, -need_expansion_low, -need_expansion_up) %>%
      rename(X_Value = max_val)
    exp_low <- exp_df %>%
      filter(need_expansion_low) %>%
      select(-need_expansion_low, -max_val, -need_expansion_up) %>%
      rename(X_Value = min_val)

    expansion_pts <- bind_rows(exp_up, exp_low) %>%
      mutate(X = as.factor(dv))

    g$data <- g_df %>%
      bind_rows(expansion_pts)
  }


  gg_x_scale <- ifelse(x_scale == "log", scale_x_log10, scale_x_continuous)
  gg_y_scale <- ifelse(y_scale == "log", scale_y_log10, scale_y_continuous)

  if (any(c(x_scale, y_scale) == "log") & logticks_annotation) {
    g <- g + annotation_logticks(sides = paste0(
      ifelse(y_scale == "log", "l", ""),
      ifelse(x_scale == "log", "b", "")
    ))
  }

  if (auto_legend) {
    g <- g + labs(
      title = sprintf(
        "%s (%s vs %s)", paste(cmt_selection$name, collapse = "/"),
        paste(dv, collapse = "/"), paste(predictions, collapse = "/")
      ),
      caption = str_c("Path: ", run$info$path)
    )

    one_unit <- (length(unique(cmt_selection$unit)) == 1)
    if (!one_unit) {
      g <- g + labs(x = "Dependent variables", y = "Predictions")
    } else {
      g <- g + labs(
        x = str_c(dv, str_replace_na(str_c(" [", cmt_selection$unit, "]"), "")),
        y = str_c(predictions, str_replace_na(str_c(" [", cmt_selection$unit, "]"), ""))
      )
    }

    g <- g +
      gg_x_scale(expand = c(0, 0)) +
      gg_y_scale(expand = c(0, 0))
  }

  g
}
