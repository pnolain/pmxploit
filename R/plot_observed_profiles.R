#' Spaghetti plot
#'
#' Plot of observed profiles versus an independent variable (e.g. TIME or TAD).
#'
#' @param idv character. Name of the column used as independent variable. Default is
#'   \code{"TIME"}.
#' @param log_dv logical. Set it to \code{TRUE} is dependent variable is in
#'   log-scale in the dataset. Default is \code{FALSE}.
#' @param mean_profiles logical. If \code{TRUE}, summarizes individual profiles
#'   by the mean profile (in each group if \code{split_by} is specified).
#' @param facetted logical. If \code{TRUE} (default) and \code{split_by}
#'   is specified, plot each group in a separate facet. Otherwise, draw groups
#'   in different colours.
#'
#' @inheritParams plot_dv_vs_predictions
#' @inheritParams plot_individual_profiles
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% plot_observed_profiles(compartment = 2)
#'
#' EXAMPLERUN %>% group_by(CMT) %>% plot_observed_profiles(compartment = 2:3)
#'
#' EXAMPLERUN %>% group_by(SEX) %>% plot_observed_profiles(compartment = 2)
#' EXAMPLERUN %>% group_by(SEX) %>% plot_observed_profiles(compartment = 2, facetted = FALSE)
#'
#' EXAMPLERUN %>% group_by(SEX) %>% plot_observed_profiles(compartment = 2, mean_profiles = TRUE)
#' EXAMPLERUN %>% group_by(SEX) %>%
#'   plot_observed_profiles(compartment = 2, mean_profiles = TRUE, facetted = FALSE)
#'
#' EXAMPLERUN %>%
#'   group_by(SEX) %>%
#'   plot_observed_profiles(compartment = 2, y_scale = "log", facetted = FALSE)
plot_observed_profiles <- function(run, compartment = NULL, idv = "TIME", ids = NULL,
                                   show_mdv = TRUE, log_dv = FALSE, mean_profiles = FALSE,
                                   x_scale = "linear", y_scale = "linear", logticks_annotation = TRUE,
                                   facetted = TRUE,
                                   facet_scales = "free", transparency = FALSE, auto_legend = TRUE) {
  if (is.null(compartment)) stop(simpleError("Compartment must be specified."))

  cmt_selection <- NULL
  dv_cmts <- run$model$compartments %>% filter(dv_target)

  if (is.null(compartment)) {
    cmt_selection <- dv_cmts
  } else {
    cmt_selection <- dv_cmts %>% filter(cmt %in% compartment)
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

  df <- df %>% filter(CMT %in% cmt_selection$cmt & !is.na(DV))

  if (log_dv) {
    df <- df %>% mutate(DV = exp(DV))
  }

  if (!is.null(ids)) {
    df <- df %>% filter(ID %in% ids)
  }


  if (nrow(df) == 0) {
    stop(simpleError(sprintf(
      "No observed data in CMT %s for selected subject(s).",
      paste(cmt_selection$cmt, collapse = ", ")
    )))
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
      df <- df %>% rename(!!!split_by)
    }
  }

  df <- df %>% select(ID, CMT, DV, MDV, one_of(c(idv, names(split_by))))

  plot_mapping <- aes_string(x = idv, y = "DV", group = "ID")

  if (length(split_by) > 0) {
    split_group <- paste(names(split_by), collapse = ".")
    split_group_name <- as.name(split_group)

    if (length(split_by) > 1) {
      df <- df %>% unite(!!split_group, names(split_by), sep = ".", remove = length(split_by) == 1)
    }

    if (!facetted) {
      plot_mapping$colour <- split_group_name
    }
  }

  dv_df <- df %>% filter(MDV == 0)
  mdv_df <- df %>% filter(MDV == 1)

  if (mean_profiles) {
    if (length(split_by) > 0) {
      grps <- map(c(idv, names(split_by)), as.name)
      df <- df %>%
        filter(MDV == 0) %>%
        group_by(!!!grps) %>%
        summarise(DV = mean(DV, na.rm = TRUE)) %>%
        unite(!!split_group, names(split_by), sep = ".", remove = length(split_by) == 1)
    } else {
      df <- df %>%
        filter(MDV == 0) %>%
        group_by(!!!as.name(idv)) %>%
        summarise(DV = mean(DV, na.rm = TRUE))
    }

    dv_df <- df

    plot_mapping$group <- NULL
  }

  g <- ggplot(data = df, mapping = plot_mapping) +
    geom_line(data = df) +
    geom_point(
      data = dv_df,
      alpha = ifelse(transparency, getOption("pmxploit.spaghettiplot.alpha"), 1),
      size = getOption("pmxploit.spaghettiplot.dv.size"),
      aes(shape = "DV")
    )

  if (!mean_profiles && nrow(mdv_df) > 0 && show_mdv) {
    g <- g + geom_point(
      data = mdv_df, aes(shape = "MDV"),
      colour = getOption("pmxploit.spaghettiplot.mdv.colour"), show.legend = FALSE
    ) +
      scale_shape_manual(values = c(
        "DV" = getOption("pmxploit.spaghettiplot.dv.shape"),
        "MDV" = getOption("pmxploit.spaghettiplot.mdv.shape")
      ), name = "Observation")
  } else {
    g <- g + guides(shape = FALSE)
  }

  if (!is.null(split_by)) {
    if (facetted) {
      safe_names <- names(split_by) %>% purrr::map(as.name)
      plot_formula <- as.formula(sprintf("~ %s", paste(safe_names, collapse = "+")))

      g <- g + facet_wrap(plot_formula, scales = facet_scales, labeller = label_both)
    }
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
    xlab <- ifelse(idv == "TIME", "Time", ifelse(idv == "TAD", "Time after dose", idv))

    g <- g +
      labs(
        title = sprintf("%s - Observed profiles", paste(cmt_selection$name, collapse = "/")),
        caption = str_c("Path: ", run$info$path)
      )
  }

  g
}
