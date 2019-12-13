#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr group_by
#' @export
dplyr::group_by


#' @importFrom dplyr ungroup
#' @export
dplyr::ungroup

#' @importFrom dplyr slice
#' @export
dplyr::slice



#' @importFrom dplyr arrange between bind_cols bind_rows distinct everything filter first funs full_join group_by group_by_at groups inner_join lag last lead left_join matches mutate mutate_at one_of pull  rename right_join row_number select semi_join slice summarise summarise_all summarise_at tbl_df ungroup vars
#' @importFrom stringr boundary fixed invert_match str_c str_detect str_extract str_extract_all str_locate str_locate_all str_match str_match_all str_pad str_remove str_remove_all str_replace str_replace_all str_replace_na str_split str_sub str_subset str_trim str_which word
#' @importFrom tidyr complete crossing extract fill gather nesting separate spread unite nest unnest
#' @importFrom readr parse_number read_delim read_file read_lines read_table write_lines
#' @importFrom purrr compact flatten keep quietly safely map map_chr map_df map_dbl map_lgl map_int map2 map2_chr map2_dbl modify modify_if pmap pmap_chr update_list
#' @importFrom tibble add_column add_row as_tibble tibble tribble
#' @importFrom lubridate interval seconds ymd_hms
#' @importFrom rlang caller_env as_quosure parse_quo quo quos set_names sym syms !! !!!
#' @import XML ggplot2
NULL

#' Recode categorical covariates levels
#'
#' Change the name of categorical covariates levels.
#'
#' @param covariates_levels list of categorical covariates, each list item
#'   contains the corresponding categorical levels.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A NONMEM run object.
#' @export
#'
#' @examples
#' recoded_EXAMPLERUN <-
#'  recode_categorical_covariates(run = EXAMPLERUN,
#'                                covariates_levels =
#'                                     list(STUD = list(`Study 1` = "1",
#'                                                       `Study 2` = "2",
#'                                                       `Study 3` = "3")))
#'
#' # before
#' EXAMPLERUN %>%
#'   group_by(STUD) %>%
#'   plot_parameters_distributions(type = "boxplot", parameters = "CL")
#' # after
#' recoded_EXAMPLERUN %>%
#'   group_by(STUD) %>%
#'   plot_parameters_distributions(type = "boxplot", parameters = "CL")
recode_categorical_covariates <- function(run, covariates_levels = NULL) {
  missing_column <- names(covariates_levels)[!(names(covariates_levels) %in% colnames(run$tables$pmxploitab))]

  if (length(missing_column) > 0) {
    stop(simpleError(sprintf("Missing column(s): %s.", missing_column)))
  }

  for (i in seq_along(covariates_levels)) {
    column <- names(covariates_levels)[i]
    levels <- covariates_levels[[i]]

    updated_levels <- run$model$categorical_covariates_levels[[column]] %>% unname()

    names(run$model$categorical_covariates_levels[[column]]) <- plyr::mapvalues(updated_levels, from = levels, to = names(levels))
  }

  run
}

#' Switch covariate type (categorical <-> continuous)
#'
#' Convert categorical covariates to continuous covariates and vice versa.
#'
#' @param covariates character vector of covariates names.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A NONMEM run object.
#' @export
#'
#' @examples
#' switched_EXAMPLERUN <- switch_covariates_type(run = EXAMPLERUN, covariates = c("WT", "STUD"))
#'
#' EXAMPLERUN %>% plot_continuous_covariates_distributions()
#' switched_EXAMPLERUN %>% plot_continuous_covariates_distributions()
#'
#' EXAMPLERUN %>% plot_categorical_covariates_distributions()
switch_covariates_type <- function(run, covariates = NULL) {
  col_ids <- with(run$model$covariates, {
    which((name %in% covariates) | (column %in% covariates))
  })

  if (length(col_ids) == 0) {
    stop(simpleError(sprintf("%s not found in continuous covariates.", paste(covariates, collapse = ", "))))
  }


  for (i in col_ids) {
    cov <- run$model$covariates[i, ]

    values <- run$tables$pmxploitab[[cov$column]]

    if (cov$type == "continuous") {
      cat_levels <- unique(values)
      run$tables$pmxploitab[[cov$column]] <- factor(values, levels = sort(cat_levels), ordered = TRUE)

      run$model$categorical_covariates_levels[[cov$column]] <- setNames(as.character(cat_levels), cat_levels)

      run$model$covariates[i, ]$type <- "categorical"
    } else if (cov$type == "categorical") {

      # if factor levels are numeric -> convert factor to numeric
      # else -> convert factor to integer level
      as_num_values <- values %>% as.character() %>% as.numeric()

      all_numerics <- all(!is.na(as_num_values))

      if (all(!is.na(as_num_values))) {
        values <- as_num_values
      } else {
        as.integer(values)
      }

      run$tables$pmxploitab[[cov$column]] <- values
      run$model$categorical_covariates_levels[[cov$column]] <- NULL
      run$model$covariates[i, ]$type <- "continuous"
    }
  }

  run$model$covariates <- run$model$covariates %>%
    arrange(type, name)

  invisible(run)
}
#' Rename model compartments
#'
#' @param compartments named integer vector with new compartments names as items names and
#'   old compartments number as values.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A NONMEM run object.
#' @export
#'
#' @examples
#' renamed_EXAMPLERUN <- EXAMPLERUN %>%
#'                         rename_compartments(compartments = c("Alirocumab (nM)" = 2,
#'                                                              "PCSK9 (nM)" = 3))
#'
#' EXAMPLERUN %>% plot_dv_vs_predictions(compartment = 2:3)
#' renamed_EXAMPLERUN %>% plot_dv_vs_predictions(compartment = 2:3)
rename_compartments <- function(run, compartments = NULL) {
  missing_cmt <- compartments[!compartments %in% run$model$compartments$cmt]

  if (length(missing_cmt) > 0) {
    stop(simpleError(sprintf("Missing compartment(s): %s.", paste(missing_cmt, collapse = ", "))))
  }

  df <- run$model$compartments

  for (i in 1:nrow(df)) {
    if (df[i, ]$cmt %in% compartments) {
      df[i, ]$name <- names(compartments[compartments == df[i, ]$cmt])
    }
  }

  run$model$compartments <- df

  invisible(run)
}

#' Set compartment units
#'
#' @param units
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A NONMEM run object.
#' @export
#'
#' @examples
#'
#' EXAMPLERUN %>%
#'  set_compartments_units(units = c("2" = "nM", "3" = "nM"))
set_compartments_units <- function(run, units = NULL) {
  compartments <- names(units)
  new_units <- unname(units)

  stopifnot(!is.null(compartments))

  missing_cmt <- compartments[!(compartments %in% run$model$compartments$cmt | compartments %in% run$model$compartments$name)]

  if (length(missing_cmt) > 0) {
    stop(simpleError(sprintf("Missing compartment(s): %s.", paste(missing_cmt, collapse = ", "))))
  }

  df <- run$model$compartments %>%
    mutate(unit = pmap_chr(list(cmt = cmt, name = name, unit = unit), function(cmt, name, unit) {
      if (as.character(cmt) %in% compartments) return(units[as.character(cmt)])
      if (name %in% compartments) return(units[name])
      unit
    }))

  run$model$compartments <- df

  invisible(run)
}

#' Rename model covariates
#'
#' @param covariates named character vector with new covariates names as items names and
#'   old covariates names as values.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A NONMEM run object.
#' @export
#'
#' @examples
#' renamed_EXAMPLERUN <- EXAMPLERUN %>%
#' rename_covariates(covariates = c("Weight" = "WT", "Study" = "STUD"))
#'
#' EXAMPLERUN %>%
#'   group_by(STUD) %>%
#'   plot_continuous_covariates_distributions(type = "boxplot")
#' renamed_EXAMPLERUN %>%
#'   group_by(STUD) %>%
#'   plot_continuous_covariates_distributions(type = "boxplot")
#'
#' # splitting also works using the original column name
#' renamed_EXAMPLERUN %>%
#'   group_by(STUD) %>%
#'   plot_continuous_covariates_distributions(type = "boxplot")
rename_covariates <- function(run, covariates = NULL) {
  missing_covariate <- covariates[!covariates %in% colnames(run$tables$pmxploitab)]

  if (length(missing_covariate) > 0) {
    stop(simpleError(sprintf("Missing covariate(s): %s.", paste(missing_covariate, collapse = ", "))))
  }

  col_dots <- setNames(covariates, names(covariates))

  run$model$covariates$name <- plyr::mapvalues(run$model$covariates$name, from = covariates, to = names(covariates))

  invisible(run)
}

get_selected_parameters <- function(source_df, parameters) {
  param_labs <- names(parameters)

  parameters <- unlist(parameters)
  if (no_param_names <- is.null(names(parameters))) {
    names(parameters) <- parameters
  }

  selected_parameters <- source_df %>%
    filter(name %in% parameters | as.character(id) %in% parameters) %>%
    mutate(label = ifelse(!no_param_names & as.character(id) %in% parameters, as.character(id), name))

  missing_parameters <- setdiff(parameters, unique(c(as.character(selected_parameters$id), selected_parameters$name)))

  if (length(missing_parameters) > 0) {
    stop(simpleError(paste("Missing parameter(s):", paste(missing_parameters, collapse = ", "))))
  }

  if (!is.null(param_labs)) {
    renamed_params <- parameters[param_labs != ""]

    selected_parameters$label <- plyr::mapvalues(selected_parameters$label, from = renamed_params, to = names(renamed_params))
  }

  selected_parameters <- selected_parameters %>%
    mutate(matching_order = match(column, parameters)) %>%
    mutate(matching_order = ifelse(is.na(matching_order), match(name, parameters), matching_order)) %>%
    arrange(matching_order)

  parameters <- setNames(selected_parameters$column, nm = selected_parameters$label)
}

get_selected_covariates <- function(source_df, covariates) {
  cov_labs <- names(covariates)

  covariates <- unlist(covariates)

  if (no_cov_names <- is.null(names(covariates))) names(covariates) <- covariates

  selected_covariates <- source_df %>%
    filter(column %in% covariates | name %in% covariates) %>%
    mutate(label = ifelse(!no_cov_names & column %in% covariates, column, name))

  missing_covariates <- setdiff(covariates, unique(c(selected_covariates$column, selected_covariates$name)))

  if (length(missing_covariates) > 0) {

    stop(simpleError(paste("Missing covariate(s):", paste(missing_covariates, collapse = ", "))))
  }

  if (!is.null(cov_labs)) {
    renamed_covs <- covariates[cov_labs != ""]

    selected_covariates$label <- plyr::mapvalues(selected_covariates$label, from = renamed_covs, to = names(renamed_covs))
  }

  selected_covariates <- selected_covariates %>%
    mutate(matching_order = match(column, covariates)) %>%
    mutate(matching_order = ifelse(is.na(matching_order), match(name, covariates), matching_order)) %>%
    arrange(matching_order)

  covariates <- setNames(selected_covariates$column, nm = selected_covariates$label)
}

get_reduced_dataset <- function(source_df, baseline_only) {
  df <- source_df

  original_groups <- NULL
  if (!is.null(groups(df))) {
    original_groups <- as.character(groups(df))
    df <- ungroup(df)
  }

  if (baseline_only) {
    df <- df %>%
      group_by(ID) %>%
      slice(1) %>%
      ungroup()
  } else {
    df <- df %>%
      group_by(ID, TIME) %>%
      slice(1) %>%
      ungroup()
  }

  # re-group like the original
  if (!is.null(original_groups)) {
    df <- df %>% group_by(!!!(syms(original_groups)))
  }

  df
}



#' Get an estimation step of a run
#'
#' Get an estimation step of a run from its number
#'
#' @param run \code{pmxploit} NONMEM run object.
#' @param estimation_number integer. Number of the estimation step.
#' Default is NULL, returning the last estimation step.

#'
#' @return A list containing the estimation information and results
get_estimation <- function(run, estimation_number = NULL) {
  run_est_numbers <- map_int(run$estimations, "number")

  if (is.null(estimation_number)) {
    est_with_iterations <- run$estimations %>%
      keep(~!is.null(.$iterations))

    estimation_number <- est_with_iterations %>%
      map_int(~.$number) %>%
      max()
  } else {
    if (!(estimation_number %in% run_est_numbers)) {
      stop(simpleError(str_c("No estimation step number ", estimation_number)))
    }
  }

  run$estimations[[estimation_number]]
}
