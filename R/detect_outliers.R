#' Outliers detection
#'
#' Detection of observations outliers based on residuals values according to two
#' different methods: Grubb's test or a non-parametric method.
#'
#' @param compartment integer. Number of the compartment of the observations of interest.
#' @param method character. One of \code{"grubbs"} (for iterative Grubb's test)
#'   or \code{"boxplot"} (for non-parametric detection).
#' @param residuals character. Column name of the residuals in the output tables.
#' @param grubbs_pvalue_threshold numeric. p-value threshold for Grubb's test.
#' @param boxplot_coefficient numeric. k coefficient for non-parametric test.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A list with the following structure: \itemize{
#' \item \code{method}: character string of outlier detection method.
#' \item \code{residuals}: character string of the type of residuals.
#' \item \code{source}: tibble of the data source.
#' \item \code{outliers}: tibble of the outliers with 5 columns: \code{ID}, \code{TIME}, \code{CMT}, \code{DV} and
#' \code{"residuals"}.
#' }
#'
#' @export
#'
#' @examples
#' EXAMPLERUN %>%
#'  detect_outliers(compartment = 2, residuals = "CWRES", method = "grubbs")
#'
#' EXAMPLERUN %>%
#'  detect_outliers(compartment = 2, residuals = "CWRES",
#'                  method = "grubbs", grubbs_pvalue_threshold = 0.10)
#'
#' EXAMPLERUN %>%
#'  detect_outliers(compartment = 2, residuals = "CWRES", method = "boxplot")
detect_outliers <- function(run, compartment = NULL, residuals = NULL, method = "grubbs",
                            grubbs_pvalue_threshold = 0.05, boxplot_coefficient = 3, keep_time_zero = FALSE) {
  if (is.null(compartment)) stop(simpleError("Compartment must be specified."))
  if (is.null(residuals)) stop(simpleError("Residuals type must be specified."))

  df <- run$tables$pmxploitab

  if (nrow(df) == 0 & !is.null(attr(df, "filters"))) {
    stop(simpleError("Data is empty after filtering."))
  }

  df <- df %>%
    filter(!is.na(DV) & CMT == compartment)

  if (!(residuals %in% colnames(df))) {
    stop(simpleError(paste("Missing residuals type:", residuals)))
  }

  if (!keep_time_zero) {
    df <- df %>% filter(TIME > 0)
  }

  df <- df %>%
    select(ID, TIME, CMT, DV, PRED, one_of(residuals))

  if (method == "grubbs") {
    copy_df <- df
    outliers_data <- NULL
    outliers_values <- NULL
    grubbs.result <- outliers::grubbs.test(copy_df[[residuals]])
    pv <- grubbs.result$p.value
    grubbs_iter <- 1
    while (!is.na(pv) && pv < grubbs_pvalue_threshold) {
      new_out_val <- as.numeric(str_split(grubbs.result$alternative, " ")[[1]][3])
      outliers_values <- c(outliers_values, new_out_val)

      kill <- which(copy_df[[residuals]] %in% outliers_values)

      if (length(kill) %in% c(0, nrow(copy_df))) {
        print(new_out_val)
        break
      }

      outliers_data <- c(
        outliers_data,
        list(
          list( # Grubbs_iteration = grubbs_iter,
            value = new_out_val
          )
        )
      )

      copy_df <- copy_df[-kill, ]

      grubbs.result <- outliers::grubbs.test(copy_df[[residuals]])
      pv <- grubbs.result$p.value
      grubbs_iter <- grubbs_iter + 1
    }

    temp_out_df <- bind_rows(outliers_data)

    if (nrow(temp_out_df) == 0) {
      outliers_df <- df %>% slice(0)
    } else {
      outliers_df <- df %>%
        mutate(Outlier = .[[residuals]] %in% temp_out_df$value) %>%
        filter(Outlier == TRUE) %>%
        select(-Outlier) %>%
        right_join(temp_out_df, by = setNames(nm = residuals, "value"))
    }
  } else if (method == "boxplot") {
    values <- df[[residuals]]

    low_q <- quantile(values, 0.25)
    up_q <- quantile(values, 0.75)

    low_b <- low_q - boxplot_coefficient * (up_q - low_q)
    up_b <- up_q + boxplot_coefficient * (up_q - low_q)

    outliers_df <- df %>%
      mutate(max_dist = pmax(abs(.[[residuals]] - low_b), abs(.[[residuals]] - up_b))) %>%
      arrange(desc(max_dist)) %>%
      mutate(Outlier = !between(.[[residuals]], low_b, up_b)) %>%
      filter(Outlier == TRUE) %>%
      select(-Outlier, -max_dist)
  } else {
    stop(simpleError("Unknown method."))
  }

  if (nrow(df) > 0) {
    ds <- run$tables$dataset %>%
      select(ID, TIME, CMT) %>%
      mutate(dataset_row_index = row_number())

    outliers_df <- outliers_df %>% inner_join(ds, by = c("ID", "TIME", "CMT"))
  }


  list(
    method = method,
    residuals = residuals,
    source = df,
    outliers = outliers_df
  )
}
