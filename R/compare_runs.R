#' Compare NONMEM runs estimation results
#'
#' Returns a table summarizing the results of estimation steps of a list of NONMEM runs.
#'
#' @param runs a list of \code{pmxploit} NONMEM run objects.
#' @param estimation_number (optional) integer. A vector of containing the position(s)
#' of the estimation step(s) to compare in the runs. If \code{estimation_number=-1},
#' the last estimation step of each run will be selected. Default is \code{NULL} to
#' consider all estimation steps.
#'
#' @return A data frame.
#'
#' @details Results are gathered in a nested data frame, containing one row per estimation step included in the comparison.
#' Each row contains the following columns: \itemize{
#' \item \code{RUN_ID}: the run identifying name/number.
#' \item \code{INFO}: information about the run (problem, number of observations, number of individuals and run duration).
#' \item \code{FILES}: a tibble containing the control stream name, the dataset name and the run archive path.
#' \item \code{ESTIMATION}: a nested tibble containing the following columns: \itemize{
#' \item \code{SUMMARY}: a tibble containing the estimation results summary (method, OFV, termination status, duration...).
#' \item \code{THETA}: a tibble containing THETA parameters results.
#' \item \code{OMEGA}: a tibble containing OMEGA parameters results.
#' \item \code{SIGMA}: a tibble containing SIGMA parameters results.
#' \item \code{ETABAR}: a tibble containing ETABAR results.
#' \item \code{SHRINKAGE}: a tibble containing shrinking results.}}
#'
#' @export
#'
#' @examples
#' compare_runs(list(EXAMPLERUN, EXAMPLERUN))
compare_runs <- function(runs = NULL, estimation_number = NULL) {
  stopifnot(is.list(runs))

  comp <- map_df(runs, function(run) {
    selected_estimations <- run$estimations

    if (is.null(estimation_number)) {
      selected_estimations <- run$estimations
    } else if (estimation_number == -1L) {
      selected_estimations <- run$estimations[length(run$estimations)]
    } else {
      selected_estimations <- run$estimations[estimation_number]
    }

    run_summary <- tibble(
      RUN_ID = run$info$run_id,
      INFO = list(tibble(
        RUN_NAME = run$info$run_name,
        OBS = run$info$number_of_observations,
        IDS = run$info$number_of_subjects,
        DURATION = run$info$duration,
        CPU = run$info$nodes
      )),
      FILES = list(tibble(
        `Control stream` = run$info$control_stream_file,
        `Dataset` = run$info$dataset_file,
        `Path` = run$info$path
      ))
    )

    est_list <- map_df(seq_along(selected_estimations), function(est_id) {
      est <- selected_estimations[[est_id]]

      # cor96 <- NA
      #
      # if (!is.null(est$correlation_matrix)) {
      #   cor96 <- any(est$correlation_matrix >= 0.96)
      # }

      est_summary <- tibble(SUMMARY = list(tibble(
        STATUS = ifelse(est$failed, "Failed",
          ifelse(!est$minimization,
            "No minimization",
            ifelse(est$termination_status == 0,
              "Successful",
              "Terminated"
            )
          )
        ),
        OBJ = est$final_ofv,
        EIGENRATIO = ifelse(is.null(est$eigenratio), NA, est$eigenratio),
        `MAXIMUM CORRELATION` = est$max_correlation,
        NPAR = est$n_parameters,
        AIC = est$aic,
        BIC = est$bic,
        EVALS = est$nfuncevals,
        SIGDIGITS = est$significant_digits,
        DURATION = est$duration,
        METHOD = est$title
      )))
      # MESSAGES = ifelse(!is.null(est$termination_messages),
      #                   str_c(est$termination_messages$message, "\n"),
      #                   "")

      thetas <- thetas_rse <- omega <- omega_rse <- sigma <- sigma_rse <- etabars <- etapval <- shrink <- NULL # tibble()#list(tibble())

      if (!est$failed) {
        if (!is.null(est$thetas)) {
          thetas <- est$thetas %>%
            select(name, estimate) %>%
            spread(name, estimate)
          thetas_rse <- est$thetas %>%
            select(name, rse) %>%
            spread(name, rse) %>%
            rename(!!!setNames(colnames(.), sprintf("RSE(%s)", colnames(.))))
        }

        if (!is.null(est$omega)) {
          temp_omega <- est$omega %>%
            mutate(name = sprintf("%s:%s", eta1, eta2))

          omega <- temp_omega %>%
            select(name, estimate) %>%
            spread(name, estimate)
          omega_rse <- temp_omega %>%
            mutate(name = sprintf("RSE(%s)", name)) %>%
            select(name, rse) %>%
            spread(name, rse)
        }

        if (!is.null(est$sigma)) {
          temp_sigma <- est$sigma %>%
            mutate(name = sprintf("%s:%s", epsilon1, epsilon2))

          sigma <- temp_sigma %>%
            select(name, estimate) %>%
            spread(name, estimate)

          sigma_rse <- temp_sigma %>%
            mutate(name = sprintf("RSE(%s)", name)) %>%
            select(name, rse) %>%
            spread(name, rse)
        }

        if (!is.null(est$eta_bars)) {
          etabars_select <- intersect(colnames(est$eta_bars), c("name", "value", "subpop"))
          is_mix <- "subpop" %in% colnames(est$eta_bars)

          etabars <- est$eta_bars %>%
            select(etabars_select) %>%
            mutate(name = map(name, ~ifelse(is_mix, str_c(., ".", subpop), .))) %>%
            mutate(name = sprintf("ETABAR(%s)", name)) %>%
            spread(name, value)

          if ("pvalue" %in% colnames(est$eta_bars)) {
            etapval_select <- intersect(colnames(est$eta_bars), c("name", "pvalue", "subpop"))

            etapval <- est$eta_bars %>%
              select(etapval_select) %>%
              mutate(name = sprintf("ETAPVAL(%s)", name)) %>%
              spread(name, pvalue)
          }
        }

        if (!is.null(est$shrinkage)) {
          shrink <- est$shrinkage %>%
            mutate(name = sprintf("%sshrink(%s)", type, parameter)) %>%
            select(-type, -parameter) %>%
            spread(name, shrinkage)
        }
      }

      theta_data <- (if (all(map_lgl(list(thetas, thetas_rse), is.null))) NULL else bind_cols(thetas, thetas_rse))
      omega_data <- (if (all(map_lgl(list(omega, omega_rse), is.null))) NULL else bind_cols(omega, omega_rse))
      sigma_data <- (if (all(map_lgl(list(sigma, sigma_rse), is.null))) NULL else bind_cols(sigma, sigma_rse))
      etabar_data <- (if (all(map_lgl(list(etabars, etapval), is.null))) NULL else bind_cols(etabars, etapval))

      est_summary %>%
        mutate(
          THETA = list(theta_data),
          OMEGA = list(omega_data),
          SIGMA = list(sigma_data),
          ETABAR = list(etabar_data),
          SHRINKAGE = list(shrink)
        )
    })

    # run_summary %>%
    #   slice(rep(1, nrow(est_list))) %>%
    #   bind_cols(est_list)
    run_summary %>% mutate(ESTIMATION = list(est_list))
  })

  comp
}
