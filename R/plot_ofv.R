#' Objective function plot
#'
#' Plots the value, at each printed out iteration, of objective function during an estimation step.
#'
#' @param estimation_number integer. Number of the estimation step. Default is
#'   \code{NULL}, plotting the last estimation.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% plot_ofv()
#'
#' EXAMPLERUN %>% plot_ofv(estimation_number = 1)
plot_ofv <-
  function(run,
             estimation_number = NULL,
             auto_legend = TRUE) {
    est_with_iterations <-
      run$estimations %>%
      keep(~!is.null(.$iterations))

    if (length(est_with_iterations) == 0) {
      stop(simpleError("No estimation step with iterations found."))
    }


    if (is.null(estimation_number)) {
      estimation_number <-
        est_with_iterations %>%
        map_int(~.$number) %>%
        max()
    }

    selected_est <-
      run$estimations %>%
      keep(~.$number == estimation_number) %>%
      flatten()

    if (is.null(selected_est$iterations)) {
      stop(simpleError("No iteration values reported."))
    }

    df <- selected_est$iterations %>%
      mutate(n_printed_iteration = row_number())

    ofv_col <- last(colnames(selected_est$iterations))

    if(nrow(df) == 0){
      g <- ggplot()+
        geom_hline(yintercept = selected_est$final_ofv)
      }
    else {

      breaks_subset <- df[seq(
        from = 1,
        to = nrow(df),
        by = 10
      ), ]

      g <- ggplot(df, aes_string(x = "n_printed_iteration", y = ofv_col)) +
        geom_path() +
        scale_x_continuous(
          breaks = breaks_subset$n_printed_iteration,
          labels = breaks_subset$ITERATION
        )
    }

    if (auto_legend) {
      g <- g + labs(x = "Iterations",
                    y = ofv_col,
                    caption = str_c("Path: ", run$info$path))
    }

    g
  }
