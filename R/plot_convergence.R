#' Convergence plot
#'
#' Plots the value, at each printed out iteration, of the population parameters (\code{THETA}, \code{OMEGA} or
#' \code{SIGMA}) during an estimation step. Fixed parameters are not plotted.
#'
#' @param estimation_number integer. Number of the estimation step. Default is
#'   \code{NULL}, plotting all the steps.
#' @param parameters character. Either one of \code{"theta"}, \code{"omega"} or
#'   \code{"sigma"} or a character vector of population parameters names. Default
#'   is \code{NULL} plotting all population parameters.
#'
#' @inheritParams plot_dv_vs_predictions
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' EXAMPLERUN %>% plot_convergence()
#'
#' EXAMPLERUN %>% plot_convergence(parameters = "theta")
#' EXAMPLERUN %>% plot_convergence(parameters = "omega")
#'
#' EXAMPLERUN %>% plot_convergence(parameters = c(CL_pop = "THETA3"))
plot_convergence <-
  function(run, estimation_number = NULL, parameters = NULL, auto_legend = TRUE) {
    estimation_ids <- estimation_number

    ok_estimation <- map_int(run$estimations, ~.$number) %>% unname()

    if (is.null(estimation_ids)) {
      estimation_ids <- ok_estimation
    }

    if (any(!(estimation_ids %in% ok_estimation))) {
      stop(simpleError("Incorrect estimation number."))
    }

    estimation_ids <- sort(estimation_ids)

    pop_param_types <- c("theta", "omega", "sigma")

    pop_parameters <- run$model$parameters %>% filter(type %in% pop_param_types)

    if (is.null(parameters)) {
      fixed_parameters <- pop_parameters %>% filter(type %in% pop_param_types) # filter(type == "theta")
      parameters <- setNames(fixed_parameters$name, fixed_parameters$name)
    } else if (all(parameters %in% pop_param_types)) {
      selected_parameters <- pop_parameters %>% filter(type %in% parameters)
      parameters <- setNames(selected_parameters$name, selected_parameters$name)
    } else {
      param_labs <- names(parameters)

      parameters <- unlist(parameters)
      if (is.null(names(parameters))) names(parameters) <- parameters

      selected_parameters <- pop_parameters %>%
        filter(name %in% parameters | as.character(id) %in% parameters) %>%
        mutate(label = ifelse(as.character(id) %in% param_labs, as.character(id), name))

      missing_parameters <- setdiff(parameters, unique(c(as.character(selected_parameters$id), selected_parameters$name)))

      if (length(missing_parameters) > 0) {
        stop(simpleError(paste("Missing parameter(s):", paste(missing_parameters, collapse = ", "))))
      }

      if (!is.null(param_labs)) {
        renamed_params <- parameters[param_labs != ""]

        selected_parameters$label <- plyr::mapvalues(selected_parameters$label, from = renamed_params, to = names(renamed_params))
      }

      parameters <- setNames(selected_parameters$name, nm = selected_parameters$label)
    }

    if (length(parameters) == 0) {
      stop(simpleError("No parameter convergence to plot."))
    }

    df <- map_df(estimation_ids, .id = NULL, function(i) {
      est <- run$estimations[[i]]

      if (est$failed | !est$minimization) {
        return(NULL)
      }

      it_df <- est$iterations %>%
        mutate(
          n_estimation = i,
          method = est$method,
          is_last = FALSE
        )

      # duplicate last row if it is not the last selected estimation
      if (i < max(estimation_ids)) {
        last_row <- it_df %>% slice(n())
        last_row$is_last <- TRUE

        it_df <- it_df %>% bind_rows(last_row)
      }

      it_df
    })

    if (nrow(df) == 0) {
      stop(simpleError("No iteration values reported."))
    }

    not_found_parameters <- parameters[!(parameters %in% colnames(df))]

    if (length(not_found_parameters) > 0) {
      warning(simpleWarning(paste("Iteration values not found for the following parameter(s):", paste(not_found_parameters, collapse = ", "))))

      parameters <- setdiff(parameters, not_found_parameters)
    }

    df <- df %>%
      mutate(
        n = ifelse(is_last, row_number() + 1, row_number()),
        estimation = paste(n_estimation, toupper(method), sep = ":")
      ) %>%
      select(estimation, method, n, ITERATION, one_of(parameters)) %>%
      rename(!!!parameters) %>%
      na.omit()

    fixed_params <- which(map_lgl(select(df, one_of(names(parameters))), function(x) {
      length(unique(x)) == 1
    })) %>% names()

    if (length(fixed_params) > 0) {
      message(simpleMessage(sprintf("Not shown fixed parameter(s): %s\n", paste(fixed_params, collapse = ", "))))

      df <- df %>% select(-one_of(fixed_params))
    }

    if (nrow(df) == 0 | length(fixed_params) == length(parameters)) {
      stop(simpleError("No parameter convergence to plot"))
    }

    g_df <- df %>%
      gather(parameter, value, -estimation, -method, -n, -ITERATION, factor_key = TRUE)

    g <- ggplot(g_df)

    if (length(estimation_ids) > 1) {
      mapping <- aes(x = n, y = value, colour = estimation)
    } else {
      mapping <- aes(x = n, y = value)
    }

    g <- g + geom_line(mapping) +
      facet_wrap(~parameter, scales = "free")

    if (auto_legend) {
      n_iter <- nrow(df)

      est_methods <- map(run$estimations[estimation_ids], function(x) x$method) %>% unlist()

      if (any(est_methods %in% c("saem", "saem_noprior")) & length(estimation_number) == 1) {
        phase2_iterations <- df %>%
          filter(method %in% c("saem", "saem_noprior") & ITERATION == 0) %>%
          summarise(break_iter = min(n))

        g <- g +
          geom_vline(
            data = phase2_iterations,
            mapping = aes(xintercept = break_iter),
            linetype = getOption("pmxploit.iterationplot.saem.linetype"),
            colour = getOption("pmxploit.iterationplot.saem.colour")
          )
      } else {

      }

      g <- g +
        labs(x = "Iterations", y = "Value", caption = str_c("Path: ", run$info$path))
    }

    g
  }
