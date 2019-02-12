#' Generate a NONMEM control stream for simulations from a run
#'
#' Generates a NONMEM control stream for simulations from a source run.
#'
#' @param n_simulations integer. Number of simulations (\code{$SIMULATION NSUBPROBLEMS})
#' to run.
#' @param seed integer. Seed number for random number generation.
#' @param output_table_format (optional) character. NONMEM format (see NONMEM help) for the simulated
#'  output table. Default is \code{,1PE11.4}, corresponding to a csv format.
#'
#' @inheritParams nm_prior
#'
#' @details
#' All \code{THETA}, \code{OMEGA} and \code{SIGMA} parameters will be fixed to the source run final estimates.
#' \code{$ESTIMATION} record(s) will be replaced by a single \code{$SIMULATION} record (with the specified number of simulations).
#' Simulated output will be saved to a single \code{simtab} file (\code{$TABLE}).
#'
#' @return A character string of the generated control stream code.
#' @export
#'
#' @examples
#'
#' # Control stream generation from pmxploit::EXAMPLERUN
#' cs1 <- nm_simulation(EXAMPLERUN, n_simulations = 300)
#'
#' # see the output
#' writeLines(cs1)
nm_simulation <- function(run,
                                        n_simulations = 100,
                                        seed = sample.int(1e9, 1),
                                        output_table_format = ",1PE11.4",
                                        output_file = NULL) {
  cs <- run$control_stream

  cols_to_keep <- cs$input %>% filter(!dropped) %>%
    mutate(f_name = ifelse(!is.na(synonym), synonym, column)) %>%
    pull(f_name)

  sim_ds <- run$tables$pmxploitab %>%
    select(one_of(cols_to_keep)) %>%
    mutate(DV = NA)

  last_est <- last(run$estimations)

  if(is.null(last_est$thetas))
    stop(simpleError("No THETA estimates found in estimation results."))

  default_padding <- 22

  # Generate control stream with THETA and OMEGA (lower tri)
  # $THETA ----
  theta_df <- last_est$thetas %>%
    select(name, estimate) %>%
    mutate(text = str_c(str_pad(estimate, default_padding, side = "right"), "FIXED ; ", name))


  # OMEGA
  omega_mat <- last_est$omega_matrix
  n_omega <- nrow(omega_mat)

  if(!is.null(cs$parameters$blocks$OMEGA) && nrow(ob <- cs$parameters$blocks$OMEGA) > 0){
    omega_text <- NULL
    current_om <- 1
    while(TRUE){
      if(any(ob$from == current_om)){
        current_block <- ob %>% filter(from == current_om)
        fr <- current_block$from
        to <- current_block$to

        cn <- crossing(a = fr:to, b = fr:to) %>% filter(a <= b) %>% mutate(text = sprintf("OMEGA(%s,%s)", a, b))

        sub_mat <- omega_mat[fr:to,fr:to]
        text_to_add <- str_c(sprintf("$OMEGA BLOCK(%s)", nrow(sub_mat)), lower_matrix_text(sub_mat), sep = "\n")

        current_om <- to + 1
      } else {
        val <- omega_mat[current_om,current_om]

        prefix <- ifelse((current_om - 1) %in% c(0, ob$to), "$OMEGA\n", "")

        text_to_add <- str_c(prefix, str_pad(val, default_padding, "right"), "FIXED")

        current_om <- current_om + 1
      }

      omega_text <- str_c(omega_text, text_to_add, sep = "\n")

      if(current_om > n_omega)
        break
    }
  } else {
    d_m <- diag(omega_mat)
    omega_text <- str_c("$OMEGA", str_c(str_pad(d_m, default_padding, "right"), str_c("FIXED ; ", names(d_m), ":", names(d_m)), collapse = "\n"), sep = "\n")
  }

  # SIGMA
  sigma_mat <- last_est$sigma_matrix
  n_sigma <- nrow(sigma_mat)

  if(!is.null(cs$parameters$blocks$SIGMA) && nrow(sb <- cs$parameters$blocks$SIGMA) > 0){
    sigma_text <- NULL
    current_sm <- 1
    while(TRUE){
      if(any(sb$frsm == current_sm)){
        current_block <- sb %>% filter(frsm == current_sm)
        fr <- current_block$frsm
        to <- current_block$to

        cn <- crossing(a = fr:to, b = fr:to) %>% filter(a <= b) %>% mutate(text = sprintf("SIGMA(%s,%s)", a, b))

        sub_mat <- sigma_mat[fr:to,fr:to]

        text_to_add <- str_c(sprintf("$SIGMA BLOCK(%s)", nrow(sub_mat)), lower_matrix_text(sub_mat), sep = "\n")

        current_sm <- to + 1
      } else {
        val <- sigma_mat[current_sm,current_sm]

        prefix <- ifelse((current_sm - 1) %in% c(0, sb$to), "$SIGMA\n", "")

        text_to_add <- str_c(prefix, str_pad(val, default_padding, "right"), "FIXED")

        current_sm <- current_sm + 1
      }

      sigma_text <- str_c(sigma_text, text_to_add, sep = "\n")

      if(current_sm > n_sigma)
        break
    }
  } else {
    d_m <- diag(sigma_mat)
    sigma_text <- str_c("$SIGMA", str_c(str_pad(d_m, default_padding, "right"), str_c("FIXED ; ", names(d_m), ":", names(d_m)), collapse = "\n"), sep = "\n")
  }


  sim_ds_filename <- basename(cs$dataset_file)
  sim_tab_columns <- intersect(c("TIME", "CMT", "EVID", "DV", "MDV"), colnames(sim_ds))

  # Filter out records incompatible with $SIMULATION
  df <- cs$records %>%
    filter(!(name %in% c("ESTIMATION", "COVARIANCE", "TABLE",
                         "PRIOR", "THETAP", "THETAPV", "OMEGAP", "OMEGAPD", "SIGMAP", "SIGMAPD",
                         "SCATTERPLOT")))

  if(!any(df$name %in% c("THETA", "OMEGA", "SIGMA"))){ # msfi record

    msfi_id <- min(which(df$name == "MSFI"))

    df <- df %>%
      filter(name != "MSFI") %>%
      add_row(name = c("THETA", "OMEGA", "SIGMA"), .after = msfi_id - 1)
  }

  # input_names <- colnames(sim_ds)

  # if(nrow(dv_synonym <- cs$input %>% filter(synonym == "DV")) == 1){
  #   input_names[input_names == "DV"] <- str_c("DV=", dv_synonym$column)
  # }

  source_path <- run$info$path

  df <- df %>%
      mutate(lines = pmap(list(name = name, start = start, end = end),
                          function(name, start, end){
                            switch(name,
                                   "DATA" = cs$code[start:end] %>% str_replace(fixed(cs$dataset_file), sim_ds_filename),
                                   # "INPUT" = str_c("$INPUT ", str_c(input_names, collapse = " ")),
                                   "THETA" = c("$THETA", theta_df$text),
                                   "OMEGA" = omega_text,
                                   "SIGMA" = c(sigma_text),
                                   {
                                     cs$code[start:end]
                                   })
                          })) %>%
    add_row(lines = sprintf("; Control stream generated for simulations (N = %s) from:\n; %s\n", n_simulations, source_path), .before = 1) %>%
    add_row(lines = sprintf("$SIMULATION (%s) ONLYSIMULATION NSUBPROBLEMS=%s", seed, n_simulations)) %>%
    add_row(lines = sprintf("$TABLE ID %s NOHEADER NOPRINT NOAPPEND FORMAT=%s FILE=simtab",
                            str_c(sim_tab_columns, collapse = " "),
                            ",1PE11.4"))

  first_record_id <- min(which(df$name %in% c("THETA", "OMEGA", "SIGMA")))
  last_record_id <- max(which(df$name %in% c("THETA", "OMEGA", "SIGMA")))

  df <- df %>%
    add_row(name = "COMMENT", lines = c(pad_line(n_pad = 50, pad = "="), "; POPULATION PARAMETERS", pad_line(n_pad = 50, pad = "=")), .before = first_record_id) %>%
    add_row(name = "COMMENT", lines = c(pad_line(n_pad = 50, pad = "=")), .after = last_record_id + 3)

  sim_text <-  str_c(map_chr(df$lines, ~ str_c(., collapse = "\n")), collapse = "\n")

  if(!is.null(output_file)){
    if(!dir.exists(dirname(output_file)))
      dir.create(dirname(output_file), recursive = TRUE)

    write_lines(sim_text, output_file)
  }


  sim_text
}
