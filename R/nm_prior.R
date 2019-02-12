#' Generate a NONMEM control stream with $PRIOR
#'
#' Generates a NONMEM control stream for an analysis using prior information from a source run.
#'
#' @param run \code{pmxploit} NONMEM run object of the source run.
#' @param df_formula integer. Formula to use for computation of degrees of freedom of OMEGA and SIGMA priors.
#' @param output_file (optional) character. File path to write the generated
#' control stream to. Default is \code{NULL}, not saving any file.
#' @param dataset_filename (optional) character. Name of the dataset to set in
#' the $DATA record of the generated control stream. Default is \code{NULL},
#' leaving $DATA record as it is in the source run.
#' @param estimations (optional) list. A list
#' containing the estimation record(s)
#' to set for the generated control stream. Each estimation record must be
#' represented by a list item containing the estimation settings as a named list.
#'
#' @return A character string of the generated control stream code.
#' @export
#'
#' @examples
#' # Control stream generation from pmxploit::EXAMPLERUN
#' cs1 <- nm_prior(EXAMPLERUN)
#' # see the output
#' writeLines(cs1)
#'
#' # Use prior information with "new_dataset.csv" and save the control stream to "new_study.ctl"
#' nm_prior(EXAMPLERUN, output_file = "new_study.ctl", dataset_filename = "new_dataset.csv")
#'
#' # Use prior information with "new_dataset.csv", set estimation to SAEM/IMP and
#' # save the control stream to "new_study.ctl"
#' nm_prior(EXAMPLERUN,
#'          output_file = "new_study.ctl",
#'          dataset_filename = "new_dataset.csv",
#'          estimations = list(list(method = "SAEM",
#'                                  "INTERACTION", NBURN = 400, NITER = 1000,
#'                                  PRINT = 50, "NOABORT", CTYPE = 3),
#'                             list(method = "IMP",
#'                                  NITER = 10, PRINT = 1,
#'                                  ISAMPLE = 3000, EONLY = 1)))
nm_prior <- function(run,
                     df_formula = 0,
                     df_values = NULL,
                     output_file = NULL,
                     dataset_filename = NULL,
                     estimations = NULL){

  cs <- run$control_stream

  df <- cs$records %>%
    mutate(lines = pmap(list(name = name, start = start, end = end), function(name, start, end){
      switch(name,
             "DATA" = {
               dat_code <- cs$code[start:end]
               if(!is.null(dataset_filename))
                 dat_code <- dat_code %>% str_replace(fixed(cs$dataset_file), dataset_filename)

               dat_code
             },
             cs$code[start:end])
    }))

  # Remove prior $PRIOR related statements, if any
  df <- df %>%
    filter(!(name %in% c("PRIOR", "THETAP", "THETAPV", "OMEGAP", "OMEGAPD", "SIGMAP", "SIGMAPD")))

  last_est <- last(run$estimations)

  if(is.null(last_est$covariance_matrix))
    stop("No variance-covariance matrix of estimates.")

  default_padding <- 22

  # THETA
  theta_id <- which(df$name == "THETA")
  n_theta <- nrow(last_est$thetas)

  theta_p_text <- str_c(str_pad(last_est$thetas$estimate, default_padding, "right"), "FIXED")
  theta_pv_mat <- last_est$covariance_matrix[1:n_theta, 1:n_theta]
  theta_pv_text <- lower_matrix_text(theta_pv_mat)


  # degrees_of_freedom addition (refering to formula)
  deg_add <- ifelse(df_formula == 0, 0, 1)

  # OMEGA
  omega_id <- which(df$name == "OMEGA")
  omega_mat <- last_est$omega_matrix
  omega_se_mat <- last_est$omegase_matrix
  n_omega <- nrow(omega_mat)

  if(nrow(ob <- cs$parameters$blocks$OMEGA) > 0){
    omega_p_text <- NULL
    omega_pd <- NULL
    current_om <- 1
    while(TRUE){
      if(any(ob$from == current_om)){
        current_block <- ob %>% filter(from == current_om)
        fr <- current_block$from
        to <- current_block$to

        cn <- crossing(a = fr:to, b = fr:to) %>% filter(a <= b) %>% mutate(text = sprintf("OMEGA(%s,%s)", a, b))

        sub_mat <- omega_mat[fr:to,fr:to]
        sub_mat_se <- omega_se_mat[fr:to,fr:to]

        deg_free <- 2 * (diag(sub_mat)/diag(sub_mat_se))^2

        omega_pd <- c(omega_pd, min(deg_free)) + deg_add

        text_to_add <- str_c(sprintf("$OMEGAP BLOCK(%s)", nrow(sub_mat)), lower_matrix_text(sub_mat), sep = "\n")

        current_om <- to + 1
      } else {
        val <- omega_mat[current_om,current_om]
        val_se <- omega_se_mat[current_om,current_om]

        deg_free <- 2 * ((val/val_se)^2)
        omega_pd <- c(omega_pd, deg_free) + deg_add

        prefix <- ifelse((current_om - 1) %in% c(0, ob$to), "$OMEGAP\n", "")

        text_to_add <- str_c(prefix, str_pad(val, default_padding, "right"), "FIXED")

        current_om <- current_om + 1
      }

      omega_p_text <- str_c(omega_p_text, text_to_add, sep = "\n")

      if(current_om > n_omega)
        break
    }
  } else {
    d_m <- diag(omega_mat)
    d_m_se  <- diag(omega_se_mat)
    deg_free <- 2 * ((d_m/d_m_se)^2) + deg_add
    omega_pd <- ifelse(!is.nan(deg_free) & !is.infinite(deg_free), deg_free, 0)
    omega_p_text <- str_c("$OMEGAP", str_c(str_pad(d_m, default_padding, "right"), "FIXED", collapse = "\n"), sep = "\n")
  }

  # SIGMA
  sigma_id <- which(df$name == "SIGMA")
  sigma_mat <- last_est$sigma_matrix
  sigma_se_mat <- last_est$sigmase_matrix
  n_sigma <- nrow(sigma_mat)
  sigma_pd <- NA_integer_
  sigma_p_text <- NA_character_

  if(length(sigma_id) > 0){
    if(nrow(sb <- cs$parameters$blocks$SIGMA) > 0){
      sigma_p_text <- NULL
      sigma_pd <- NULL
      current_sm <- 1
      while(TRUE){
        if(any(sb$from == current_sm)){
          current_block <- ob %>% filter(from == current_sm)
          fr <- current_sm$from
          to <- current_sm$to

          cn <- crossing(a = fr:to, b = fr:to) %>% filter(a <= b) %>% mutate(text = sprintf("SIGMA(%s,%s)", a, b))

          #sub_mat <- last_est$covariance_matrix[cn$text,cn$text]
          sub_mat <- sigma_mat[fr:to,fr:to]
          sub_mat_se <- sigma_se_mat[fr:to,fr:to]

          deg_free <- 2 * (diag(sub_mat)/diag(sub_mat_se)^2)

          sigma_pd <- c(sigma_pd, min(deg_free)) + deg_add

          text_to_add <- str_c(sprintf("BLOCK(%s)", nrow(sub_mat)), lower_matrix_text(sub_mat), sep = "\n")

          current_sm <- to + 1
        } else {
          val <- sigma_mat[current_sm,current_sm]
          val_se <- sigma_se_mat[current_sm,current_sm]

          deg_free <- 2 * ((val/val_se)^2)
          sigma_pd <- c(sigma_pd, deg_free) + deg_add

          prefix <- ifelse((current_sm - 1) %in% c(0, sb$to), "$SIGMAP\n", "")

          text_to_add <- str_c(prefix, str_pad(val, default_padding, "right"), "FIXED")
          # text_to_add <- str_c("$SIGMAP\n", str_pad(val, default_padding, "right"), "FIXED")

          current_sm <- current_sm + 1
        }

        sigma_p_text <- str_c(sigma_p_text, text_to_add, sep = "\n")

        if(current_sm > n_sigma)
          break
      }
    } else {
      d_m <- diag(sigma_mat)
      d_m_se  <- diag(sigma_se_mat)
      deg_free <- 2 * ((d_m/d_m_se)^2) + deg_add
      sigma_pd <- ifelse(!is.nan(deg_free) & !is.infinite(deg_free), deg_free, 0)
      sigma_p_text <- str_c("$SIGMAP", str_c(str_pad(d_m, default_padding, "right"), "FIXED", collapse = "\n"), sep = "\n")
    }

  }

  if(!is.null(df_values)){
    if(!is.null(df_values$OMEGAPD)){
      omega_pd <- df_values$OMEGAPD
    }

    if(!is.null(df_values$SIGMAPD)){
      sigma_pd <- df_values$SIGMAPD
    }
  }

  omega_pd <- round(omega_pd)
  sigma_pd <- round(sigma_pd)

  new_records <- list(
    list(name = "COMMENT", lines = c(pad_line(n_pad = 50, pad = "="), "; PRIOR INFORMATION", pad_line(n_pad = 50, pad = "="))),
    list(name = "THETAP", lines = str_c(c(pad_line("THETA Priors"),
                                          "$THETAP",
                                          theta_p_text), collapse = "\n")),
    list(name = "THETAPV", lines = str_c(c(pad_line("Variance to THETA priors"),
                                           sprintf("$THETAPV BLOCK(%s)", n_theta),
                                           theta_pv_text), collapse = "\n")),
    list(name = "OMEGAP", lines = str_c(c(pad_line("OMEGA Priors"),
                                          omega_p_text), collapse = "\n")),
    list(name = "OMEGAPD", lines = str_c(c(pad_line("Degrees of freedom of OMEGA Prior"),
                                           "$OMEGAPD",
                                           str_c(str_c(str_pad(omega_pd, default_padding, "right"), "FIXED ; ", names(omega_pd)), collapse = "\n")), collapse = "\n")),
    list(name = "SIGMAP", lines = str_c(c(pad_line("SIGMA Priors"),
                                          sigma_p_text), collapse = "\n")),
    list(name = "SIGMAPD", lines = str_c(c(pad_line("Degrees of freedom of SIGMA Prior"),
                                           "$SIGMAPD",
                                           str_c(str_c(str_pad(sigma_pd, default_padding, "right"), "FIXED ; "), names(sigma_pd), collapse = "\n")), collapse = "\n")),
    list(name = "COMMENT", lines = c(pad_line(pad = "=")))
  )

  prior_writing_id <- max(which(df$name %in% c("THETA", "OMEGA", "SIGMA")))

  df <- df %>%
    add_row(name = map_chr(new_records, "name"),
            lines = map(new_records, "lines"), .after = prior_writing_id) %>%
    add_row(lines = sprintf("; Control stream generated for PRIOR analysis from:\n; %s\n", run$info$path), .before = 1) %>%
    add_row(name = "PRIOR", lines = "$PRIOR NWPRI\n", .after = max(which(df$name %in% c("INPUT", "DATA", "MODEL"))))

  # df <- df %>%
  #   mutate(lines = map2(name, lines, ~ if(..1 == "INPUT") { "$INPUT ;; Edit $INPUT to match the new dataset columns" } else { ..2 }))
  input_writing_id <- max(which(df$name == "INPUT"))

  df[input_writing_id,]$lines <- "$INPUT ;; Edit $INPUT to match the new dataset columns"

  if(!is.null(estimations)){
    est_writing_id <- max(which(df$name == "ESTIMATION"))

    df <- df %>% filter(!name == "ESTIMATION") # remove original estimation steps

    est_rows <- map_chr(estimations, function(e){
      item_names <- toupper(names(e))
      values <- toupper(e)
      str_c("$ESTIMATION", str_c(str_c(ifelse(!is.null(item_names) & item_names != "",
                                              str_c(item_names, "=", values), values), collapse = " "), collapse = " "), sep = " ")
    })

    df <- df %>%
      add_row(.before = est_writing_id, name = "ESTIMATION", lines = est_rows)
  }

  prior_text <- str_c(str_replace_na(map_chr(df$lines, ~ str_c(., collapse = "\n")), ""), collapse = "\n")

  if(!is.null(output_file)){
    if(!dir.exists(dirname(output_file)))
      dir.create(dirname(output_file), recursive = TRUE)

    write_lines(prior_text, output_file)
  }

  prior_text
}
