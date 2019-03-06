#' Load a NONMEM run data from a directory
#'
#' @param control_stream \code{nonmem_control_stream} object. Control stream data, obtained from a prior
#'   call to \code{parse_nm_control_stream()} function.
load_nm_run_directory <-
  function(path,
             dataset_separator = NA,
             dataset_na_string = ".",
             read_initial_values = TRUE,
             load_tables = TRUE,
             control_stream = NULL,
             update_progress = NULL,
             verbose = FALSE) {
    run_directory <- path
    run_directory <- normalizePath(run_directory)

    if (!dir.exists(run_directory)) {
      stop(simpleError("Run directory not found."))
    }

    loop <- 1
    while (loop != 0) {
      run_files <- list.files(run_directory, full.names = TRUE) %>%
        .[file.exists(.)] %>%
        normalizePath() %>%
        unique() # needed because of symbolik links

      run_files_df <- tibble(file = run_files,
                             name = basename(file),
                             name_sans_ext = tools::file_path_sans_ext(name),
                             ext = tools::file_ext(name),
                             datetime = file.info(run_files)$mtime)

      run_xml_file <- run_files_df %>%
        filter(tolower(ext) == "xml") %>%
        arrange(desc(datetime)) %>%
        slice(1)

      xml_file_found <- (nrow(run_xml_file) == 1)

      if (xml_file_found) {

        run_results_files <- run_files_df %>%
          filter(name_sans_ext == run_xml_file$name_sans_ext)

        control_stream_file <- run_results_files %>% filter(ext %in% c("con", "mod", "ctl", "nmctl")) %>% pull(file)
        estimation_file <- run_results_files %>% filter(ext == "ext") %>% pull(file)
        phi_file <- run_results_files %>% filter(ext == "phi") %>% pull(file)
        xml_file <- run_results_files %>% filter(ext == "xml") %>% pull(file)
        report_file <- run_results_files %>% filter(ext %in% c("rep", "lst", "out", "res")) %>% pull(file) %>% first()

        run_name <- run_xml_file$name_sans_ext

        break
      }

      if (!xml_file_found && is.null(control_stream)) {
        stop(simpleError("No XML document found in run files."))
      }

      loop <- loop + 1

      if (loop != 0) {
        print("No XML document found in run files, retrying.")
        Sys.sleep(3)
      }

      if (loop > 3) {
        stop(simpleError("No XML document found in run files"))
      }
    }

    if (is.function(update_progress)) {
      update_progress(detail = "Reading XML data")
    }

    xml_lines <- read_lines(xml_file) %>% na.omit() %>% str_replace_all("\\&", "and") %>% str_trim()

    # XML file parsing
    safe_xml_parsing <- safely(xmlParse)

    load_xml <- safe_xml_parsing(xml_lines,
      encoding = "UTF-8",
      options = 1
    ) # recover on error

    if (!is.null(load_xml$error)) {

      # bug in NONMEM 7.4.1: monitor tag mismatch
      err_msg <- load_xml$error$message
      err_pattern <- "Opening and ending tag mismatch: monitor line (\\d+)"
      if (str_detect(err_msg, err_pattern)) {
        err_line <- str_match_all(err_msg, err_pattern)[[1]][, 2] %>% as.integer()

        # 2nd try
        load_xml <- safe_xml_parsing(xml_lines[-err_line], encoding = "UTF-8")
      }

      if (!is.null(load_xml$error)) {
        stop(load_xml$error)
      }
    }

    xml_report <- load_xml$result

    root_node <- xmlRoot(xml_report)
    nonmem_node <- root_node[["nonmem"]]

    if (is.null(nonmem_node)) {
      stop(simpleError("NONMEM run not recognized: XML result file is invalid."))
    }

    problem_node <- nonmem_node[["problem"]]

    nm_version <- nonmem_node %>% xmlGetAttr(name = "version")

    if (nm_version < numeric_version("7.2")) {
      stop(simpleError(sprintf("NONMEM version %s is not supported, please use NONMEM 7.2 or higher.", nm_version)))
    }

    # report file
    report <- NULL

    if (!is.na(report_file)) {
      report <- read_file(report_file)
    }  # , locale = readr::locale(encoding = "ISO-8859-1"))

    # control stream
    cs_text <- root_node[["control_stream"]] %>% xmlValue()
    # control_stream <- read_file(file = control_stream_file)

    cs_data <- control_stream

    if (is.null(cs_data)) {
      safe_parse_cs <- safely(parse_nm_control_stream)

      safe_cs <- safe_parse_cs(content = cs_text, read_initial_values = read_initial_values, verbose = verbose)

      if (!is.null(safe_cs$error)) {
        stop(simpleError("Error reading control stream file."))
      }

      cs_data <- safe_cs$result
    }

    dv_name <- "DV"

    # if dependent column has another name than default 'DV'
    if (nrow(dv_alias_row <- cs_data$input %>% filter(synonym == "DV"))) {
      dv_name <- dv_alias_row$column
    }

    dataset_file <-
      run_files[str_detect(run_files, fixed(str_replace_all(basename(cs_data$dataset_file), "\\.", "\\\\.")))]

    if (length(dataset_file) == 0 || !file.exists(dataset_file)) {
      # if dataset not found in run folder, look up the file tree
      merged_path <- str_c(run_directory, cs_data$dataset_file, sep = "/")

      if (!file.exists(normalizePath(merged_path))) {
        stop(simpleError(sprintf("Dataset %s not found in run files.", cs_data$dataset_file)))
      }

      dataset_file <- merged_path
    }


    estimations <- NULL
    last_estimation <- NULL
    if (!is.null(problem_node)) {
      # estimations
      estimation_nodes <-
        xmlElementsByTagName(problem_node, "estimation")
      # estimation_nodes <- getNodeSet(problem_node, namespaces = "nm", "//nm:estimation")

      if (is.function(update_progress)) {
        update_progress(detail = "Reading estimation informations")
      }

      # remove CHAIN estimations before digging into results
      useful_est_nodes <- Filter(function(x) {
        meth_node <- xmlElementsByTagName(x, "estimation_method")
        if (length(meth_node) == 0) {
          return(NULL)
        }
        return(xmlValue(meth_node[[1]]) != "chain")
      }, estimation_nodes)

      estimations <- map(seq_along(useful_est_nodes), function(i_node) {
        est_node <- useful_est_nodes[[i_node]]

        term_status <- NULL

        if (!is.null(est_node[["termination_status"]])) {
          term_status <- est_node[["termination_status"]] %>% xmlValue() %>% as.numeric()
        }

        estimation <-
          list(
            number = i_node,
            minimization = !is.null(term_status),
            termination_status = term_status,
            failed = ifelse(is.null(term_status),
              FALSE, # No estimation
              ifelse(is.na(term_status), TRUE, term_status == 8)
            ),
            final_ofv = est_node[["final_objective_function"]] %>% xmlValue() %>% as.numeric(),
            termination_information = est_node[["termination_information"]] %>% xmlValue() %>% str_trim(),
            method = est_node[["estimation_method"]] %>% xmlValue(),
            title = est_node[["estimation_title"]] %>% xmlValue(),
            nfuncevals = est_node[["termination_nfuncevals"]] %>% xmlValue() %>% as.numeric(),
            significant_digits = est_node[["termination_sigdigits"]] %>% xmlValue() %>% as.numeric(),
            eigenratio = NA,
            eigenvalues = NA,
            correlation = NA,
            duration = est_node[["estimation_elapsed_time"]] %>%
              xmlValue() %>%
              as.numeric()
          )

        if (verbose) {
          print(sprintf("Estimation %s (%s)", i_node, estimation$title))
        }

        # <nm:parallel_est nm:parafile='fpilinux8.pnm' nm:protocol='FPI' nm:nodes='8'/>
        parallel_node <- est_node[["parallel_est"]]

        if (!is.null(parallel_node)) {
          estimation$parallel <-
            list(
              protocol = parallel_node %>% xmlGetAttr("protocol"),
              nodes = parallel_node %>% xmlGetAttr("nodes") %>% as.numeric()
            )
        }

        msgs_node <- est_node[["termination_txtmsg"]]

        if (!is.null(msgs_node)) {
          term_msgs <- map_df(xmlChildren(msgs_node), function(x) {
            msg_id <- x %>% xmlValue()

            nonmem_txtmsgs[nonmem_txtmsgs$id == msg_id, ]
          }, .id = NULL)

          estimation$termination_messages <- term_msgs
        }

        # THETAs
        thetas_node <- est_node[["theta"]]
        if (!is.null(thetas_node)) {
          thetas_se_node <- est_node[["thetase"]]

          thetas_ids <-
            paste0("THETA", xmlSApply(thetas_node, function(x)
              xmlGetAttr(x, name = "name")))
          thetas_values <-
            xmlSApply(thetas_node, xmlValue) %>%
            as.numeric()

          if (!is.null(thetas_se_node)) {
            thetas_se <-
              xmlSApply(thetas_se_node, xmlValue) %>%
              as.numeric()

            # theta_se = 10000000000 <=> fixed theta
            thetas_se <- infinite_as_na(thetas_se)
          } else {
            thetas_se <- NA
          }

          # make a data frame...
          thetas_df <- tibble(
            id = thetas_ids,
            estimate = thetas_values,
            se = thetas_se,
            rse = abs(thetas_se / thetas_values),
            ci_low = thetas_values - 2 * thetas_se,
            ci_up = thetas_values + 2 * thetas_se
          )

          estimation$thetas <- thetas_df
        } else {
          estimation$failed <- TRUE
        }

        # SUBPOPs (if mixture model)
        subpop_names <- paste0("SUBPOP", seq_len(cs_data$subpopulations))

        # SHRINKAGEs
        eta_shrinkage <- ebv_shrinkage <- epsilon_shrinkage <- NULL

        # OMEGAs
        # eta bars
        eta_bar_node <- est_node[["etabar"]]
        eta_bar_se_node <- est_node[["etabarse"]]
        eta_bar_n_node <- est_node[["etabarn"]]
        eta_bar_pval_node <- est_node[["etabarpval"]]

        if (all(!is.null(c(eta_bar_node, eta_bar_se_node, eta_bar_n_node, eta_bar_pval_node)))) {
          eta_bar <- load_matrix(eta_bar_node)
          eta_bar_se <- load_matrix(eta_bar_se_node)
          eta_bar_n <- load_matrix(eta_bar_n_node)
          eta_bar_pval <- load_matrix(eta_bar_pval_node)

          etas_names <-
            xmlSApply(
              eta_bar_node %>% xmlChildren() %>% first(),
              function(x)
                xmlGetAttr(x, name = "cname")
            ) %>%
            unname()

          etas_id_col <- rep(etas_names, cs_data$subpopulations)

          eta_bars_df <- tibble(
            id = etas_id_col,
            value = eta_bar[, 1],
            se = eta_bar_se[, 1]
          )

          if (!is.null(eta_bar_n)) {
            eta_bars_df$n <- eta_bar_n[, 1]
          }

          if (cs_data$subpopulations > 1) {
            eta_bars_df$subpop <- rep(subpop_names, each = length(etas_names))
          }

          if (!is.null(eta_bar_pval)) {
            eta_bars_df$pvalue <- eta_bar_pval[, 1]
          }

          estimation$eta_bars <- eta_bars_df
        }

        omega_node <- est_node[["omega"]]
        omegase_node <- est_node[["omegase"]]
        etas_ids <- NULL
        if (!is.null(omega_node)) {
          omega_matrix <- load_lower_triangular_matrix(omega_node)

          etas_ids <- setNames(seq_len(nrow(omega_matrix)), paste0("ETA", seq_len(nrow(omega_matrix))))
          colnames(omega_matrix) <- rownames(omega_matrix) <- names(etas_ids)

          omega_df <- load_matrix_estimate_table(
            estimate_matrix_node = est_node[["omega"]],
            se_matrix_node = omegase_node
          ) %>%
            rename(eta1 = row, eta2 = col) %>%
            mutate(
              eta1 = plyr::mapvalues(eta1, from = etas_ids, to = names(etas_ids), warn_missing = FALSE),
              eta2 = plyr::mapvalues(eta2, from = etas_ids, to = names(etas_ids), warn_missing = FALSE)
            )

          omegac_df <- load_matrix_estimate_table(
            estimate_matrix_node = est_node[["omegac"]],
            se_matrix_node = est_node[["omegacse"]]
          ) %>%
            rename(eta1 = row, eta2 = col) %>%
            mutate(
              eta1 = plyr::mapvalues(eta1, from = etas_ids, to = names(etas_ids), warn_missing = FALSE),
              eta2 = plyr::mapvalues(eta2, from = etas_ids, to = names(etas_ids), warn_missing = FALSE)
            )

          fixed_omegas_indices <- which(omega_df$estimate == 2.0000000000000000E-012)

          if (length(fixed_omegas_indices) > 0) {
            message(paste("Fixed omega indices", paste(fixed_omegas_indices, collapse = ", ")))
          }

          fixed_omegas <- setdiff(
            cs_data$parameters$definition %>% filter(type == "eta") %>% pull(id),
            unique(c(omega_df$eta1, omega_df$eta2))
          )

          estimation$omega <- omega_df %>%
            filter(!(row_number() %in% fixed_omegas_indices)) %>%
            mutate(cv = purrr::pmap_dbl(
              list(eta1 = eta1, eta2 = eta2, estimate = estimate),
              function(eta1, eta2, estimate) {
                ifelse(eta1 == eta2, sqrt(exp(estimate) - 1), NA_real_)
              }
            ))
          # mutate(cv = ifelse(eta1 == eta2, sqrt(exp(estimate) - 1), 0))

          estimation$omegac <- omegac_df %>% filter(!(row_number() %in% fixed_omegas_indices))
          estimation$omega_matrix <- omega_matrix

          if (!is.null(omegase_node)) {
            omegase_matrix <- load_lower_triangular_matrix(omegase_node)
            colnames(omegase_matrix) <- rownames(omegase_matrix) <- names(etas_ids)

            estimation$omegase_matrix <- omegase_matrix
          }

          # ETA EBV shrinkages
          eta_shrink_tag <- ifelse(nm_version < numeric_version("7.4"), "etashrink", "etashrinksd")
          ebv_shrink_tag <- ifelse(nm_version < numeric_version("7.4"), "ebvshrink", "ebvshrinksd")

          eta_shrinkage_matrix <- load_matrix(est_node[[eta_shrink_tag]])
          ebv_shrinkage_matrix <- load_matrix(est_node[[ebv_shrink_tag]])

          if (!is.null(eta_shrinkage_matrix)) {
            eta_shrinkage <- tibble(parameter = etas_id_col, shrinkage = eta_shrinkage_matrix[, 1] / 100)

            if (cs_data$subpopulations > 1) {
              eta_shrinkage$subpop <- rep(subpop_names, each = length(etas_names))
            }
          }
          if (!is.null(ebv_shrinkage_matrix)) {
            ebv_shrinkage <- tibble(parameter = etas_id_col, shrinkage = ebv_shrinkage_matrix[, 1] / 100)

            if (cs_data$subpopulations > 1) {
              ebv_shrinkage$subpop <- rep(subpop_names, each = length(etas_names))
            }
          }
        }

        # SIGMAs
        sigma_node <- est_node[["sigma"]]
        sigmase_node <- est_node[["sigmase"]]
        eps_ids <- NULL
        if (!is.null(sigma_node)) {
          sigma_matrix <- load_lower_triangular_matrix(sigma_node)

          eps_ids <- setNames(seq_len(nrow(sigma_matrix)), paste0("EPS", seq_len(nrow(sigma_matrix))))
          colnames(sigma_matrix) <- rownames(sigma_matrix) <- names(eps_ids)

          sigma_df <- load_matrix_estimate_table(
            estimate_matrix_node = est_node[["sigma"]],
            se_matrix_node = sigmase_node
          ) %>%
            rename(epsilon1 = row, epsilon2 = col) %>%
            mutate(
              epsilon1 = plyr::mapvalues(epsilon1, from = eps_ids, to = names(eps_ids), warn_missing = FALSE),
              epsilon2 = plyr::mapvalues(epsilon2, from = eps_ids, to = names(eps_ids), warn_missing = FALSE)
            )

          sigmac_df <- load_matrix_estimate_table(
            estimate_matrix_node = est_node[["sigmac"]],
            se_matrix_node = est_node[["sigmacse"]]
          ) %>%
            rename(epsilon1 = row, epsilon2 = col, sigmac_square = estimate) %>%
            mutate(
              epsilon1 = plyr::mapvalues(epsilon1, from = eps_ids, to = names(eps_ids), warn_missing = FALSE),
              epsilon2 = plyr::mapvalues(epsilon2, from = eps_ids, to = names(eps_ids), warn_missing = FALSE)
            )

          estimation$sigma <- sigma_df
          estimation$sigmac <- sigmac_df
          estimation$sigma_matrix <- sigma_matrix

          if (!is.null(sigmase_node)) {
            sigmase_matrix <- load_lower_triangular_matrix(sigmase_node)
            colnames(sigmase_matrix) <- rownames(sigmase_matrix) <- names(eps_ids)

            estimation$sigmase_matrix <- sigmase_matrix
          }

          # EPS shrinkage
          eps_shrink_tag <- ifelse(nm_version < numeric_version("7.4"), "epsshrink", "epsshrinksd")
          eps_shrinkage_matrix <- load_matrix(est_node[[eps_shrink_tag]])

          eps_id_col <- rep(names(eps_ids), cs_data$subpopulations)

          if (!is.null(eps_shrinkage_matrix)) {
            epsilon_shrinkage <- tibble(
              parameter = eps_id_col,
              shrinkage = eps_shrinkage_matrix[, 1] / 100
            )

            if (cs_data$subpopulations > 1) {
              epsilon_shrinkage$subpop <- rep(subpop_names, each = length(eps_ids))
            }
          }
        }

        # gathering SHRINKAGEs
        if (!all(map_lgl(list(eta_shrinkage, ebv_shrinkage, epsilon_shrinkage), is.null))) {
          estimation$shrinkage <- bind_rows(list(
            ETA = eta_shrinkage,
            EBV = ebv_shrinkage,
            EPS = epsilon_shrinkage
          ),
          .id = "type"
          )
        }

        # covariance
        covariance_matrix <- NULL
        covariance_node <- est_node[["covariance"]]

        if (!is.null(covariance_node)) {
          covariance_matrix <- load_lower_triangular_matrix(covariance_node)

          # item names
          cov_m_names <- map(
            covariance_node %>%
              xmlChildren() %>%
              last() %>%
              xmlChildren(),
            function(x)
              xmlGetAttr(x, "cname")
          ) %>% unlist(use.names = FALSE)

          colnames(covariance_matrix) <- rownames(covariance_matrix) <- cov_m_names
        }

        estimation$covariance_matrix <- covariance_matrix

        # covariance
        correlation_matrix <- NULL
        correlation_node <- est_node[["correlation"]]

        if (!is.null(correlation_node)) {
          correlation_matrix <- load_lower_triangular_matrix(correlation_node)

          # item names
          cov_m_names <- map(
            correlation_node %>%
              xmlChildren() %>%
              last() %>%
              xmlChildren(),
            function(x)
              xmlGetAttr(x, "cname")
          ) %>% unlist(use.names = FALSE)

          colnames(correlation_matrix) <- rownames(correlation_matrix) <- cov_m_names

          # look for correlation between parameters
          temp_correlation_matrix <- correlation_matrix
          diag(temp_correlation_matrix) <- NA

          estimation$correlation <- any(!is.na(temp_correlation_matrix) & temp_correlation_matrix >= 0.96)
        }

        estimation$correlation_matrix <- correlation_matrix

        # eigenvalues
        eigenvalues_node <- est_node[["eigenvalues"]]

        if (!is.null(eigenvalues_node)) {
          eigenvalues <-
            xmlSApply(eigenvalues_node, xmlValue) %>%
            as.numeric()

          estimation$eigenvalues <- eigenvalues
          estimation$eigenratio <- max(eigenvalues) / min(eigenvalues)
        }

        if (estimation$failed) {
          warning(simpleWarning(sprintf("Estimation %s (%s) failed.", estimation$number, estimation$method)))
        }

        estimation
      })

      not_failed_estimations <- Filter(function(x) {
        !x$failed
      }, estimations)

      last_estimation <- last(not_failed_estimations)
    }

    params_df <- cs_data$parameters$definition

    # if not explicitly written ETAXXX = ETA(1)... :
    params_df <- params_df %>%
      mutate(
        name = ifelse(is.na(name), id, name),
        n = ifelse(is.na(n), str_extract(id, "[0-9]+$"), n),
        column = NA
      )

    etas_df <- params_df %>% filter(type == "eta")

    consistent_omega_dim <- TRUE
    if (!is.null(last_estimation)) {
      omega_params_df <- sigma_params_df <- NULL

      if (!is.null(last_estimation$omega_matrix)) {
        if (nrow(etas_df) != ncol(last_estimation$omega_matrix)) {
          consistent_omega_dim <- FALSE
          warning(simpleWarning(sprintf(
            "The number of ETAs (%s) declared is not consistent with OMEGA matrix dimensions (%s).",
            nrow(etas_df), paste(rep(ncol(last_estimation$omega_matrix), 2), collapse = "x")
          )))
        }

        omega_n <- seq_len(ncol(last_estimation$omega_matrix))
        if (is_matrix_diagonal(last_estimation$omega_matrix)) {
          omega_x <- omega_n
          omega_y <- omega_n
        } else {
          omega_x <- rep(omega_n, omega_n)
          omega_y <- map(omega_n, function(x) 1:(omega_n)[x]) %>% unlist()
        }
        omega_x_names <- plyr::mapvalues(omega_x, from = etas_df$n, to = etas_df$name)
        omega_y_names <- plyr::mapvalues(omega_y, from = etas_df$n, to = etas_df$name)
        omega_ids <- sprintf("OMEGA(%s,%s)", omega_x, omega_y)

        omega_params_df <- tibble(
          id = sprintf("OMEGA(%s,%s)", omega_x, omega_y),
          type = "omega",
          name = sprintf("OMEGA(%s,%s)", omega_x_names, omega_y_names)
        )
      }

      if (!is.null(last_estimation$sigma_matrix)) {
        sigma_n <- seq_len(ncol(last_estimation$sigma_matrix))
        if (is_matrix_diagonal(last_estimation$sigma_matrix)) {
          sigma_x <- sigma_n
          sigma_y <- sigma_n
        } else {
          sigma_x <- rep(sigma_n, sigma_n)
          sigma_y <- map(sigma_n, function(x) 1:(sigma_n)[x]) %>% unlist()
        }

        sigma_params_df <- tibble(
          id = sprintf("SIGMA(%s,%s)", sigma_x, sigma_y),
          type = "sigma",
          name = sprintf("SIGMA(EPS%s,EPS%s)", sigma_x, sigma_y)
        )
      }

      params_df <- params_df %>% bind_rows(omega_params_df, sigma_params_df)
    }

    temp_p_df <- params_df %>% # look for duplicated names (e.g. IOV ETAs)
      group_by(name) %>%
      mutate(temp_name = pmap_chr(
        list(dplyr::n(), row_number(), name),
        ~ifelse(..1 > 1, str_c(name, "_", ..2), ..3)
      )) %>%
      ungroup()

    # update omega matrix and omega tables row/col names
    estimations <- map(estimations, function(x) {
      if (!is.null(x$thetas)) {
        x$thetas <- x$thetas %>%
          mutate(name = plyr::mapvalues(id, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE)) %>%
          select(id, name, everything())
      }

      if (!is.null(x$eta_bars)) {
        x$eta_bars <- x$eta_bars %>%
          mutate(name = plyr::mapvalues(id, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE)) %>%
          select(id, name, everything())
      }

      if (!is.null(x$omega)) {
        x$omega <- x$omega %>%
          mutate(
            eta1 = plyr::mapvalues(eta1, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE),
            eta2 = plyr::mapvalues(eta2, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE)
          )
      }

      if (!is.null(x$omegac)) {
        x$omegac <- x$omegac %>%
          mutate(
            eta1 = plyr::mapvalues(eta1, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE),
            eta2 = plyr::mapvalues(eta2, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE)
          )
      }

      if (!is.null(x$sigma)) {
        x$sigma <- x$sigma %>%
          mutate(
            epsilon1 = plyr::mapvalues(epsilon1, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE),
            epsilon2 = plyr::mapvalues(epsilon2, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE)
          )
      }

      if (!is.null(x$sigmac)) {
        x$sigmac <- x$sigmac %>%
          mutate(
            epsilon1 = plyr::mapvalues(epsilon1, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE),
            epsilon2 = plyr::mapvalues(epsilon2, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE)
          )
      }

      if (!is.null(x$shrinkage)) {
        x$shrinkage <- x$shrinkage %>%
          mutate(parameter = plyr::mapvalues(parameter, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE))
      }

      if (!is.null(x$omega_matrix) && consistent_omega_dim) {
        colnames(x$omega_matrix) <- temp_p_df %>% filter(type == "eta") %>% pull(temp_name)
        rownames(x$omega_matrix) <- temp_p_df %>% filter(type == "eta") %>% pull(temp_name)
      }

      x
    })

    if (length(estimation_file) == 1 && file.exists(estimation_file)) {
      ext_data <- parse_nm_ext(filepath = estimation_file)

      if (!is.null(ext_data) && nrow(ext_data) > 0) {
        ext_data <- ext_data %>% filter(problem == 1)

        merge_ext_data <- function(x) {
          iterations_data <- ext_data %>% filter(number == x$number) %>% pull(iterations) %>% .[[1]]

          if (nrow(iterations_data) == 0) return(NULL)

          iterations_data <- iterations_data %>%
            filter(ITERATION > -1e9) %>% # remove last lines
            mutate(row_index = row_number())

          it_params <- colnames(iterations_data)[colnames(iterations_data) %in% params_df$id]
          names(it_params) <- plyr::mapvalues(it_params, from = params_df$id, to = temp_p_df$temp_name, warn_missing = FALSE)

          iterations_data %>%
            select(-row_index) %>%
            rename(!!!it_params)
        }
        estimations <- estimations %>%
          modify_if(~.$number %in% ext_data$number, ~update_list(., iterations = ~merge_ext_data(.)))
      }
    }

    if (length(phi_file) == 1 && file.exists(phi_file)) {
      phi_data <- parse_nm_phi(filepath = phi_file)

      if (!is.null(phi_data) && nrow(phi_data) > 0) {
        phi_data <- phi_data %>% filter(problem == 1)

        if (!is.null(cs_data$covariance$sir)) { # do not read the SIR table
          phi_data <- phi_data %>% filter(method != "Importance Sampling of Variance-Covariance (SIR)")
        }

        # for(est_i in phi_data$number){
        #   estimations[[est_i]]$phi <- phi_data[est_i,]$individuals[[1]]
        # }

        estimations <- estimations %>%
          modify_if(~.$number %in% phi_data$number, ~update_list(., phi = ~phi_data[.$number, ]$individuals[[1]]))
      }
    }

    if (is.function(update_progress)) {
      update_progress(detail = basename(dataset_file))
    }


    # readr--------------
    data_lines <- read_lines(dataset_file)
    data_lines <- data_lines %>% str_trim()
    first_line_index <- 1


    lines_to_ignore <- vector("integer")
    lines_to_keep <- vector("integer")
    ignored_lines <- vector("character")

    if (cs_data$ignore$C) {
      lines_to_ignore <- which(!str_detect(data_lines, "^\\s*[^C]"))
    }

    if (cs_data$ignore$`#`) {
      lines_to_ignore <- c(lines_to_ignore, which(!str_detect(data_lines, "^\\s*[^#]")))
    }

    if (cs_data$ignore$`@`) {
      # id_lines_to_remove <- which(!str_detect(substr(data_lines, 1, 1), "[0-9\\.]"))
      # id_lines_to_remove <- str_which(data_lines, "^[a-zA-Z]")
      id_lines_to_remove <- str_which(data_lines, "^[^0-9]")

      lines_to_ignore <- c(lines_to_ignore, id_lines_to_remove)
    }

    if (!is.null(cs_data$ignore$txt) && !any(c(cs_data$ignore$C, cs_data$ignore$`#`, cs_data$ignore$`@`))) {
      lines_to_ignore <- which(!str_detect(data_lines, sprintf("^\\s*[^%s]", cs_data$ignore$txt)))
    }

    lines_to_keep <- setdiff(seq_along(data_lines), lines_to_ignore)

    if (length(lines_to_ignore) > 0) {
      ignored_lines <- data_lines[lines_to_ignore]
      data_lines <- data_lines[-lines_to_ignore]
    }

    first_data_line <- data_lines[first_line_index]

    if (is.na(dataset_separator)) {
      dataset_separator <- ifelse(str_detect(first_data_line, ","),
        ",",
        ifelse(str_detect(first_data_line, "\t"),
          "\t",
          " "
        )
      )
    }

    ds_readr_func <- switch(dataset_separator,
      "," = readr::read_csv,
      "\t" = readr::read_tsv,
      " " = readr::read_table2, {
        function(...) readr::read_delim(delim = dataset_separator, ...)
      }
    )

    has_header <- str_detect(str_trim(first_data_line), "[A-Za-z]")

    input_selected_cols <- cs_data$input %>% filter(!dropped)
    dropped_cols <- cs_data$input %>% filter(dropped)

    # analyse header
    if ((first_kept_line <- lines_to_keep[1]) > 1) {
      top_lines <- ignored_lines[lines_to_ignore < first_kept_line]

      ds_file_colnames <- vector("character")
      potential_colnames <- na.omit(c(input_selected_cols$column, input_selected_cols$synonym))

      for (t_line in rev(top_lines)) {
        # if(cs_data$ignore$C)
        #   t_line <- t_line %>% str_replace("^\\s*C", "")

        # ds_file_colnames <- str_split(t_line, dataset_separator) %>% unlist() %>% toupper()
        ds_file_colnames <- str_split(t_line, boundary("word")) %>% unlist() %>% toupper()

        # if half of the potential colnames (from $INPUT) are found in the lines, consider that it is the header
        if (mean(map_lgl(potential_colnames, ~any(str_detect(ds_file_colnames, word(.))))) > 0.5) {
          break
        }
      }

      if (length(ds_file_colnames) < nrow(cs_data$input)) {
        cs_data$input <- cs_data$input %>% filter(column %in% ds_file_colnames |
          synonym %in% ds_file_colnames)
      }
    }

    keep_indices <- which(!cs_data$input$dropped)


    dataset <- ds_readr_func(paste(data_lines, collapse = "\n"),
      na = dataset_na_string,
      guess_max = length(data_lines),
      # col_types = cols(.default = col_number()),
      col_names = FALSE
    )

    keep_indices <- keep_indices[keep_indices <= ncol(dataset)]

    dataset <- dataset %>% select(keep_indices)
    colnames(dataset) <- cs_data$input$column[keep_indices]

    cols_check <- input_selected_cols %>% mutate(is_present = column %in% colnames(dataset))

    if (any(!cols_check$is_present)) {
      # get the column from the column order in the dataset
      temp_df <- cols_check %>%
        mutate(n = row_number())

      cols_not_in_dataset <- temp_df %>%
        filter(!is_present) %>%
        mutate(source_column = ifelse(colnames(dataset)[n] %in% cols_check$column,
          NA,
          colnames(dataset)[n]
        ))

      found_columns <- cols_not_in_dataset %>% filter(!is.na(source_column))
      missing_columns <- cols_not_in_dataset %>% filter(is.na(source_column))

      if (nrow(missing_columns) > 0) {
        warning(simpleWarning(str_c("$INPUT", str_c(missing_columns$column, collapse = ", "), "not found in dataset.", sep = " ")))
      }

      if (nrow(found_columns) > 0) {
        input_selected_cols[found_columns$n, ]$column <- found_columns$source_column
        input_selected_cols[found_columns$n, ]$synonym <- found_columns$column
      }

      input_selected_cols <- input_selected_cols %>% filter(!(column %in% missing_columns$column))
    }

    columns_with_synonym <- input_selected_cols %>% filter(!is.na(synonym))
    rename_col <- columns_with_synonym %>% filter(column %in% colnames(dataset) & !(column %in% nm_reserved_names))

    c_to_rename <- setNames(rename_col$column, nm = rename_col$synonym)

    # rename column with their synonym
    if (length(c_to_rename) > 0) {
      dataset <- dataset %>%
        mutate_at(.vars = vars(c_to_rename), as.numeric) %>%
        rename(!!!c_to_rename)
    }

    if (!is.null(cs_data$ignore$data)) {
      ignore_quos <- map(seq_len(nrow(cs_data$ignore$data)), function(i) {
        ignore_cond <- cs_data$ignore$data[i, ]
        rhs_val <- ifelse(is.character(dataset[[ignore_cond$column]]), sprintf("'%s'", ignore_cond$value), ignore_cond$value)
        formula_text <- sprintf("is.na(%s) | !(%s %s %s)", ignore_cond$column, ignore_cond$column, ignore_cond$operator, rhs_val)

        parse_quo(formula_text, env = caller_env())
      })

      dataset <- dataset %>% filter(UQS(ignore_quos))
    }

    # add EVID column if missing
    if (!("EVID" %in% colnames(dataset))) {
      dataset <- dataset %>% mutate(EVID = ifelse(is.na(DV), 1L, 0L))
    }

    # Fix EVID column if is na -> observation event
    dataset <- dataset %>% mutate(EVID = ifelse(is.na(EVID), 0L, EVID))

    # add MDV column if missing
    if (!("MDV" %in% colnames(dataset))) {
      dataset <- dataset %>% mutate(MDV = ifelse(EVID == 0L, 0L, 1L))
      # dataset$MDV <- 0L#factor(0, levels = c(0, 1))
    } else {
      dataset <- dataset %>%
        mutate(MDV = as.integer(ifelse(is.na(MDV),
          ifelse(is.na(DV), 1L, 0L),
          MDV
        ))) # %>%
      # mutate(MDV = as.factor(MDV))
    }


    nrow_ds <- nrow(dataset)

    run_tables <- list()
    covariates_df <- NULL
    cat_covs_levels <- NULL
    # loading tables
    if (load_tables && !is.null(cs_data$tables) && nrow(cs_data$tables) > 0) {
      run_tables <- cs_data$tables %>%
        split(.$file) %>%
        map(function(x) {
          tab <- x$file

          file_pattern <- str_replace_all(tab, "\\.", "\\\\.")

          file_detection <- str_detect(run_files, file_pattern)

          file_path <- NULL

          if (any(file_detection)) {
            file_path <- first(run_files[file_detection])
          }

          if (!is.null(file_path) && file.exists(file_path)) {
            if (is.function(update_progress)) {
              update_progress(detail = sprintf("%s table", tab))
            }

            n_skip <- as.integer(!x$notitle) + as.integer(!x$noheader)

            # readr way
            # tab_readr_func <- switch(str_sub(x$format, 1, 1),
            #                     "," = readr::read_csv,
            #                     "t" = readr::read_tsv,
            #                     readr::read_table2)
            # base way
            tab_readr_func <- switch(str_sub(x$format, 1, 1),
              "," = read.csv,
              "t" = read.delim,
              read.table
            )

            # remove the repetition of the headers every 900 rows
            # if(!x$noheader & !x$oneheader){
            tab_lines <- read_lines(file_path) %>%
              str_subset("^\\s*[0-9]")

            con <- textConnection(
              str_c(tab_lines, collapse = "\n")
            )

            safe_read <- safely(tab_readr_func)

            tab_data <- safe_read(con,
              header = FALSE,
              quote = "\"", comment.char = "", na.strings = ".",
              check.names = FALSE
            )

            if (!is.null(tab_data$error)) {
              warning(simpleWarning(sprintf("%s could not be read: %s", tab, tab_data$error$message)))
              return(NULL)
            }

            tab_data <- tab_data$result

            tab_col_names <- unlist(x$columns)

            if (!x$noappend) {
              # tab_col_names <- tab_col_names[tab_col_names != "PRED"] # NONMEM omits PRED column in the table if it appends it also at the end...
              append_cols <- c(dv_name, "PRED", "RES", "WRES")

              id_append <- which(tab_col_names %in% c("PRED", "RES", "WRES"))

              if (last(tab_col_names) == dv_name) {
                # NONMEM omits DV column if it is the last requested column
                tab_col_names <- tab_col_names[-length(tab_col_names)]
              }

              if (length(id_append) > 0) {
                tab_col_names <- tab_col_names[-id_append]
              }

              tab_col_names <- c(tab_col_names, append_cols)
            }

            if (length(tab_col_names) != ncol(tab_data)) {
              warning(simpleWarning(sprintf("%s could not be read.", tab)))
              return(NULL)
            }

            colnames(tab_data) <- tab_col_names

            # remove potential duplicated columns
            col_names <- colnames(tab_data)

            if (dv_name != "DV" && all(c(dv_name, "DV") %in% col_names)) {
              tab_data <- tab_data %>% select(-DV)
            }

            dup_columns <- which(duplicated(col_names))

            if (length(dup_columns) > 0) {
              tab_data <- tab_data[, -dup_columns]
            }

            if (dv_name != "DV" && !x$noappend) {
              tab_data <- tab_data %>% rename(!!!setNames(dv_name, "DV"))
            }

            nm_tab <- tab_data %>% convert_nm_table()

            nm_tab
          } else {
            warning(simpleWarning(sprintf("%s is missing.", tab)))

            NULL
          }
        })

      run_tables <- compact(run_tables)

      # column checks
      required_columns <- c("ID", "TIME", "DV")
      map(required_columns, function(x) {
        if (!(x %in% colnames(dataset))) {
          stop(simpleError(paste(x, "column is required.")))
        }
      })
    }

    compartments <- cs_data$model_compartments

    if (is.null(compartments)) { # if $PRED subroutine
      n_cmt <- 1L

      if ("CMT" %in% colnames(dataset)) {
        n_cmt <- max(dataset$CMT)
      }

      compartments <- tibble(cmt = seq_len(n_cmt), name = str_c("CMT_", cmt))
    }

    if ("CMT" %in% colnames(dataset)) {
      dataset <- dataset %>%
        mutate(CMT = abs(CMT)) # because of NONMEM "disabling CMT"
    } else {
      subrout <- ifelse(!is.null(cs_data$subroutine$ADVAN), cs_data$subroutine$ADVAN, "PRED")
      default_dv_cmt <- switch(subrout,
        "ADVAN1" = 1,
        "ADVAN10" = 1,
        "ADVAN2" = 2,
        "ADVAN3" = 1,
        "ADVAN4" = 2,
        "ADVAN11" = 1,
        "ADVAN12" = 2,
        1
      )

      dataset$CMT <- default_dv_cmt
    }

    # update compartments info
    dv_cmts <- dataset %>%
      filter(!is.na(DV) & MDV == 0 & CMT != 0) %>%
      select(CMT) %>%
      unique() %>%
      unlist(use.names = FALSE)

    compartments$dv_target <- compartments$cmt %in% dv_cmts

    # NONMEM output compartment used ? -> CMT with number = number of compartments + 1
    output_cmt <- nrow(compartments) + 1
    if (output_cmt %in% as.numeric(as.character(dv_cmts))) {
      compartments <- bind_rows(
        compartments,
        tibble(
          cmt = output_cmt, name = "Output", dv_target = TRUE
        )
      )
    }


    dataset <- dataset %>% convert_nm_table()

    # merged table: will contains all columns
    pmxploitab <- dataset

    for (i in seq_along(run_tables)) {
      tab <- run_tables[[i]]

      # populate table set with FIRSTONLY option
      if (cs_data$tables[cs_data$tables$file == names(run_tables)[i], ]$firstonly) {
        tab <- tab %>% left_join(select(dataset, ID), by = "ID")
      }

      tab_col_names <- colnames(tab)
      new_col_names <-
        tab_col_names[!(tab_col_names %in% c(colnames(pmxploitab), columns_with_synonym$synonym))] # , columns_with_synonym$column))]

      if (length(new_col_names) > 0) {
        if (nrow(tab) == nrow(pmxploitab)) {
          pmxploitab <- pmxploitab %>%
            bind_cols(select(tab, one_of(new_col_names)))
        } else {
          if(length(estimations) > 0) # do not warn for simulations runs
            warning(simpleWarning(sprintf("%s's number of rows is different than the dataset's.", names(run_tables)[i])))
        }
      }
    }

    names(run_tables) <- tolower(names(run_tables))

    # look for individual parameters in patab table (Xpose convention)
    if (!is.null(run_tables$patab)) {
      all_cols <- colnames(run_tables$patab)

      params_df <- params_df %>%
        mutate(column = ifelse(type == "eta", ifelse(name %in% all_cols, name, id), NA))

      par_names <- setdiff(all_cols, c(
        params_df$id,
        params_df$name,
        na.omit(params_df$column),
        nm_reserved_names,
        columns_with_synonym$synonym
      ))

      if (length(par_names) > 0) {
        individual_params_df <- tibble(
          id = par_names, type = "individual", name = par_names,
          column = par_names
        )

        params_df <- bind_rows(params_df, individual_params_df)
      }

      params_df <- params_df %>%
        mutate(column = ifelse(!is.na(column),
          ifelse(id %in% colnames(run_tables$patab) |
            column %in% colnames(run_tables$patab),
          column, NA
          ), NA
        ))
    } else {
      # look for ETAs in another tab
      temp_etas_df <- params_df %>%
        mutate(n_row = row_number()) %>%
        filter(type == "eta" & is.na(column))

      if (nrow(temp_etas_df) > 0) {
        temp_etas_df <- temp_etas_df %>%
          mutate(short_name = paste0(substring("ETA", 1, 4 - nchar(n)), n)) %>%
          mutate(column = ifelse(name %in% colnames(pmxploitab), name,
            ifelse(short_name %in% colnames(pmxploitab), short_name, column)
          )) %>%
          filter(!is.na(column)) %>%
          select(-short_name)
      }

      if (any(!is.na(temp_etas_df$column))) {
        params_df[temp_etas_df$n_row, ]$column <- temp_etas_df$column
      }
    }

    params_df$id <- factor(params_df$id, levels = unique(params_df$id))
    params_df$type <- factor(params_df$type, levels = unique(params_df$type))

    params_df <- params_df %>%
      arrange(type, id)

    covariates_df <- tibble(
      column = character(),
      type = character(),
      name = character()
    )

    # look for categorical covariates in cotab table (Xpose convention)
    if (!is.null(run_tables$catab)) {
      all_cols <- setdiff(colnames(run_tables$catab), columns_with_synonym$synonym)

      cat_cov_names <- setdiff(all_cols, nm_reserved_names)

      if (length(cat_cov_names) > 0) {
        cat_covs_df <- tibble(column = cat_cov_names, type = "categorical", name = cat_cov_names)
        covariates_df <- bind_rows(covariates_df, cat_covs_df)
      }
    }
    # look for continuous covariates in cotab table (Xpose convention)
    if (!is.null(run_tables$cotab)) {
      all_cols <- setdiff(colnames(run_tables$cotab), columns_with_synonym$synonym)

      cont_cov_names <- setdiff(all_cols, nm_reserved_names)

      if (length(cont_cov_names) > 0) {
        cont_covs_df <- tibble(column = cont_cov_names, type = "continuous", name = cont_cov_names)
        covariates_df <- bind_rows(covariates_df, cont_covs_df)
      }
    }

    # if covariates columns are in patab, remove theme from parameters
    if (any(covariates_df$column %in% params_df$column)) {
      params_df <- params_df %>%
        filter(!(type == "individual" & (column %in% covariates_df$column)))
    }

    covariates_df$type <- factor(covariates_df$type, levels = c("categorical", "continuous"))

    covariates_df <- covariates_df %>%
      arrange(type, name)


    independent_variables <- tibble(column = character(), name = character(), unit = character())
    prediction_types <- NULL
    residual_types <- NULL

    if (load_tables) {
      pmxploitab_cols <- colnames(pmxploitab)

      if (any(c("TIME", "TAD") %in% pmxploitab_cols)) {
        independent_variables <- independent_variables %>%
          add_row(
            column = intersect(c("TIME", "TAD"), pmxploitab_cols),
            name = column,
            unit = NA_character_
          )
      }

      prediction_types <- intersect(nm_predictions, pmxploitab_cols)

      residual_types <- intersect(nm_residuals, pmxploitab_cols)
    }

    n_obs <- NA

    if ("DV" %in% colnames(dataset)) {
      n_obs <- dataset %>% filter(!is.na(DV) & MDV != 1) %>% nrow()
    }

    run_tables$dataset <- dataset

    r_number <- str_extract(basename(run_name), "[0-9]+$")

    n_nodes <- NA

    if (length(estimations) > 0) {
      n_nodes <- estimations %>% map_dbl(~ifelse(!is.null(.$parallel), .$parallel$nodes, 1)) %>% max()
    }

    run_short_name <- basename(run_name)
    run_id <- ifelse(!is.na(r_number), r_number, run_short_name)

    info <- list(
      run_id = run_id,
      run_name = run_short_name,
      run_number = as.integer(r_number),
      path = run_directory,
      problem = cs_data$problem,
      control_stream_file = basename(control_stream_file),
      dataset_file = basename(dataset_file),
      report_file = basename(report_file),
      nodes = n_nodes,
      nm_version = nm_version,
      nm_tran_warnings = root_node[["nmtran"]] %>% xmlValue(),
      number_of_subjects = dataset$ID %>% unique() %>% length(),
      number_of_observations = n_obs,
      start_time = root_node[["start_datetime"]] %>%
        xmlValue() %>%
        ymd_hms(),
      stop_time = root_node[["stop_datetime"]] %>%
        xmlValue() %>%
        ymd_hms()
    )

    duration_interval <- interval(info$start_time, info$stop_time)

    info$duration <- duration_interval / seconds(1)

    compartments <- mutate(compartments, type = "continuous", unit = NA_character_)
    parameters <- mutate(params_df, unit = NA_character_)
    covariates <- mutate(covariates_df, unit = NA_character_)

    model <- list(
      compartments = compartments,
      parameters = params_df,
      covariates = covariates_df,
      independent_variables = independent_variables,
      predictions = prediction_types,
      residuals = residual_types
    )

    files <- list(
      control_stream = control_stream,
      report = report
    )

    # AIC & BIC
    # Set number of parameters as : n (estimated theta) + n (estimated eta) + n (estimated epsilon)
    estimations <- estimations %>%
      modify(~update_list(.,
        n_parameters = ~ifelse(!is.null(.$thetas), nrow(.$thetas), 0L) + # n (estimated theta)
          ifelse(!is.null(.$omega), nrow(filter(.$omega, eta1 == eta2)), 0L) + # + n (estimated eta)
          ifelse(!is.null(.$sigma), nrow(filter(.$sigma, epsilon1 == epsilon2)), 0L)
      )) # n (estimated epsilon)

    estimations <- estimations %>%
      modify(~update_list(.,
        aic = ~.$final_ofv + 2L * .$n_parameters,
        bic = ~.$final_ofv + log(n_obs) * .$n_parameters
      ))

    # set estimation steps names
    names(estimations) <- map_chr(estimations, ~str_c(.$number, .$method, sep = ": "))

    mdata_file <- paste0(run_directory, "/pmxploit_metadata.rds")

    attr(model, "pmxploit_version") <- packageDescription("pmxploit")$Version

    if (file.exists(mdata_file)) {
      model_copy <- model
      model <- readRDS(mdata_file)

      if (!identical(attr(model, "pmxploit_version"), packageDescription("pmxploit")$Version)) {
        warning(simpleWarning("Run metadata were saved with a different version of pmxploit package, you may need to reset it (delete `pmxploit_metadata.rds` file)"))
      }

      model$internal <- model_copy
      model$has_metadata <- TRUE
    } else {
      model$has_metadata <- FALSE
    }

    # look for duplicated covariates in catab and cotab
    if (any(dup_covs <- duplicated(model$covariates$column))) {
      dup_covs_names <- model$covariates[dup_covs, ]$column

      covs_summary <- pmxploitab %>%
        select(one_of(dup_covs_names)) %>%
        summarise_all(funs(length(unique(.)))) %>%
        gather(column, n_unique) %>%
        mutate(type = factor(ifelse(n_unique <= 20, "categorical", "continuous"),
          levels = c("categorical", "continuous")
        ), name = column) %>%
        select(-n_unique)

      message(simpleMessage(sprintf(
        "The following covariate(s) found both in catab and cotab: %s will be considered as: %s%s.\n",
        paste(covs_summary$column, collapse = "/"),
        paste(covs_summary$type, collapse = "/"),
        ifelse(length(dup_covs_names) > 1, " respectively", "")
      )))



      model$covariates <- model$covariates %>%
        filter(!(column %in% dup_covs_names)) %>%
        bind_rows(covs_summary)
    }


    cat_cols <- model$covariates %>% filter(type == "categorical")

    for (cc in cat_cols$column) {
      cat_values <- pmxploitab[[cc]]
      pmxploitab[[cc]] <- factor(cat_values, levels = sort(unique(cat_values)), ordered = TRUE)

      cov_levels <- unique(cat_values) %>% as.character() %>% sort()
      names(cov_levels) <- cov_levels
      levels_list <- list(cov_levels)
      names(levels_list) <- cc

      cat_covs_levels <- c(cat_covs_levels, levels_list)
    }

    if (!model$has_metadata) {
      model$categorical_covariates_levels <- cat_covs_levels
    } else {
      model$internal$categorical_covariates_levels <- cat_covs_levels
    }

    run_tables$pmxploitab <- pmxploitab

    run_data <- list(
      info = info,
      control_stream = cs_data,
      model = model,
      estimations = estimations,
      files = files,
      tables = run_tables,
      xml = xml_report
    )

    class(run_data) <- "nonmem_run"

    run_data
  }

#' Load a NONMEM run data
#'
#' Loads NONMEM run results data from either a folder or a *.tar.gz archive file.
#'
#' @param path character. Run folder or archive file path.
#' @param temp_directory (optional) character. When \code{path} is a *.tar.gz archive file,
#' sets the path of the temporary directory where the archive files will be extracted.
#' @param load_tables logical. If \code{TRUE} (default), loads output tables.
#' @param read_initial_values logical. If \code{TRUE} (default), parses initial
#'  parameter values from the control stream.
#' @param keep_tempfiles logical. If \code{TRUE}, \code{temp_directory} will not be deleted
#' once run data is loaded.
#' @param extract_everything logical. If \code{TRUE}, when \code{path} is a *.tar.gz archive file,
#' extracts all the content of the archive. Otherwise, extracts only the files required for
#' post-processing analysis (default).
#' @param dataset_separator (optional) character. Character used as column
#'   separator in the dataset. Default is \code{NA} for automatic detection.
#' @param dataset_na_string character. Character string corresponding to missing
#'   values in the dataset. Default is \code{"."}.
#' @param update_progress (otional) function of the form \code{function(detail, ...)\{\}}.
#'   Useful to follow loading progression.
#' @param verbose logical. If \code{TRUE}: prints output messages.
#'
#' @inheritParams load_nm_run_directory
#'
#' @return A NONMEM run object. See \code{\link{load_nm_run_directory}}.
#' @export
#'
#' @examples
#' \dontrun{
#' run <- load_nm_run("path/to/my/run_folder")
#'
#' run <- load_nm_run("path/to/my/run_archive.tar.gz")
#' }
load_nm_run <-
  function(path,
             temp_directory = str_c(tempdir(), "pmxploit"),
             load_tables = TRUE,
             read_initial_values = TRUE,
             keep_tempfiles = FALSE,
             extract_everything = FALSE,
             dataset_separator = NA,
             dataset_na_string = ".",
             update_progress = NULL,
             verbose = FALSE) {
    if (!file.exists(path)) {
      stop(simpleError("Run not found."))
    }

    if (file.info(path)$isdir) {
      run_directory <- path

      run <- load_nm_run_directory(
        path = run_directory,
        dataset_separator = dataset_separator,
        dataset_na_string = dataset_na_string,
        read_initial_values = read_initial_values,
        load_tables = load_tables,
        update_progress = update_progress,
        verbose = verbose
      )
    } else {
      run_archive <- path

      run_archive <- normalizePath(run_archive)

      if (!(str_detect(tolower(run_archive), "\\.tar\\.gz$"))) {
        stop(simpleError("Run archive file has to be a *.tar.gz file."))
      }

      is_windows <- (tolower(Sys.info()["sysname"]) == "windows")

      archive_dir <- dirname(run_archive)

      if (is.function(update_progress)) {
        update_progress(value = 0.10, detail = "Extracting files...")
      }

      quiet_untar <- function(...) {
        q_untar <- quietly(untar)

        q_untar(...)$result
      }

      # skip TAR messages on Windows (many warnings), and if verbose = FALSE
      untar_function <- ifelse(is_windows || !verbose, quiet_untar, untar)

      # Create tempdir
      repeat {
        if (!is.null(temp_directory)) {
          if (dir.exists(temp_directory)) {
            unlink(temp_directory, recursive = TRUE)
          }
          run_directory <- temp_directory
        } else {
          run_directory <-
            paste(archive_dir, paste0("pmxploit", sample(1:10000, 1)), sep = "/")
        }

        if (!dir.exists(run_directory)) {
          archive_files <- untar_function(tarfile = run_archive, list = TRUE, verbose = verbose)

          files_extensions <- tools::file_ext(archive_files) %>% unique()

          cs_exts <- c("ctl", "con", "mod", "nmctl")

          if (!(any(cs_exts %in% files_extensions) && "xml" %in% files_extensions)) {
            stop(simpleError("NONMEM run not recognized: no XML result file found."))
          }

          # dur <- system.time({

          cs_file <- tibble(filename = archive_files) %>%
            mutate(
              ext = ordered(tools::file_ext(filename), cs_exts),
              file_sans_ext = tools::file_path_sans_ext(basename(filename))
            ) %>%
            filter(ext %in% cs_exts) %>%
            mutate(dir = dirname(filename)) %>%
            filter(dir == ".") %>%
            group_by(file_sans_ext) %>%
            mutate(group_len = dplyr::n()) %>%
            filter(group_len == max(group_len)) %>%
            ungroup() %>%
            arrange(ext) %>%
            slice(1)

          untar_function(
            tarfile = run_archive, files = cs_file$filename,
            exdir = run_directory, verbose = verbose
          )

          # cs_file_path <- cs_file %>%
          #   # safe check if it is a symbolik link
          #   filter(file.exists(str_c(run_directory, filename, sep = "/"))) %>%
          #   arrange(ext) %>% #
          #   slice(1) %>%
          #   pull(filename)

          cs_file_path <- cs_file$filename

          # parse control stream data
          cs_path <- str_c(run_directory, basename(cs_file_path), sep = "/")
          cs_content <- read_file(file = cs_path)

          safe_parse_cs <- safely(parse_nm_control_stream)

          safe_cs <- safe_parse_cs(content = cs_content, read_initial_values = read_initial_values, verbose = verbose)

          if (!is.null(safe_cs$error)) {
            stop(simpleError(sprintf("Error reading control stream file, \"%s\".", cs_path)))
          }

          cs_data <- safe_cs$result

          # look for ID column
          if (!("ID" %in% c(cs_data$input$column, cs_data$input$synonym))) {
            id_col_index <- ifelse(cs_data$ignore$C & cs_data$input$column[1] == "C", 2L, 1L)

            cs_data$input[id_col_index, ]$column <- "ID"
          }

          if (extract_everything) {
            untar_function(tarfile = run_archive, exdir = run_directory, verbose = verbose)
          } else {
            run_name <- tools::file_path_sans_ext(basename(cs_file_path))

            archive_files_df <- tibble(file = archive_files) %>%
              mutate(
                file2 = ifelse(substring(file, 1, 2) == "./", substring(file, 3), file), # trick when tar.gz archives that contain subdirectories
                dir_name = dirname(file),
                base_name = basename(file),
                extension = tools::file_ext(file)
              ) %>%
              filter(dir_name == "." & base_name != ".")

            files_pattern <-
              sprintf(
                "^%s$", c(
                  cs_data$dataset_file %>% str_replace_all("\\.", "\\\\."),
                  sprintf("%s\\.rep", run_name),
                  sprintf("%s\\.lst", run_name),
                  sprintf("%s\\.ext", run_name),
                  sprintf("%s\\.phi", run_name),
                  sprintf("%s\\.xml", run_name)
                )
              )

            if (load_tables && !is.null(cs_data$tables) && nrow(cs_data$tables) > 0) {
              tabs_pattern <- paste0("^", cs_data$tables$file %>% str_replace_all("\\.", "\\\\."))

              pattern <- sprintf("(%s)", paste(c(files_pattern, tabs_pattern), collapse = "|"))
            } else {
              pattern <- sprintf("(%s)", paste(files_pattern, collapse = "|"))
            }

            archive_files_df <- archive_files_df %>%
              mutate(match = str_detect(file2, pattern)) %>%
              filter(match == TRUE)

            files_list <- archive_files_df$file

            # extra_files <- c(cs_data$extra_files, cs_data$subroutine$FILES)

            if (length(cs_data$extra_files) > 0) {
              files_list <- c(files_list, str_c("./", cs_data$extra_files))
            }

            if (length(mdata_file <- str_subset(archive_files, fixed("pmxploit_metadata.rds"))) == 1) {
              files_list <- c(mdata_file, files_list)
            }

            untar_function(
              tarfile = run_archive, files = files_list,
              exdir = run_directory, verbose = verbose
            )
          }
          break
        }
      }

      run <- load_nm_run_directory(
        path = run_directory,
        load_tables = load_tables,
        update_progress = update_progress,
        dataset_separator = dataset_separator,
        dataset_na_string = dataset_na_string,
        control_stream = cs_data,
        verbose = verbose
      )

      # run$info$directory <- normalizePath(dirname(run_archive))
      run$info$path <- normalizePath(run_archive)

      # Cleanup ?
      if (keep_tempfiles) {
        # run$info$temp_directory <- run_directory
        print(sprintf(
          "Temp files have been extracted to '%s' directory.", run_directory
        ))
      } else {
        unlink(run_directory, recursive = TRUE)
      }
    }

    invisible(run)
  }
