#' Generates control files and dataset for $CHAIN analysis
#'
#' @param iaccept IACCEPT parameter
#' @param ctype CTYPE parameter
#' @param degrees_of_freedom DF parameter
#' @inheritParams nm_bootstrap
#'
#' @details See NONMEM documentation for details about IACCEPT, CTYPE and DF parameters.
#'
#' @return A vector containing the paths of the generated control files
#' @export
#'
#' @examples
#' \dontrun{
#' nm_chain(pmxploit::EXAMPLERUN)
#' }
nm_chain <- function(run,
                     n_sample = 15,
                     iaccept = 0.5,
                     ctype = 2,
                     degrees_of_freedom = NULL,
                     seed = 1000,
                     target_directory = "initial",
                     update_progress = NULL){
  if(class(run) != "nonmem_run")
    stop(simpleError("`run` must be a NONMEM run loaded with `pmxploit` package."))

  if(is.null(target_directory) || dir.exists(target_directory)) {
    i <- 2

    source_dir <- ifelse(is.null(target_directory), dirname(run$info$path), dirname(target_directory))

    short_name <- basename(target_directory)
    repeat{
      target_directory <- str_c(source_dir, str_c(short_name, ".", i),  sep = "/")

      if(!dir.exists(target_directory))
        break

      i <- i + 1
    }
  }

  dir.create(target_directory, recursive = TRUE)
  dir.create(str_c(target_directory, "input", sep = "/"), recursive = TRUE)

  if(is.null(degrees_of_freedom)){
    degrees_of_freedom <- nrow(run$control_stream$parameters$initialization$OMEGA)
  }

  # generate chain data
  chain_records <- tibble(n = seq_len(n_sample)) %>%
    mutate(record = sprintf("$ESTIMATION METHOD=CHAIN ISAMPLE=%s NSAMPLE=%s DF=%s IACCEPT=%s CTYPE=%s SEED=%s",
                                  n, n_sample, degrees_of_freedom, iaccept, ctype, seed))

  generate_chain_data <- function(n, record){
    chain_sample_cs_name <- str_c(run$info$run_name, ".chain.", n, ".ctl")

    est_lines <- str_which(run$control_stream$code, "^\\s*\\$EST")

    # if(length(est_lines) == 0) stop(simpleError("No $ESTIMATION record found in control stream."))
    insert_chain_before <- first(est_lines)

    if(is.na(insert_chain_before)){ # No $ESTIMATION, add $CHAIN after parameters initial values
      insert_chain_before <- run$control_stream$records %>%
        filter(name %in% c("THETA", "OMEGA", "SIGMA")) %>%
        pull(end) %>%
        max() + 1
    }

    chain_cs_code <- run$control_stream$code %>%
      str_replace(fixed(run$info$dataset_file), str_c(basename(run$info$dataset_file), sep = "/"))

    input_record <- run$control_stream$records %>% filter(name == "INPUT")

    chain_cs_code[input_record$start:input_record$end] <- ""
    chain_cs_code[input_record$start] <- str_c("$INPUT", str_c(run$control_stream$input$match, collapse = " "), sep = " ")

    chain_cs_code <- append(chain_cs_code, record, insert_chain_before - 1)

    chain_sample_cs_path <- str_c(target_directory, "input", chain_sample_cs_name, sep = "/")

    tibble(cs_path = chain_sample_cs_path,
           cs_code = list(chain_cs_code))
  }

  chain_data <- chain_records %>%
    pmap_dfr(generate_chain_data)

  # save chain data
  save_chain_data <- function(cs_path, cs_code){
    write_lines(cs_code, cs_path)

    if (is.function(update_progress))
      update_progress(detail = str_c("Saving ", basename(cs_path)))
  }

  pwalk(chain_data, save_chain_data)

  # Move original files: dataset and extra files (custom subroutines)
  extra_files <- c(run$control_stream$subroutine$FILES, run$control_stream$extra_files)

  files_to_copy <- c(run$control_stream$dataset_file, extra_files)

  if(length(files_to_copy) > 0){
    if(file.info(run$info$path)$isdir){
      file.copy(str_c(run$info$path, files_to_copy, sep = "/"),
                dirname(chain_data$cs_path[[1]]))
    } else {
      archive_files <- untar(run$info$path, list = TRUE)
      archive_files <- str_subset(archive_files, fixed(files_to_copy))
      untar(run$info$path, files = archive_files,
            exdir = dirname(chain_data$cs_path[[1]]))
    }
  }

  chain_data$cs_path
}
