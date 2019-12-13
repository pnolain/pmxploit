#' Generates control files and dataset for bootstrap analysis
#'
#' @param run \code{pmxploit} NONMEM run object of the source run.
#' @param target_directory path of the directory where the generated files will be located.
#' @param n_sample integer. Number of runs to generate.
#' @param seed integer. Seed number for reproducibility.
#' @param update_progress a function useful to monitor the generation process. This function should take a \code{detail} argument.
#'
#' @return A vector containing the paths of the generated control files
#' @export
#'
#' @examples
#' \dontrun{
#' nm_bootstrap(pmxploit::EXAMPLERUN)
#' }
nm_bootstrap <- function(run,
                         target_directory = "bootstrap",
                         n_sample = 200,
                         seed = 1000,
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

  source_ds <- run$tables$dataset %>%
    mutate_if(is.factor, ~ as.integer(as.character(.)))

  if("ID_ORIG" %in% colnames(source_ds)){
    source_ds <- source_ds %>% select(-ID_ORIG)
    warning(simpleWarning("`ID_ORIG` column was dropped from the source dataset."))
  }

  ids <- source_ds$ID %>% unique()
  n_ids <- length(ids)

  set.seed(seed)

  # generate bootstrap data
  boot_data <- map_dfr(seq_len(n_sample), function(x){
    boot_sample_ids <- sample(ids, n_ids, replace = TRUE)

    boot_sample_ds <- tibble(ID = seq_len(n_ids), ID_ORIG = boot_sample_ids) %>%
      left_join(source_ds, by = c("ID_ORIG" = "ID")) %>%
      select(-ID_ORIG, everything(), ID_ORIG)

    boot_sample_ds_name <- str_c("bootstrap.", x, ".csv")
    boot_sample_cs_name <- str_c(run$info$run_name, ".bootstrap.", x, ".ctl")

    boot_sample_ds_path <- str_c(target_directory, "input", boot_sample_ds_name, sep = "/")

    new_ds_text <- str_c(str_c(boot_sample_ds_name, sep = "/"), "IGNORE=@", sep = " ")

    boot_cs_code <- run$control_stream$code %>%
      # remove previous $IGNORE=C/@/#
      str_replace("IGNORE\\s*=\\s*(@|#|C)\\b", "") %>%
      # set dataset file name
      str_replace(fixed(run$info$dataset_file), new_ds_text)

    input_record <- run$control_stream$records %>% filter(name == "INPUT")

    boot_col_names <- colnames(boot_sample_ds)

    # Keep original aliases so it works stays compatible with original control stream code
    for(i in seq_along(boot_col_names)){
      input_col <- run$control_stream$input %>% filter(map2_lgl(column, synonym, ~ boot_col_names[i] %in% c(..1, ..2)))
      if(nrow(input_col) == 1)
        boot_col_names[i] <- input_col$match
    }

    boot_cs_code[input_record$start:input_record$end] <- ""
    boot_cs_code[input_record$start] <- str_c("$INPUT", str_c(boot_col_names, collapse = " "), sep = " ")

    boot_sample_cs_path <- str_c(target_directory, "input", boot_sample_cs_name, sep = "/")

    tibble(cs_path = boot_sample_cs_path,
           ds_path = boot_sample_ds_path,
           cs_code = list(boot_cs_code),
           dataset = list(boot_sample_ds))
  })

  # save bootstrap data
  save_boot_data <- function(cs_path, ds_path, cs_code, dataset){
    write_csv(dataset, ds_path, na = ".")
    write_lines(cs_code, cs_path)

    if (is.function(update_progress))
      update_progress(detail = str_c("Saving ", basename(cs_path)))
  }

  pwalk(boot_data, save_boot_data)

  extra_files <- c(run$control_stream$subroutine$FILES, run$control_stream$extra_files)

  if(length(extra_files) > 0){
    if(file.info(run$info$path)$isdir){
      file.copy(str_c(run$info$path, extra_files, sep = "/"),
                dirname(boot_data$cs_path[[1]]))
    } else {
      archive_files <- untar(run$info$path, list = TRUE)
      archive_files <- str_subset(archive_files, fixed(extra_files))
      untar(run$info$path, files = archive_files,
            exdir = dirname(chain_data$cs_path[[1]]))
    }
  }

  boot_data$cs_path
}
