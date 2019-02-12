#' Generates control files and dataset for jacknife analysis
#'
#' @inheritParams nm_bootstrap
#'
#' @return A vector containing the paths of the generated control files
#' @export
#'
#' @examples#' \notrun{
#' nm_jacknife(pmxploit::EXAMPLERUN)
#' }
nm_jacknife <- function(run,
                        target_directory = "jacknife",
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

  # generate jacknife data
  jack_data <- map_dfr(seq_len(n_ids), function(x){
    jack_sample_ids <- ids[-x]

    jack_sample_ds <- tibble(ID = seq_len(n_ids - 1), ID_ORIG = jack_sample_ids) %>%
      left_join(source_ds, by = c("ID_ORIG" = "ID")) %>%
      select(-ID_ORIG, everything(), ID_ORIG)

    jack_sample_ds_name <- str_c("jacknife.", x, ".csv")
    jack_sample_cs_name <- str_c(run$info$run_name, ".jacknife.", x, ".ctl")

    jack_sample_ds_path <- str_c(target_directory, "input", jack_sample_ds_name, sep = "/")

    new_ds_text <- str_c(str_c(jack_sample_ds_name, sep = "/"), "IGNORE=@", sep = " ")

    jack_cs_code <- run$control_stream$code %>%
      # remove previous $IGNORE=C/@/#
      str_replace("IGNORE\\s*=\\s*(@|#|C\\b)", "") %>%
      # set dataset file name
      str_replace(fixed(run$info$dataset_file), new_ds_text)

    input_record <- run$control_stream$records %>% filter(name == "INPUT")

    jack_col_names <- colnames(jack_sample_ds)

    # Keep original aliases so it works stays compatible with original control stream code
    for(i in seq_along(jack_col_names)){
      input_col <- run$control_stream$input %>% filter(map2_lgl(column, synonym, ~ jack_col_names[i] %in% c(..1, ..2)))
      if(nrow(input_col) == 1)
        jack_col_names[i] <- input_col$match
    }

    jack_cs_code[input_record$start:input_record$end] <- ""
    jack_cs_code[input_record$start] <- str_c("$INPUT", str_c(jack_col_names, collapse = " "), sep = " ")

    jack_sample_cs_path <- str_c(target_directory, "input", jack_sample_cs_name, sep = "/")

    tibble(cs_path = jack_sample_cs_path,
           ds_path = jack_sample_ds_path,
           cs_code = list(jack_cs_code),
           dataset = list(jack_sample_ds))
  })

  # save jacknife data
  save_jack_data <- function(cs_path, ds_path, cs_code, dataset, extra_files){
    write_csv(dataset, ds_path, na = ".")
    write_lines(cs_code, cs_path)

    if (is.function(update_progress))
      update_progress(detail = str_c("Saving ", basename(cs_path)))
  }

  pwalk(jack_data, save_jack_data)

  extra_files <- c(run$control_stream$subroutine$FILES, run$control_stream$extra_files)

  if(length(extra_files) > 0){
    if(file.info(run$info$path)$isdir){
      file.copy(str_c(run$info$path, extra_files, sep = "/"),
                dirname(jack_data$cs_path[[1]]))
    } else {
      archive_files <- untar(run$info$path, list = TRUE)
      archive_files <- str_subset(archive_files, fixed(extra_files))
      untar(run$info$path, files = archive_files,
            exdir = dirname(chain_data$cs_path[[1]]))
    }
  }

  jack_data$cs_path
}
