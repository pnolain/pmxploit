cleanup_nm_folder <- function(cs_data, short_name, ds_filename, extra_files){
  # cleanup running directory
  unlink("temp_dir", recursive = TRUE)

  all_files <- tibble(file = list.files())

  files_info <- all_files %>%
    mutate(sans_ext = tools::file_path_sans_ext(file, compression = TRUE), extension = tools::file_ext(file))

  # prevent bug: control file name ends with '_' (before extension), '_' character is removed in *.ext file
  if(str_detect(short_name, "_$")){
    good_ext <- files_info %>%  filter(extension == "ext") %>%  filter(file.info(file)$size > 0 & short_name != sans_ext)
    bad_ext <- files_info %>%  filter(extension == "ext") %>%  filter(file.info(file)$size == 0 & short_name == sans_ext)

    if(nrow(bad_ext) == 1) map(bad_ext$file, file.remove)
    if(nrow(good_ext) == 1) file.rename(good_ext$file, str_c(short_name, ".ext"))
  }

  bs_extra_files <- (if(length(extra_files) > 0) basename(extra_files) else NULL)

  run_files <- (if(!is.null(cs_data$tables) && nrow(cs_data$tables) > 0) c(bs_extra_files, cs_data$tables$file) else bs_extra_files)

  files_to_keep <- files_info %>%
    mutate(same_name = sans_ext == short_name,
           is_dataset = file == basename(ds_filename),
           is_not_empty = file.info(file)$size > 0,
           is_in_files = map_lgl(file, ~ ifelse(!is.null(run_files), any(str_detect(., fixed(run_files))), FALSE))) %>%
    mutate(keep = (same_name | is_dataset | is_in_files) & is_not_empty)

  files_to_keep %>%
    filter(!keep) %>%
    pull(file) %>%
    keep(file.exists) %>%
    walk(unlink, recursive = TRUE)
}
