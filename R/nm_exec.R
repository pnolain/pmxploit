#' Starts a NONMEM run
#'
#' @param control_file path of the control file
#' @param run_directory path of the directory where to perform the run
#' @param nonmem_exe path of NONMEM executable path
#' @param n_nodes number of CPUs
#' @param parafile path of a NONMEM parallel processing settings file
#' @param result_directory path of the directory where to place the results
#' @param job_id (optional) integer run ID
#' @param archive logical. If TRUE, compress the result files to a *.tar.gz archive?
#' @param cleanup logical. If TRUE, remove NONMEM temporary files
#' @param quiet logical. If TRUE, do not print NONMEM output when executing
#' @param call_template character string used to construct NONMEM call. Helps creating custom script calls.
#'
#' @return A \code{pmxploit} NONMEM run object.
#' @export
#'
#' @examples
#' \notrun{
#' nm_exec(control_file = "control_file.ctl", nonmem_exe = "/path/to/nmfe")
#' }
nm_exec <- function(control_file,
                   run_directory = NULL,
                   nonmem_exe = NULL,
                   n_nodes = 1L,
                   parafile = NULL,
                   result_directory = NULL,
                   job_id = NULL,
                   archive = TRUE,
                   cleanup = TRUE,
                   quiet = FALSE,
                   call_template = NULL,
                   ...) {

  if(!file.exists(control_file))
    stop(simpleError("Control file not found."))

  if(is.null(nonmem_exe))
    stop(simpleError("You must specify a command to execute NONMEM (NONMEM executable path or a custom script a taken control file as input)."))

  if(!file.exists(nonmem_exe))
    stop(simpleError("`nonmem_exe` not found."))

  if(is.null(call_template) || call_template == "")
    call_template <- "{nonmem_exe} {control_file} {output_file} [nodes]={n_nodes} -parafile={parafile}"

  # if(n_nodes > 1){
  #   if(is.null(parafile)) stop(simpleError("You must specify a parallel computing configuration file."))
  #   if(!file.exists(parafile)) stop(simpleError("Parallelization file `parafile` does not exists."))
  # }

  output_file <- str_c(tools::file_path_sans_ext(basename(control_file)), ".lst")
  init_workdir <- dirname(control_file)

  short_name <- tools::file_path_sans_ext(basename(control_file))
  control_file_ext <- tools::file_ext(basename(control_file))

  #running_prefix <- "temp_run."
  running_prefix <- ""

  if(is.null(run_directory))
    run_directory <- str_c(dirname(control_file), str_c(running_prefix, short_name, ifelse(control_file_ext == "", "_dir", "")), sep = "/")

  run_directory_copy <- run_directory

  if(dir.exists(run_directory) || file.exists(run_directory)) {
    i <- 2

    source_dir <- ifelse(is.null(run_directory), dirname(control_file), dirname(run_directory))

    repeat{
      run_directory <- str_c(source_dir, str_c(short_name, ".", i),  sep = "/")

      if(!dir.exists(run_directory))
        break

      i <- i + 1
    }
  }

  # create running dir
  dir.create(run_directory, recursive = TRUE)

  # unsure to get absolute path
  run_directory <- normalizePath(run_directory)

  # create results dir
  if(is.null(result_directory)){
    result_directory <- ifelse(archive, dirname(run_directory), run_directory)
  } else {
    if(!dir.exists(result_directory))
      dir.create(result_directory, recursive = TRUE)
  }
  result_directory <- normalizePath(result_directory)

  # Look for dataset file
  safe_parse <- safely(pmxploit::parse_nm_control_stream)
  cs_data <- safe_parse(control_file)

  owd <- getwd()

  if(!is.null(cs_data$error)){
    # Change working directory
    setwd(run_directory)
    on.exit(setwd(owd))

    write_lines(str_c("Control stream parsing error:", cs_data$error$message, sep = "\n"), path = "ERROR.txt")
  } else {
    cs_data <- cs_data$result
    ds_filename <- cs_data$dataset_file

    ds_path <- normalizePath(str_c(init_workdir, ds_filename, sep = "/"))

    if(!file.exists(ds_path)){
      # Change working directory
      setwd(run_directory)
      on.exit(setwd(owd))

      write_lines(str_c("Dataset file not found at ", ds_path), path = "ERROR.txt")
    } else {
      # Copy files needed for NONMEM execution to TEMP folder
      extra_files <- c(cs_data$extra_files)

      if(length(extra_files) > 0)
        extra_files <- normalizePath(str_c(init_workdir, extra_files, sep = "/"))

      files_to_copy <- c(control_file, ds_path, extra_files)

      if(n_nodes > 1){
        files_to_copy <- c(files_to_copy, parafile)
      } else {
        parafile <- NULL # No parafile needed if no parallel processing
      }

      file.copy(files_to_copy, run_directory)

      if(ds_filename != basename(ds_filename)){
        # if dataset file is in another folder -> rename $DATA in control stream
        running_cs_path <- str_c(run_directory, basename(control_file), sep = "/")

        cs_content <- read_lines(running_cs_path)

        # Edit control stream content
        cs_content %>%
          str_replace(fixed(ds_filename), basename(ds_filename)) %>%
          write_lines(running_cs_path)
      }

      # Change working directory
      owd <- setwd(run_directory)
      on.exit(setwd(owd))

      nm_args <- list(basename(control_file),
                      output_file)

      # V1
      # if(n_nodes > 1){
      #   nm_args <- c(nm_args,
      #                str_c("-parafile=", basename(parafile)),
      #                str_c("[nodes]=", n_nodes))
      # }
      # system2(nonmem_exe, args = nm_args, stdout = ifelse(quiet, FALSE, ""))

      # V2
      # nm_args_str <- str_c(nm_args, collapse = " ")
      # nm_cmd <- str_c(nonmem_exe, nm_args_str, sep = " ")
      # system(nm_cmd, ignore.stdout = quiet)

      # V3
      # Set "basename(file)" so glue can generate the nonmem command string
      control_file <- basename(control_file)
      parafile <- ifelse(is.null(parafile), "''", basename(parafile))

      # Look for extra arguments to build the nonmem command string
      extra_args <- as.list(substitute(list(...)))[-1L]

      if(length(extra_args) > 0){
        current_env <- environment()

        for(i in seq_along(extra_args)){
          item <- extra_args[[i]]
          n_item <- names(extra_args)[i]

          if(n_item %in% names(current_env))
            stop(simpleError(sprintf("`%s already` exists in the current context, please use another name.", n_item)))

          current_env[[n_item]] <- item
        }
      }

      nm_cmd <- glue::glue(call_template)

      system(nm_cmd, ignore.stdout = quiet)

      safe_load <- safely(pmxploit::load_nm_run)

      run <- safe_load(getwd())

      if(!is.null(run$error)){
        stop(simpleError(sprintf("An error occured while loading run results. Run files are located at '%s'.", getwd())))
      }

      if(cleanup){
        # browser()
        cleanup_nm_folder(cs_data, short_name, ds_filename, extra_files)
      }
    }
  }

  # if compress the job: zip run
  if(archive){
    run_name <- tools::file_path_sans_ext(basename(control_file))
    job_suffix <- ifelse(!is.null(job_id), str_c(".", job_id), "")

    archive_file <- str_c(run_name, job_suffix, ".tar.gz")

    tar(archive_file, compression = "gzip")
  }

  # if it was deleted during run, recreate it
  if(!dir.exists(result_directory))
    dir.create(result_directory, recursive = TRUE)

  # move archive/run
  if(archive){
    target_archive_path <- str_c(result_directory, archive_file, sep = "/")

    if(file.exists(target_archive_path)){
      i <- 2
      repeat {
        if(file.exists(target_archive_path)){
          target_archive_path <- str_c(result_directory,
                                       str_c(tools::file_path_sans_ext(archive_file, compression = TRUE),
                                             str_c("-", i), ".tar.gz"), sep = "/")
          i <- i + 1
        } else {
          break
        }
      }
    }

    file.copy(archive_file, target_archive_path)
  } else if(run_directory != result_directory){
    #fs::dir_copy(run_directory, result_directory)
    complete_result_directory <- str_c(result_directory, basename(run_directory_copy), sep = "/")

    if(dir.exists(complete_result_directory)){
      i <- 2

      repeat{
        complete_result_directory <- str_c(result_directory, str_c(basename(run_directory_copy), ".", i),  sep = "/")

        if(!dir.exists(complete_result_directory))
          break

        i <- i + 1
      }
    }

    #file.copy(run_directory, result_directory, recursive = TRUE)
    dir.create(complete_result_directory, recursive = TRUE)
    file.copy(list.files(run_directory), complete_result_directory)
    unlink(run_directory, recursive = TRUE)

    # file.rename(run_directory, complete_result_directory)
    # fs::file_move(run_directory, complete_result_directory)

    run$info$path <- complete_result_directory
  }

  # delete temp running directory
  if(archive || (cleanup && run_directory != result_directory))
    unlink(run_directory, recursive = TRUE)

  run <- run$result

  run$info$path <- ifelse(archive, target_archive_path, result_directory)

  return(run)
}
