#' Performs a NM-TRAN check of the control file
#'
#' @param control_file Path of the control file
#' @param nmcheck_cmd Path of NMTRAN check executable
#'
#' @return A list with control file path and error/error message if any.
#' @export
#'
#' @examples
#' \dontrun{
#' nm_check(control_file = "control_file.ctl",
#' nmcheck_exe = "/path/to/nmtran.exe",
#' call_template = "cat {control_file} | {nmcheck_exe}")
#' }
nm_check <- function(control_file,
                     nmcheck_exe = NULL,
                     call_template = NULL) {

  stopifnot(!is.null(nmcheck_exe))
  stopifnot(file.exists(control_file))

  if(!file.exists(nmcheck_exe))
    stop(simpleError("`nmcheck_exe` not found."))

  if(is.null(call_template) || call_template == "")
    call_template <- "cat {control_file} | {nmcheck_exe}"

  owd <- setwd(dirname(control_file))
  on.exit(setwd(owd))

  nmcheck_cmd <- glue::glue(call_template)

  safe_call <- safely(~ system(nmcheck_cmd, intern = TRUE))

  nmtran_check <- safe_call()

  nmtran_files <- c("FCON", "FDATA", "FREPORT", "FSIZES", "FSTREAM", "FSUBS", "FSUBS2", "FSUBS_MU.F90", "FWARN", "PRSIZES.f90", "trash.tmp")

  nmtran_files %>% walk(~if(file.exists(.)) file.remove(.))

  is_error <- FALSE
  msg <- NULL

  if(!is.null(nmtran_check$error)){
    is_error <- TRUE

    msg <- nmtran_check$error

  } else {
    msg <- nmtran_check$result %>%
      stringr::str_conv("") %>%
      str_trim() %>%
      str_c(collapse = "\n")

    is_error <- str_detect(msg, "AN ERROR WAS FOUND IN THE CONTROL STATEMENTS.")
  }

  list(path = control_file,
       error = is_error,
       message = msg)
}
