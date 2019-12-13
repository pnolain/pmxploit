#' Starts a NONMEM run and do not wait for the results
#'
#' Starts a NONMEM run and do not wait for the results. Useful when NONMEM jobs are run via an HPC scheduler or a cloud solution.
#'
#' @param control_file path of the control file
#' @param call_template character string call template used to start the run.
#'
#' @return A character string returned by the system call.
#' @export
#'
#' @examples
#' \dontrun{
#' nm_exec2(control_file = "control_file.ctl", custom_script = "/path/to/script", call_template = "{custom_script} | {control_file}")
#' }
nm_exec2 <- function(control_file,
                     call_template = NULL,
                     ...) {

  if(!file.exists(control_file))
    stop(simpleError("Control file not found."))

  if(is.null(call_template) || call_template == "")
    stop(simpleError("You must specify a call template to start the remote NONMEM run."))

  cmd <- glue::glue(call_template)

  system(cmd, intern = TRUE)
}
