#' Parse a NONMEM iteration file (*.ext)
#'
#' Reads the content of an iteration file and extracts informations related to the
#' parameters and the objective function values.
#'
#' @param filepath character. If \code{content = NULL}, defines the filepath of
#'   the iteration file.
#' @param content character. Text content of an iteration file.
#'
#' @return A tibble with one row per estimation step and 10 columns: \itemize{
#' \item \code{number}: the number of the estimation step
#' \item \code{method}: the name of the estimation method
#' \item \code{goal_function}: the goal function of the estimation method
#' \item \code{problem}: the goal function of the estimation method
#' \item \code{subproblem}: subproblem value
#' \item \code{superproblem1}: superproblem1 value
#' \item \code{iteration1}: iteration1 value
#' \item \code{superproblem2}: superproblem2 value
#' \item \code{iteration2}: iteration2 value
#' \item \code{iterations}: a tibble with the population parameters and objective function values at each printed out iterations.
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' parse_nm_ext(filepath = "job_iterations.ext")
#' }
parse_nm_ext <- function(filepath = NULL, content = NULL) {
  if (all(is.null(c(content, filepath)))) {
    stop(simpleError("One of `content` or `filepath` arguments is required."))
  }

  if (is.null(content)) {
    if (!file.exists(filepath)) {
      stop(simpleError("File does not exists."))
    }
    content <- read_file(filepath)
  }

  content_lines <- content %>%
    str_split("\n") %>%
    unlist() %>%
    str_trim()

  tab_line_pattern <- "TABLE NO\\.\\s*([0-9]+): (.+): Goal Function=(.+): Problem=([0-9]+) Subproblem=([0-9]+) Superproblem1=([0-9]+) Iteration1=([0-9]+) Superproblem2=([0-9]+) Iteration2=([0-9]+)"

  tab_title_lines <- content_lines %>% str_which(tab_line_pattern)

  if (length(tab_title_lines) == 0) {
    return(NULL)
  }

  tab_titles_df <- tibble(
    index = tab_title_lines,
    line = content_lines[tab_title_lines]
  ) %>%
    extract(line, into = c("number", "method", "goal_function", "problem", "subproblem", "superproblem1", "iteration1", "superproblem2", "iteration2"), tab_line_pattern, convert = TRUE) %>%
    filter(!str_detect(method, "Chain Method Processing")) %>%
    mutate(
      start_index = index,
      end_index = lead(start_index, default = length(content_lines) + 1) - 1
    ) %>%
    filter(start_index != end_index) %>% # filter out if no iteration reported yet
    mutate(iterations = map2(start_index, end_index, function(start, end) {
      header <- content_lines[(start + 1)]

      if (header == "") return(tibble())

      if (!str_detect(header, "^ITERATION")) return(tibble()) # buggy tables

      cn <- header %>%
        str_extract_all("[^\\s]+") %>%
        unlist() %>%
        str_remove_all(",")

      est_lines <- content_lines[(start + 2):end]

      fixed_lines <- est_lines %>%
        str_replace_all(",", " ") %>%
        str_replace_all("\\s+", " ")

      df <- read_delim(str_c(fixed_lines, "\n", collapse = "\n"),
        delim = " ", col_names = cn
      )

      df
    })) %>%
    select(-start_index, -end_index, -index)
}
