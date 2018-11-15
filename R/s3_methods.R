#' Print a NONMEM run object
#'
#' @param x A \code{nonmem_run} object generated.
#'
#' @method print nonmem_run
#' @examples
#' # Using the print function
#' print(EXAMPLERUN)
#'
#' # Or
#' EXAMPLERUN
#'
#' @export
print.nonmem_run <- function(x, ...) {
  txt <- c(
    str_c("Run path: ", x$info$path),
    str_c("NONMEM version ", x$info$nm_version),
    str_c("Dataset: "),
    str_c("\tFile: ", x$info$dataset_file),
    str_c("\tIndividuals: ", x$info$number_of_subjects),
    str_c("\tObservations: ", x$info$number_of_observations),
    str_c("Duration: ", x$info$duration),
    str_c("Estimation(s): \n", str_c(sprintf("\t%s", names(x$estimations)), collapse = "\n")),
    str_c("Table(s): ", ifelse(nrow(x$control_stream$tables) > 0,
      str_c(sort(x$control_stream$tables$file), collapse = ", "),
      "No output table"
    ))
  )

  active_filters <- attr(x$tables$pmxploitab, "filters")

  if (!is.null(active_filters)) {
    filtered <- x %>% filter(UQS(active_filters))

    txt <- c(
      txt,
      str_c(
        "PMXploit data is filtered:\n",
        "\tFilters:\n",
        str_c(sprintf("\t%s", as.character(active_filters)), collapse = "\n"),
        "\n\tSummary:\n",
        str_c("\t\tIndividuals: ", filtered$tables$pmxploitab$ID %>% unique() %>% length()),
        str_c("\n\t\tObservations: ", filtered$tables$pmxploitab %>% filter(!is.na(DV) & MDV != 1) %>% nrow())
      )
    )
  }

  writeLines(txt)
}

#' Filter a NONMEM run object data
#'
#' @param x An \code{nonmem_run} object.
#'
#' @method filter nonmem_run
#'
#' @details Adds an attribute to the \code{nonmem_run} object so that
#' every function call on the run (plot, summary, quality criteria) will
#' first filter the internal data.
#'
#' @examples
#' filter(EXAMPLERUN, STUD == 0)
#'
#' filter(EXAMPLERUN, SEX == 0, STUD == 1)
#'
#' @export
filter.nonmem_run <- function(x, ...) {
  filters <- quos(...)

  if (length(filters) > 0) {
    temp_tab <- x$tables$pmxploitab %>%
      filter(UQS(filters))

    x$tables$pmxploitab <- temp_tab

    attr(x$tables$pmxploitab, "filters") <- append(attr(x$tables$pmxploitab, "filters"), filters)
  }

  x
}

#' Group a NONMEM run object data
#'
#' @param x An \code{nonmem_run} object generated.
#'
#' @method group_by nonmem_run
#' @examples
#' group_by(EXAMPLERUN, SEX)
#'
#' group_by(EXAMPLERUN, SEX, STUD)
#'
#' @export
group_by.nonmem_run <- function(x, ...) {
  grps <- quos(...)

  temp_tab <- x$tables$pmxploitab %>%
    group_by(UQS(grps))

  x$tables$pmxploitab <- temp_tab

  x
}

#' Ungroup a NONMEM run object data
#'
#' @param x An \code{nonmem_run} object generated.
#'
#' @method ungroup nonmem_run
#' @examples
#' ungroup(EXAMPLERUN)
#'
#' @export
ungroup.nonmem_run <- function(x) {
  temp_tab <- x$tables$pmxploitab %>%
    ungroup()

  x$tables$pmxploitab <- temp_tab

  # attr(x$tables$pmxploitab, "filters") <- filters

  x
}


#' Slice a NONMEM run object data
#'
#' @param x An \code{nonmem_run} object generated.
#'
#' @method slice nonmem_run
#' @examples
#' slice(EXAMPLERUN, 1:10)
#'
#' @export
slice.nonmem_run <- function(x, ...) {
  temp_tab <- x$tables$pmxploitab

  temp_tab <- x$tables$pmxploitab %>%
    slice(...)

  x$tables$pmxploitab <- temp_tab

  x
}



#' Print a NONMEM control_stream object
#'
#' @param x A \code{nonmem_control_stream} object generated.
#'
#' @method print nonmem_control_stream
#' @examples
#' # Using the print function
#' print(EXAMPLERUN$control_stream)
#'
#' # Or
#' EXAMPLERUN$control_stream
#'
#' @export
print.nonmem_control_stream <- function(x, ...) {
  writeLines(x$code)
}


#' Print the summary of a NONMEM control stream object
#'
#' @param x A \code{nonmem_control_stream} object generated.
#'
#' @method summary nonmem_control_stream
#' @examples
#' summary(EXAMPLERUN$control_stream)
#'
#' @export
summary.nonmem_control_stream <- function(x, ...) {
  txt <- c(
    str_c("Dataset: "),
    str_c("\tFile: ", x$dataset_file),
    str_c("\tIgnore: ", ""),
    str_c("Estimation(s): ", ifelse(nrow(x$estimations) > 0,
      str_c("\n", sprintf("\t%s", x$estimations$method), collapse = "\n"),
      "No estimation step"
    )),
    str_c("Table(s): ", ifelse(nrow(x$tables) > 0,
      str_c(sort(x$tables$file), collapse = ", "),
      "No output table"
    ))
  )

  writeLines(txt)
}
