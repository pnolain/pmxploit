% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_nm_run.R
\name{load_nm_run}
\alias{load_nm_run}
\title{Load a NONMEM run data}
\usage{
load_nm_run(
  path,
  temp_directory = str_c(tempdir(), "/pmxploit"),
  load_tables = TRUE,
  read_initial_values = TRUE,
  keep_tempfiles = FALSE,
  extract_everything = FALSE,
  dataset_separator = NA,
  dataset_na_string = ".",
  update_progress = NULL,
  verbose = FALSE
)
}
\arguments{
\item{path}{character. Run folder or archive file path.}

\item{temp_directory}{(optional) character. When \code{path} is an archive file,
sets the path of the temporary directory where the archive files will be extracted.}

\item{load_tables}{logical. If \code{TRUE} (default), loads output tables.}

\item{read_initial_values}{logical. If \code{TRUE} (default), parses initial
parameter values from the control stream.}

\item{keep_tempfiles}{logical. If \code{TRUE}, \code{temp_directory} will not be deleted
once run data is loaded.}

\item{extract_everything}{logical. If \code{TRUE}, when \code{path} is an archive file,
extracts all the content of the archive. Otherwise, extracts only the files required for
post-processing analysis (default).}

\item{dataset_separator}{(optional) character. Character used as column
separator in the dataset. Default is \code{NA} for automatic detection.}

\item{dataset_na_string}{character. Character string corresponding to missing
values in the dataset. Default is \code{"."}.}

\item{update_progress}{(otional) function of the form \code{function(detail, ...)\{\}}.
Useful to follow loading progression.}

\item{verbose}{logical. If \code{TRUE}: prints output messages.}
}
\value{
A NONMEM run object. See \code{\link{load_nm_run_directory}}.
}
\description{
Loads NONMEM run results data from either a folder or a archive file (tar.gz, tgz or zip).
}
\examples{
\dontrun{
run <- load_nm_run("path/to/my/run_folder")

run <- load_nm_run("path/to/my/run_archive.tar.gz")
}
}
