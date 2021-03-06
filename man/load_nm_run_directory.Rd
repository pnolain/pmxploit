% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_nm_run.R
\name{load_nm_run_directory}
\alias{load_nm_run_directory}
\title{Load a NONMEM run data from a directory}
\usage{
load_nm_run_directory(
  path,
  dataset_separator = NA,
  dataset_na_string = ".",
  read_initial_values = TRUE,
  load_tables = TRUE,
  control_stream = NULL,
  update_progress = NULL,
  verbose = FALSE
)
}
\arguments{
\item{path}{character. Run folder or archive file path.}

\item{dataset_separator}{(optional) character. Character used as column
separator in the dataset. Default is \code{NA} for automatic detection.}

\item{dataset_na_string}{character. Character string corresponding to missing
values in the dataset. Default is \code{"."}.}

\item{read_initial_values}{logical. If \code{TRUE} (default), parses initial
parameter values from the control stream.}

\item{load_tables}{logical. If \code{TRUE} (default), loads output tables.}

\item{control_stream}{\code{nonmem_control_stream} object. Control stream data, obtained from a prior
call to \code{parse_nm_control_stream()} function.}

\item{update_progress}{(otional) function of the form \code{function(detail, ...)\{\}}.
Useful to follow loading progression.}

\item{verbose}{logical. If \code{TRUE}: prints output messages.}
}
\description{
Load a NONMEM run data from a directory
}
