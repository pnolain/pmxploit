% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_individual_ofv.R
\name{plot_individual_ofv}
\alias{plot_individual_ofv}
\title{Individual contribution to objective function plot}
\usage{
plot_individual_ofv(
  run,
  estimation_number = NULL,
  ids = NULL,
  ordered = TRUE,
  auto_legend = TRUE
)
}
\arguments{
\item{run}{\code{pmxploit} NONMEM run object.}

\item{estimation_number}{integer. Number of the estimation step. Default is
\code{NULL}, plotting the last estimation.}

\item{ids}{integer vector of the IDs of the individuals to plot. Default is
\code{NULL}, plotting all the individuals.}

\item{ordered}{logical. Sort individuals based on their individual OFV.}

\item{auto_legend}{logical. When \code{TRUE} (default), default \code{pmxploit} plotting
options are applied (title, labels, axis...).}
}
\value{
A ggplot2 object.
}
\description{
Individual contribution to objective function plot
}
\examples{

random_ids <- sample(1:527, 40)

EXAMPLERUN \%>\% plot_individual_ofv(ids = random_ids)
EXAMPLERUN \%>\% plot_individual_ofv(ids = random_ids, ordered = FALSE)
}
