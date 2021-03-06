% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rename_compartments}
\alias{rename_compartments}
\title{Rename model compartments}
\usage{
rename_compartments(run, compartments = NULL)
}
\arguments{
\item{run}{\code{pmxploit} NONMEM run object.}

\item{compartments}{named integer vector with new compartments names as items names and
old compartments number as values.}
}
\value{
A NONMEM run object.
}
\description{
Rename model compartments
}
\examples{
renamed_EXAMPLERUN <- EXAMPLERUN \%>\%
                        rename_compartments(compartments = c("Alirocumab (nM)" = 2,
                                                             "PCSK9 (nM)" = 3))

EXAMPLERUN \%>\% plot_dv_vs_predictions(compartment = 2:3)
renamed_EXAMPLERUN \%>\% plot_dv_vs_predictions(compartment = 2:3)
}
