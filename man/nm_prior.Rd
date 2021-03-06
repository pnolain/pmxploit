% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nm_prior.R
\name{nm_prior}
\alias{nm_prior}
\title{Generate a NONMEM control stream with $PRIOR}
\usage{
nm_prior(
  run,
  df_formula = 0,
  df_values = NULL,
  output_file = NULL,
  dataset_filename = NULL,
  estimations = NULL
)
}
\arguments{
\item{run}{\code{pmxploit} NONMEM run object of the source run.}

\item{df_formula}{integer. Formula to use for computation of degrees of freedom of OMEGA and SIGMA priors.}

\item{output_file}{(optional) character. File path to write the generated
control stream to. Default is \code{NULL}, not saving any file.}

\item{dataset_filename}{(optional) character. Name of the dataset to set in
the $DATA record of the generated control stream. Default is \code{NULL},
leaving $DATA record as it is in the source run.}

\item{estimations}{(optional) list. A list
containing the estimation record(s)
to set for the generated control stream. Each estimation record must be
represented by a list item containing the estimation settings as a named list.}
}
\value{
A character string of the generated control stream code.
}
\description{
Generates a NONMEM control stream for an analysis using prior information from a source run.
}
\examples{
# Control stream generation from pmxploit::EXAMPLERUN
cs1 <- nm_prior(EXAMPLERUN)
# see the output
writeLines(cs1)

# Use prior information with "new_dataset.csv" and save the control stream to "new_study.ctl"
nm_prior(EXAMPLERUN, output_file = "new_study.ctl", dataset_filename = "new_dataset.csv")

# Use prior information with "new_dataset.csv", set estimation to SAEM/IMP and
# save the control stream to "new_study.ctl"
nm_prior(EXAMPLERUN,
         output_file = "new_study.ctl",
         dataset_filename = "new_dataset.csv",
         estimations = list(list(method = "SAEM",
                                 "INTERACTION", NBURN = 400, NITER = 1000,
                                 PRINT = 50, "NOABORT", CTYPE = 3),
                            list(method = "IMP",
                                 NITER = 10, PRINT = 1,
                                 ISAMPLE = 3000, EONLY = 1)))
}
