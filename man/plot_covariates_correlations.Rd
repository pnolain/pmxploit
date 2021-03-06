% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_covariates_correlations.R
\name{plot_covariates_correlations}
\alias{plot_covariates_correlations}
\title{Continuous covariates correlations}
\usage{
plot_covariates_correlations(
  run,
  covariates = NULL,
  baseline_only = TRUE,
  correlation_method = NULL,
  auto_order = TRUE,
  smoothing_method = NULL,
  smoothing_se = TRUE,
  type = "heatmap",
  fixed_ratio = TRUE,
  auto_legend = TRUE
)
}
\arguments{
\item{run}{\code{pmxploit} NONMEM run object.}

\item{covariates}{character vector of continous covariates names. Default is
\code{NULL}, returning all continuous covariates.}

\item{baseline_only}{logical. Consider only the baseline (= first) values of the subjects. Default is
\code{TRUE}.}

\item{correlation_method}{a character string indicating which correlation
coefficient (or covariance) is to be computed. One of \code{"pearson"}
(default), \code{"kendall"}, or \code{"spearman"}: can be abbreviated. If
\code{type = "scatterplot"}, \code{"pearson"} method will be used.}

\item{auto_order}{logical. If \code{type = "heatmap"}, auto-orders the
matrix with respect to the distances between values. Default is
\code{TRUE}.}

\item{smoothing_method}{character. If \code{type = "scatterplot"}, corresponds
to a \code{ggplot2} smoothing method.}

\item{smoothing_se}{logical. Display confidence interval around smooth?}

\item{type}{character. Type of plot. One of \code{heatmap} or \code{scatterplot}.}

\item{fixed_ratio}{logical. If \code{type = "heatmap"}, plot scaled to a
1:1 ratio. Default is \code{TRUE}.}

\item{auto_legend}{logical. When \code{TRUE} (default), default \code{pmxploit} plotting
options are applied (title, labels, axis...).}

\item{split_by}{character. If \code{type = "scatterplot"}, categorical
covariate name to colour observations by group.}
}
\value{
A a ggplot2 object.
}
\description{
Plots the selected continuous covariates correlations.
}
\examples{

cov <- c("AGE", "WT", "BSLDLC", "FBSPCSK", "TBSPCSK", "CLCR")

EXAMPLERUN \%>\%
  plot_covariates_correlations(covariates = cov, type = "heatmap")+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
  legend.position = "bottom")

EXAMPLERUN \%>\% plot_covariates_correlations(covariates = cov, type = "heatmap", auto_order = FALSE)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
  legend.position = "bottom")

EXAMPLERUN \%>\%
  plot_covariates_correlations(covariates = c("AGE", "CLCR"), type = "scatterplot", smoothing_method = "lm")

EXAMPLERUN \%>\% plot_covariates_correlations(covariates = cov, type = "scatterplot")
EXAMPLERUN \%>\% plot_covariates_correlations(covariates = cov, type = "scatterplot", smoothing_method = "lm")
EXAMPLERUN \%>\% plot_covariates_correlations(covariates = cov, type = "scatterplot", smoothing_method = "loess")
EXAMPLERUN \%>\% group_by(STUD) \%>\% plot_covariates_correlations(covariates = cov, type = "scatterplot")
}
