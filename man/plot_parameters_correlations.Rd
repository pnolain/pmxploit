% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_parameters_correlations.R
\name{plot_parameters_correlations}
\alias{plot_parameters_correlations}
\title{Plot parameters correlations}
\usage{
plot_parameters_correlations(
  run,
  parameters = NULL,
  type = "heatmap",
  correlation_method = NULL,
  baseline_only = TRUE,
  auto_order = TRUE,
  smoothing_method = NULL,
  smoothing_se = TRUE,
  fixed_ratio = TRUE,
  auto_legend = TRUE
)
}
\arguments{
\item{run}{\code{pmxploit} NONMEM run object.}

\item{parameters}{character vector of parameters names. Default is \code{NULL},
returning all individual parameters (random and post-hoc).}

\item{type}{character. Type of plot. One of \code{heatmap} or \code{scatterplot}.}

\item{correlation_method}{a character string indicating which correlation
coefficient (or covariance) is to be computed. One of \code{"pearson"}
(default), \code{"kendall"}, or \code{"spearman"}: can be abbreviated. If
\code{type = "scatterplot"}, \code{"pearson"} method will be used.}

\item{baseline_only}{logical. Consider only the baseline (= first) values of the subjects. Default is
\code{TRUE}.}

\item{auto_order}{logical. If \code{type = "heatmap"}, auto-orders the
matrix with respect to the distances between values. Default is
\code{TRUE}.}

\item{smoothing_method}{character. If \code{type = "scatterplot"}, corresponds
to a \code{ggplot2} smoothing method.}

\item{smoothing_se}{logical. Display confidence interval around smooth?}

\item{fixed_ratio}{logical. If \code{type = "heatmap"}, plot scaled to a
1:1 ratio. Default is \code{TRUE}.}

\item{auto_legend}{logical. When \code{TRUE} (default), default \code{pmxploit} plotting
options are applied (title, labels, axis...).}
}
\value{
A ggplot2 object.
}
\description{
Returns selected parameters correlations in the form of either a matrix, a
data frame or a plot.
}
\examples{
EXAMPLERUN \%>\% plot_parameters_correlations()

EXAMPLERUN \%>\% plot_parameters_correlations(type = "heatmap")
EXAMPLERUN \%>\% plot_parameters_correlations(type = "heatmap", auto_order = FALSE)

EXAMPLERUN \%>\% plot_parameters_correlations(type = "scatterplot")
EXAMPLERUN \%>\% plot_parameters_correlations(type = "scatterplot", smoothing_method = "lm")
EXAMPLERUN \%>\% plot_parameters_correlations(type = "scatterplot", smoothing_method = "loess")
EXAMPLERUN \%>\% group_by(STUD) \%>\% plot_parameters_correlations(type = "scatterplot")
}
