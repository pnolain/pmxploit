#' Summarize THETA parameters
#'
#' Summarize typical population parameters values resulting of an estimation step.
#'
#'
#' @inheritParams get_estimation
#'
#' @return A data frame with one row per parameter and the following columns:
#' \itemize{
#' \item \code{id}: parameter ID (ie. THETA1, THETA2, ..., THETAn)
#' \item \code{name}: parameter name
#' \item \code{estimate}: final estimate
#' \item \code{se}: standard error
#' \item \code{rse}: relative standard error
#' \item \code{ci_low}: lower endpoint of the 95\% confidence interval
#' \item \code{ci_up}: upper endpoint of the 95\% confidence interval
#' }
#' @export
#'
#' @examples
#'
#' EXAMPLERUN %>% summarize_thetas()
summarize_thetas <- function(run, estimation_number = NULL) {
  est <- get_estimation(run, estimation_number)

  est$theta
}

#' Summarize OMEGA matrix
#'
#' Summarize variance-covariance matrix of the random parameters values resulting of an estimation step.
#'
#' @return A data frame with one row per parameter and the following columns:
#' \itemize{
#' \item \code{eta1}: first parameter name
#' \item \code{eta2}: second parameter name
#' \item \code{estimate}: final estimate
#' \item \code{se}: standard error
#' \item \code{rse}: relative standard error
#' \item \code{ci_low}: lower endpoint of the 95\% confidence interval
#' \item \code{ci_up}: upper endpoint of the 95\% confidence interval
#' \item \code{cv}: coefficient of variation, considering the random parameter
#' is a variability term associated to a log-normally distributed parameter
#' }
#' @export
#'
#' @examples
#'
#' EXAMPLERUN %>% summarize_omega()
summarize_omega <- function(run, estimation_number = NULL) {
  est <- get_estimation(run, estimation_number)

  est$omega
}

#' Summarize SIGMA matrix
#'
#' Summarize variance-covariance matrix of the residual error parameters values resulting of an estimation step.
#'
#' @inheritParams get_estimation
#'
#' @return A data frame with one row per parameter and the following columns:
#' \itemize{
#' \item \code{epsilon1}: first parameter name
#' \item \code{epsilon2}: second parameter name
#' \item \code{estimate}: final estimate
#' \item \code{se}: standard error
#' \item \code{rse}: relative standard error
#' \item \code{ci_low}: lower endpoint of the 95\% confidence interval
#' \item \code{ci_up}: upper endpoint of the 95\% confidence interval
#' }
#' @export
#'
#' @examples
#'
#' EXAMPLERUN %>% summarize_sigma()
summarize_sigma <- function(run, estimation_number = NULL) {
  est <- get_estimation(run, estimation_number)

  est$sigma
}

#' Summarize shrinkage
#'
#' Summarize shrinkage (ETA, EBV and EPS) of the individual parameters resulting of an estimation step.
#'
#' @param type character vector. Shrinkage type (ETA, EBV or EPS).
#' @inheritParams get_estimation
#'
#' @return A data frame with one row per shrinkage value and the following columns:
#' \itemize{
#' \item \code{type}: shrinkage type (ETA, EBV or EPS)
#' \item \code{parameter}: random parameter name
#' \item \code{shrinkage}: shrinkage value
#' }
#' @export
#'
#' @examples
#'
#' EXAMPLERUN %>% summarize_shrinkage()
#' EXAMPLERUN %>% summarize_shrinkage(type = "ETA")
summarize_shrinkage <- function(run, estimation_number = NULL, type = NULL) {
  est <- get_estimation(run, estimation_number)

  df <- est$shrinkage

  if(is.null(est$shrinkage)) return(NULL)

  shrinkage_types <- toupper(type)

  if(length(shrinkage_types) > 0){
    df <- df %>%
      filter(type %in% shrinkage_types)
  }

  df
}

#' Summarize ETA bars
#'
#' Summarize ETA bars of the individual parameters resulting of an estimation step.
#'
#' @inheritParams get_estimation
#'
#' @return A data frame with one row per parameter and the following columns:
#' \itemize{
#' \item \code{id}: parameter ID (ie. ETA1, ETA2, ..., ETAn)
#' \item \code{name}: parameter name
#' \item \code{value}: eta bar value
#' \item \code{se}: standard error
#' \item \code{n}: sample size
#' \item \code{pvalue}: p-value helping assess whether the sample average is "far" from 0
#' }
#' @export
#'
#' @examples
#' EXAMPLERUN %>% summarize_etabars()
summarize_etabars <- function(run, estimation_number = NULL) {
  est <- get_estimation(run, estimation_number)

  est$eta_bars
}
