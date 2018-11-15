#' NONMEM text messages
#'
#' NONMEM text messages extracted from NONMEM /source/txtmsgs.f90 file.
#'
#' @format A data frame with two variables: \code{id}, \code{message}.
"nonmem_txtmsgs"

#' NONMEM reserved names
#'
#' NONMEM reserved names related to DATA, PREDPP and NM-TRAN (cf. NONMEM guides).
#'
#' @format A vector of NONMEM reserved names.
#' @export
"nm_reserved_names"

#' NONMEM example run
#'
#' A NONMEM run example, based on the reference noted below.
#'
#' @format A NONMEM run object of the class \code{nonmem_run}.
#'
#' @name EXAMPLERUN
#'
#' @references Djebli, N., Martinez, JM., Lohan, L. et al. Clin Pharmacokinet (2017) 56: 1155.
#' \url{https://doi.org/10.1007/s40262-016-0505-1}
#'
#' @export
"EXAMPLERUN"
