#' \code{pmxploit} ggplot2 theme
#'
#' A green & white theme
#'
#' @return A ggplot2 theme.
#' @export
#'
#' @examples
#' EXAMPLERUN %>%
#'      plot_residuals(compartment = 2, idv = c("TIME", "PRED"), residuals = "CWRES")+
#'      theme_pmx()
theme_pmx <- function() {
  theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      strip.background = element_rect(fill = "#e6ffe6", linetype = "dashed"),
      strip.text = element_text(size = 15),
      text = element_text(size = 15),
      plot.caption = element_text(size = 10, colour = "darkgrey")
    )
}
