#' Individual contribution to objective function plot
#'
#' @param ordered logical. Sort individuals based on their individual OFV.
#'
#' @inheritParams plot_ofv
#' @inheritParams plot_individual_profiles
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#'
#' random_ids <- sample(1:527, 40)
#'
#' EXAMPLERUN %>% plot_individual_ofv(ids = random_ids)
#' EXAMPLERUN %>% plot_individual_ofv(ids = random_ids, ordered = FALSE)
plot_individual_ofv <- function(run, estimation_number = NULL, ids = NULL, ordered = TRUE, auto_legend = TRUE){

  selected_est <- get_estimation(run, estimation_number)

  phi_df <- selected_est$phi %>%
    mutate(ID = as.factor(ID))

  if (!is.null(ids)) {
    phi_df <- phi_df %>%
      filter(ID %in% ids)
  }

  ofv_col <- last(colnames(selected_est$iterations))
  ofv_sym <- sym(ofv_col)
  x_sym <- if(ordered) quo(reorder(ID, !!(ofv_sym))) else quo(ID)

  g <- ggplot(phi_df,
              aes(x = !!(x_sym),
                  y =  !!(ofv_sym)))+
    geom_pointrange(aes(ymin = 0, ymax = !!(ofv_sym)))+
    scale_y_continuous(expand= c(0, 0.6))

  if (auto_legend) {
    g <- g + labs(x = "ID",
                  y = "Contribution to Objective Function Value",
                  caption = str_c("Path: ", run$info$path))
  }

  g
}
