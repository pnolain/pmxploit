convert_nm_table <- function(table) {
  # factor_columns <- c("ID", "CMT", "MDV", "EVID")

  # for (fc in factor_columns) {
  #   if (fc %in% colnames(table)){
  #     table[[fc]] <-
  #       factor(table[[fc]], levels = sort(unique(table[[fc]])), ordered = TRUE)
  #   }
  #
  # }

  if ("ID" %in% colnames(table)) {
    table$ID <- factor(table$ID, levels = sort(unique(table$ID)), ordered = TRUE)
  }

  # if("CMT" %in% colnames(table))
  #   table$CMT <- factor(table$CMT, levels = sort(unique(table$CMT)), ordered = TRUE)

  # if("MDV" %in% colnames(table))
  #   table$MDV <- factor(table$MDV, levels = c(0, 1), ordered = TRUE)

  if ("EVID" %in% colnames(table)) {
    table$EVID <- factor(table$EVID, levels = c(0, 1, 2, 3, 4), ordered = FALSE)
  }

  table %>% tbl_df()
}

infinite_as_na <- function(values) {
  values[values == 10000000000] <- NA

  values
}

is_matrix_diagonal <- function(m) {
  all(m[lower.tri(m)] == 0, m[upper.tri(m)] == 0)
}

load_lower_triangular_matrix <- function(parent_node) {
  if (is.null(parent_node) | parent_node[[1]] %>% xmlValue() %>% str_trim() == "") {
    return(matrix(NA))
  }

  matrix_values <- xmlSApply(parent_node, function(x) {
    xmlSApply(x, xmlValue) %>% as.numeric()
  })

  m <- diag(length(matrix_values))
  m[upper.tri(m, diag = TRUE)] <-
    matrix_values %>%
    unlist(use.names = FALSE)
  m[lower.tri(m, diag = FALSE)] <-
    t(m)[lower.tri(m, diag = FALSE)] # make it symmetric

  m
}

load_matrix <- function(parent_node, byrow = TRUE) {
  if (is.null(parent_node)) {
    return(NULL)
  }

  matrix_values <- xmlSApply(parent_node, function(x) {
    xmlSApply(x, xmlValue) %>% as.numeric()
  })

  m <-
    matrix(matrix_values %>% unlist(use.names = FALSE), byrow = byrow)

  m
}

load_matrix_estimate_table <- function(estimate_matrix_node, se_matrix_node) {
  lower_estimate <- load_lower_triangular_matrix(estimate_matrix_node)
  lower_se <- NULL

  if (!is.null(se_matrix_node)) {
    lower_se <- load_lower_triangular_matrix(se_matrix_node) %>% infinite_as_na()
  }

  lower_estimate[upper.tri(lower_estimate)] <- NA
  not_zero <- which(lower_estimate != 0, arr.ind = TRUE, useNames = FALSE)

  if (any(not_zero)) {
    row <- not_zero[, 1]
    col <- not_zero[, 2]
    estimate <- lower_estimate[not_zero]

    if (!is.null(lower_se)) {
      estimate_se <- lower_se[not_zero]
    } else {
      estimate_se <- NA
    }

    matrix_df <- data_frame(
      row = row,
      col = col,
      estimate = estimate,
      se = estimate_se,
      rse = abs(estimate_se / estimate),
      ci_low = estimate - 1.96 * estimate_se,
      ci_up = estimate + 1.96 * estimate_se
    )
  } else {
    zeros <- rep(0, nrow(lower_estimate))
    matrix_df <- data_frame(
      row = seq_len(nrow(lower_estimate)),
      col = seq_len(nrow(lower_estimate)),
      estimate = zeros,
      se = NA,
      rse = NA,
      ci_low = NA,
      ci_up = NA
    )
  }

  matrix_df
}
