lower_matrix_text <- function(m, fixed = TRUE){
  map_chr(seq_len(nrow(m)), function(i){
    row <- m[i,][1:i]

    row[which(row == 0)] <- 1e-9
    row <- str_pad(row, 21, "right")

    if(fixed && i == 1)
      row <- paste(row, "FIXED")

    str_c(row, collapse = " ")
  }) %>% str_c(collapse = "\n")
}

pad_line <- function(x = NULL, n_pad = 40, pad = "-"){
  preffix <- ifelse(!is.null(x), str_c(x, " "), "")
  str_pad(str_c("; ", preffix), n_pad, "right", pad)
}
