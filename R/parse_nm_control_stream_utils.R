parse_vector_record <- function(lines, record_name) {
  text <- toupper(lines) %>%
    str_replace("\\$.\\b", "") %>%
    str_trim() %>%
    .[. != ""] %>%
    str_c(collapse = "\n")

  number_patt <- "[\\+\\-]?[0-9\\.]+([eE][\\+\\-]?[0-9\\.]+)?"

  vec_pattern <- "((\\(\\s*(XXX)\\s*(FIX|[,\\s]+(XXX)?[,\\s]*(XXX)?)\\s*\\))|((XXX)\\s*(FIX)?))(\\s*(x|X)\\s*([0-9]+))?" %>%
    str_replace_all("XXX", number_patt)


  init_vector_matches <- str_match_all(text, vec_pattern)[[1]]

  initial_vector_values <- tibble(
    val1 = ifelse(!is.na(init_vector_matches[, 12]), init_vector_matches[, 12], init_vector_matches[, 4]),
    val2 = init_vector_matches[, 7],
    val3 = init_vector_matches[, 9],
    val4 = str_detect(ifelse(!is.na(init_vector_matches[, 14]), init_vector_matches[, 14], init_vector_matches[, 6]), "FIX"),
    n_rep = as.integer(init_vector_matches[, 17])
  )

  if (any(!is.na(initial_vector_values$n_rep))) {
    repetitions <- initial_vector_values %>%
      mutate(rn = row_number()) %>%
      mutate(reps = map2(rn, n_rep, function(a, b) {
        if (is.na(b)) return(a)
        rep(a, b)
      }))

    rep_indices <- unlist(repetitions$reps)

    initial_vector_values <- initial_vector_values %>% slice(rep_indices) %>% select(-n_rep)
  }

  initial_vector_values <- initial_vector_values %>%
    mutate(
      n = row_number(),
      value = ifelse(is.na(val2) & is.na(val3), val1, val2),
      lower_bound = ifelse(!is.na(val2), val1, NA),
      upper_bound = ifelse(!is.na(val2) & !is.na(val3), val3, NA),
      fixed = ifelse(is.na(val4), FALSE, val4)
    ) %>%
    select(n, value, lower_bound, upper_bound, fixed) %>%
    mutate_at(vars(value, lower_bound, upper_bound), as.double)

  initial_vector_values
}

parse_matrix_record <- function(lines, record_name) {
  text <- toupper(lines) %>%
    str_replace_all("(DIAG|DIAGONAL)\\s*\\(\\s*[0-9]+\\s*\\)", "") %>%
    str_replace_all("BLOCK\\s*\\(", "BLOCK\\(") %>%
    str_replace_all("VALUES\\(", "VALUES (") %>%
    str_replace_all("\\(|\\)", " ") %>%
    str_replace_all(",", " ") %>%
    str_replace_all("X", " X ") %>%
    str_replace_all("(I X ED)|(I X)", "IXED") %>%
    str_replace_all("\\bx|X\\s+", "X") %>%
    str_trim()


  om_words <- map(text, ~str_split(., "\\s+")[[1]]) %>% unlist()

  same_pattern <- "SAME(\\d+)?"
  om_recs <- tibble(
    start = which(str_detect(om_words, str_c("\\$", record_name))),
    end = lead(start - 1, default = length(om_words))
  ) %>%
    mutate(
      words = map2(start, end, ~om_words[..1:..2]),
      blk = map_lgl(words, ~any(str_detect(., "BLOCK")))
    ) %>%
    mutate(same = map_int(words, function(x) {
      if (length(s_which <- str_which(x, same_pattern)) == 0) return(0L)

      s_match <- str_match(x[s_which], same_pattern)

      n_same <- as.integer(str_replace_na(s_match[, 2], replacement = "1"))

      n_same
    }))

  om_recs <- om_recs %>%
    mutate(
      n = row_number(),
      is_same = same > 0,
      cum_same = cumsum(same)
    ) %>%
    mutate(
      words2 = ifelse(is_same, lag(words), words),
      words3 = map(words2, ~.[-1]),
      sli = map2(n, same, ~rep(..1, max(1, ..2)))
    )

  sel_seq <- om_recs$sli %>% unlist()

  om_recs <- om_recs %>% slice(sel_seq)

  recs_list <- list()
  previous_item <- NULL

  for (oi in seq_len(nrow(om_recs))) {
    current_item <- NULL

    # om_w2 <- om_recs[oi,]$words2[[1]]
    om_w2 <- om_recs[oi, ]$words3[[1]]
    om_block <- which(str_detect(om_w2, "BLOCK"))
    om_fix <- which(str_detect(om_w2, "FIX"))
    om_rep <- which(str_detect(om_w2, "x|X\\d+"))
    om_val <- setdiff(seq_along(om_w2), c(om_block, om_block + 1, om_fix, om_rep))

    om_block_n <- 1L
    om_values_rec <- NULL
    fixed_values <- FALSE
    n_rep <- 1L

    if (length(om_block) > 0) {
      om_block_n <- str_extract(om_w2[om_block + 1], "[0-9]+") %>% as.integer()

      om_values_rec <- str_which(om_w2, "VALUES")

      if (length(om_values_rec) == 1) {
        om_d <- as.double(om_w2[om_values_rec + 1])
        om_non_d <- as.double(om_w2[om_values_rec + 2])
        om_mat <- diag(om_d, om_block_n, om_block_n)
        om_mat[lower.tri(om_mat, diag = FALSE)] <- om_non_d
        om_mat[upper.tri(om_mat, diag = FALSE)] <- NA
        mat_values <- as.double(t(om_mat)) %>% na.omit()
      } else if (last(om_w2) == "SAME") { # USEFUL ???
        current_item <- previous_item
        current_item$n_record <- oi
        current_item$inner_n <- previous_item$inner_n + om_block_n
      } else {
        mat_values <- as.numeric(om_w2[om_val])
      }
    } else {
      if (length(om_rep) > 0) {
        n_rep <- as.integer(str_extract(om_w2[om_rep], "\\d+"))
        rep_fixed_indices <- 0

        rep_values <- tibble(
          n_times = n_rep,
          word_pos = map_int(om_rep, ~max(om_val[om_val < .])),
          num_pos = map_int(word_pos, ~which(om_val == .))
        )

        not_repeated <- om_val[!(om_val %in% rep_values$word_pos)]

        if (length(not_repeated) > 0) {
          rep_values <- rep_values %>%
            add_row(word_pos = not_repeated, n_times = 1L, num_pos = map_int(word_pos, ~which(om_val == .))) # %>%
        }


        rep_values <- rep_values %>%
          mutate(
            val = as.numeric(om_w2[word_pos]),
            is_fixed = map_lgl(word_pos, ~(. + 1) %in% om_fix)
          ) %>%
          arrange(num_pos) %>%
          mutate(
            values = map2(val, n_times, rep),
            fix_values = map2(is_fixed, n_times, rep)
          )

        mat_values <- unlist(rep_values$values)
        fixed_values <- unlist(rep_values$fix_values)
      } else {
        mat_values <- as.numeric(om_w2[om_val])
      }
    }

    if (is.null(current_item)) {
      current_item <- tibble(
        n_record = oi,
        inner_n = seq_along(mat_values),
        value = mat_values,
        fixed = fixed_values,
        block_length = om_block_n
      )

      if (!is.null(om_values_rec)) {
        current_item$fixed <- length(om_fix) > 0
      } else if (length(om_fix) > 0) {
        if (length(om_rep) == 0) {
          current_item$fixed <- current_item$inner_n %in% (om_fix - seq_along(om_fix) - (length(om_block) > 0))
        }
      }

      if (length(om_block) > 0) {
        diag_m <- diag(om_block_n)
        diag_m[lower.tri(diag_m, diag = TRUE)] <- 1
        coord <- as_tibble(which(diag_m == 1, arr.ind = TRUE)) %>% arrange(row)
        current_item$row <- coord$row
        current_item$column <- coord$col
        current_item$is_diag <- current_item$row == current_item$column
      } else {
        current_item$row <- 1L
        current_item$column <- 1L
        current_item$is_diag <- TRUE
      }
    }

    recs_list <- c(recs_list, list(current_item))

    previous_item <- current_item
  }

  recs_df <- bind_rows(recs_list) %>%
    mutate(n_val1 = cumsum(is_diag) + (column != row)) %>%
    group_by(n_record) %>%
    mutate(n_val2 = ifelse(row == column,
      n_val1,
      n_val1 - (row - column)
    )) %>%
    ungroup()

  matrix_blocks <- recs_df %>%
    filter(block_length > 1) %>%
    group_by(n_record) %>%
    # nest(data = c(row, column, inner_n, n_val1, value, fixed)) %>%
    nest(data = -c(n_record)) %>%
    mutate(
      from = map_int(data, ~min(.$n_val1)),
      to = map_int(data, ~max(.$n_val1)),
      fixed_block = map_lgl(data, ~any(.$fixed))
    ) %>%
    rename(n = n_record) %>%
    # ungroup() %>%
    # mutate(n = row_number()) %>%
    # group_by(n) %>%
    select(n, from, to, fixed_block, data)
    # ungroup() %>%
    # mutate(num = row_number()) %>%
    # select(num, from, to, fixed_block, data)

  recs_values <- recs_df %>%
    mutate(
      n = row_number(),
      in_block = block_length > 1
    ) %>%
    mutate(
      value = ifelse(value == "SAME", NA, value),
      fixed = ifelse(value == "SAME", NA, fixed)
    ) %>%
    select(n, n_val1, n_val2, value, in_block, fixed) %>%
    fill(value, fixed) %>%
    mutate_at(vars(value), as.double)

  list(
    values = recs_values,
    blocks = matrix_blocks
  )
}
