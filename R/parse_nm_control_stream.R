#' Parse a NONMEM control stream
#'
#' Reads the content of a control stream and extracts informations related to
#' input data, model, estimation and output.
#'
#' @param filepath character. If \code{content = NULL}, defines the filepath of
#'   the control stream file.
#' @param content character. Text content of a control stream file.
#'
#' @inheritParams  load_nm_run
#'
#' @return A list representing a NONMEM control stream.
#'
#'   Object structure: \itemize{
#'   \item \code{content}: control stream lines.
#'   \item \code{problem}: problem description.
#'   \item \code{ignore}: list with: \itemize{
#'   \item \code{C}: logical. \code{TRUE} if \code{IGNORE=C} \item \code{@}: logical.
#'    \code{TRUE} if \code{IGNORE=@} \item \code{#}: logical. \code{TRUE} if \code{IGNORE=#}
#'    \item \code{data}: data frame containing other \code{IGNORE} options.
#'   } \item \code{ignore_at}: logical, indicating if if \code{IGNORE=@} is specified.
#'   \item \code{dataset_file}: dataset file name.
#'   \item \code{subroutine}: NONMEM ADVAN subroutine.
#'   \item \code{model_compartments}: tibble of the compartments (name and number of each).
#'   \item \code{subpopulations}: number of subpopulations (for mixture models).
#'   \item \code{input}: tibble of dataset input columns (name, is dropped?, has a synonymn/alias?).
#'   \item \code{parameters_definitions}: tibble of the model parameters (\code{THETAs} and \code{ETAs}).
#'   \item \code{estimations}: successive estimation methods.
#'   \item \code{tables}: output tables with columns definitions.}
#' @export
#'
#' @examples
#' \dontrun{
#' parse_nm_control_stream(filepath = "run_control_stream.con")
#' }
parse_nm_control_stream <- function(filepath = NULL, content = NULL, read_initial_values = TRUE, verbose = FALSE) {
  if (all(is.null(c(content, filepath)))) {
    stop(simpleError("One of `content` or `filepath` arguments is required."))
  }

  if (is.null(content)) {
    if (!file.exists(filepath)) {
      stop(simpleError("File does not exists."))
    }

    content <- read_file(filepath)
  }

  # Clean up control stream
  content_lines <- str_split(content, "\n") %>%
    unlist() %>%
    str_trim()

  # read only the first $PROBLEM
  pb_lines <- str_which(content_lines, "^\\$PROB")

  if (length(pb_lines) > 1) {
    warning(simpleWarning("Several $PROBLEM records were found. Only the first problem will be taken into account."))
    content_lines <- content_lines[1:(pb_lines[2] - 1L)]
  }


  # ignore blank and comments
  uncommented_lines <- content_lines %>%
    str_replace(";.*$", "")

  blank_lines <- which(uncommented_lines == "")

  if (length(blank_lines) > 0) {
    lines <- uncommented_lines[-blank_lines] %>%
      str_trim()
  } else {
    lines <- uncommented_lines %>%
      str_trim()
  }

  lines <- lines %>%
    str_replace_all("\\s+", " ") %>% # reduce blanks to one space
    str_replace_all("\\s=\\s", "=") # clear space around equal symbol


  lines_mapping <- tibble(
    line = lines,
    source_index = setdiff(seq_along(uncommented_lines), blank_lines),
    cleaned_index = seq_along(lines)
  )


  # look for record names
  record_names <- c(
    PROBLEM = "PROB", INPUT = "INP", DATA = "DAT", SUBROUTINE = "SUB", ABBR = "ABBR", PRED = "PRED",
    MODEL = "MODEL", MIX = "MIX", PK = "PK", DES = "DES", ERROR = "ERR", THETA = "THE?TA\\b",
    OMEGA = "OMEGA\\b", SIGMA = "SIGM?A?\\b",
    LEVEL = "LEVEL",
    ANNEAL = "ANNEAL",
    PRIOR = "PRIOR",
    ETAS = "ETAS",
    PHIS = "PHIS",
    THETAI = "THETAI", "THETAR" = "THETAR",
    THETAP = "THETAP\\b", THETAPV = "THETAPV",
    OMEGAP = "OMEGAP\\b", OMEGAPD = "OMEGAPD",
    SIGMAP = "SIGMAP\\b", SIGMAPD = "SIGMAPD",
    MSFI = "MSFI", ESTIMATION = "EST", SIMULATION = "SIM",
    COVARIANCE = "COV", TABLE = "TAB", SCATTERPLOT = "SCAT"
  )

  records_df <- map_df(seq_along(record_names), function(i) {
    x <- record_names[i]
    rec <- names(record_names)[i]

    nums <- str_which(lines, str_c("^\\$(", x, ")"))

    if (length(nums) > 0) {
      tibble(name = rec, start = nums)
    } else {
      NULL
    }
  })

  has_prior <- any(records_df$name == "PRIOR")

  cleaned_records_df <- records_df %>%
    arrange(start) %>%
    mutate(end = ifelse(row_number() == dplyr::n(), length(lines), lead(start) - 1L))

  if (has_prior & !any(records_df$name %in% c("THETAP", "THETAPV", "OMEGAP", "OMEGAPD", "SIGMAP", "SIGMAPD"))) { # read only first $THETA and $OMEGA section
    temp_rec_df <- cleaned_records_df %>%
      mutate(n = row_number())

    rec_to_avoid <- temp_rec_df %>%
      filter(name %in% c("THETA", "OMEGA")) %>%
      arrange(start) %>%
      group_by(name) %>%
      filter(row_number() > 1)

    cleaned_records_df <- cleaned_records_df %>% slice(-rec_to_avoid$n)
  }

  # link $TABLE to their $ESTIMATION or $SIMULATION
  est_sim_recs <- cleaned_records_df %>%
    filter(name %in% c("ESTIMATION", "SIMULATION")) %>%
    rename(task = name) %>%
    mutate(n_task = row_number())

  tab_recs <- cleaned_records_df %>%
    filter(name == "TABLE") %>%
    mutate(task = map_int(start, ~length(which(est_sim_recs$start < .))))


  cleaned_records_df <- cleaned_records_df %>%
    full_join(tab_recs, by = c("name", "start", "end"))

  grouped_records_df <- cleaned_records_df %>%
    group_by(name) %>%
    summarise(
      n = dplyr::n(),
      start = as.integer(min(start)),
      end = as.integer(max(end))
    ) %>%
    arrange(start)

  duplicated_records <- grouped_records_df %>% filter(n > 1)

  if (any(!(dup_rec <- duplicated_records$name %in% c(
    "THETA", "THETAP", "THETAPV",
    "OMEGA", "OMEGAP", "OMEGAPD",
    "SIGMA", "SIGMAP", "SIGMAPD",
    "ESTIMATION", "TABLE", "ABBR", "SCATTERPLOT"
  )))) {
    stop(simpleError(sprintf("Duplication of the following record(s) in the control stream is not allowed: %s", str_c(duplicated_records$name[!dup_rec], collapse = ", "))))
  }

  records <- grouped_records_df %>%
    split(.$name) %>%
    map(function(x) {
      list(start = x$start, end = x$end)
    })
  # %>%
  #   left_join(select(lines_mapping, -line), by = c("end" = "cleaned_index")) %>%
  #   rename(source_end = source_index)


  grouped_records_df <- grouped_records_df %>%
    left_join(select(lines_mapping, -line), by = c("start" = "cleaned_index")) %>%
    rename(source_start = source_index) %>%
    mutate(source_end = as.integer(lead(source_start - 1L, default = length(content_lines))))


  # $PROBLEM----
  pb_text <- NULL
  if (length(records$PROBLEM) > 0) {
    if (verbose) print("Parsing $PROBLEM...")

    pb_rec <- grouped_records_df %>% filter(name == "PROBLEM")

    pb_text <- content_lines[pb_rec$source_start:pb_rec$source_end] %>%
      str_c(collapse = "\n") %>%
      str_replace("\\$(PROBLEM|PROB)", "") %>%
      str_trim()
  }

  # $INPUT----
  input_matches <- NULL
  if (length(records$INPUT) > 0) {
    if (verbose) print("Parsing $INPUT...")

    # Reading dataset columns and aliases
    input_text <- lines[records$INPUT$start:records$INPUT$end] %>%
      # str_replace_all("\\s*=\\s*", "=") %>%
      str_c(collapse = "\n") # lines[records$INPUT:(records$DATA-1)] %>% str_c(collapse = "\n")
    splitted_input <- str_split(input_text, "\\s")[[1]]

    column_pattern <- "^([\\w]+)=?([\\w]*)$"
    input_matches <- str_match(splitted_input, column_pattern) %>%
      na.omit() %>%
      as_tibble() %>%
      rename(match = V1, left = V2, right = V3) %>%
      mutate(
        dropped = (left == "DROP" | right == "DROP"),
        column = ifelse(dropped, ifelse(left == "DROP", right, left),
          ifelse(right == "", left,
            ifelse(left %in% nm_reserved_names, right, left)
          )
        ),
        synonym = ifelse(dropped, NA,
          ifelse(right == "", NA,
            ifelse(left %in% nm_reserved_names, left, right)
          )
        )
      ) %>%
      # mutate(column = ifelse(dropped, ifelse(left == "DROP", right, left), dropped)) %>%
      select(-left, -right) #%>%
      # filter(nchar(column) > 0)
  }

  # $DATA----
  data_filename <- NULL
  ignore_txt <- NULL
  ignore_df <- NULL
  if (length(records$DATA) > 0) {
    if (verbose) print("Parsing $DATA...")

    data_text <- lines[records$DATA$start:records$DATA$end] %>% str_c(collapse = " ") # lines[records$DATA:(records$SUBROUTINE-1)] %>% str_c(collapse = " ")
    data_pattern <- "^\\$DATA\\s*[\"']?([^\\s\"']+)"
    if (str_detect(data_text, data_pattern)) {
      data_filename <- str_match(data_text, data_pattern)[, 2]
      data_text <- data_text %>% str_remove(data_pattern) %>% str_trim()
    }

    ignore_txt_pattern <- "IGNORE(=|\\s*)['\"]?([aA-zZ@#])+['\"]?"

    if (str_detect(data_text, ignore_txt_pattern)) {
      ignore_txt <- str_match(data_text, ignore_txt_pattern)[, 3]

      data_text <- data_text %>% str_remove(ignore_txt_pattern) %>% str_trim()
    }


    # ignore_at <- str_detect(data_text, "IGNORE=@")
    # ignore_hash <- str_detect(data_text, "IGNORE=#")

    # input_cols <- input_matches %>% filter(!dropped) %>% pull(column)
    not_dropped_cols <- input_matches %>% filter(!dropped)
    input_cols <- na.omit(unique(c(not_dropped_cols$column, not_dropped_cols$synonym)))

    # fortran_ops <- c("==" = "EQ", ">=" = "GE", ">" = "GT", "<=" = "LE", "<" = "LT", "!=" = "NE")
    fortran_ops <- tribble(
      ~fortran, ~r,
      ".EQ.", "==",
      "==", "==",
      "=", "==",
      ".GE.", ">=",
      ">=", ">=",
      ".GT.", ">",
      ">", ">",
      ".LE.", "<=",
      "<=", "<=",
      ".LT.", "<",
      "<", "<",
      ".NE.", "!=",
      "/=", "!="
    )

    data_text<- data_text %>% str_split("\\s+") %>% unlist()
    ignore_group_pattern <- "IGNORE\\s*=?\\s*\\((.+)\\)"

    if (any(str_detect(data_text, ignore_group_pattern))) {
      ignore_matches <- str_match(data_text, ignore_group_pattern)
      ignore_text <- ignore_matches[, 2] %>%
        na.omit() %>%
        str_c(collapse = ",") %>%
        str_replace_all("\\s", "")

      split_ignore <- ignore_text %>% str_split(",") %>% unlist() %>% str_remove_all("\"|'")

      ignore_df <- tibble(condition = split_ignore) %>%
        # separate(condition, c("column", "operator", "value"), convert = TRUE) %>%
        extract(condition, c("column", "operator", "value"),
          regex = sprintf(
            "(%s)[\\.]?(%s)[\\.]?['\"]?(.+)['\"]?",
            str_c(input_cols, collapse = "|"),
            str_c(fortran_ops$fortran, collapse = "|")
          ),
          convert = TRUE
        ) %>%
        mutate(operator = toupper(operator)) %>%
        mutate(operator = plyr::mapvalues(operator, fortran_ops$fortran, fortran_ops$r, warn_missing = FALSE))
    }


    # separated IGNORE statements
    ignore_pattern2 <- sprintf(
      "IGNORE\\s*=?\\s*\\((%s)\\s*(%s)\\s*([0-9\\.\\-]+)\\)",
      str_c(input_cols, collapse = "|"),
      str_c(fortran_ops$fortran, collapse = "|")
    )

    ignore_matches2 <- str_match_all(data_text, ignore_pattern2)

    if (length(unlist(ignore_matches2)) > 0) {
      ignore_df2 <- ignore_matches2 %>%
        .[[1]] %>%
        as_tibble() %>%
        rename(column = V2, operator = V3, value = V4) %>%
        mutate(
          operator = plyr::mapvalues(operator, fortran_ops$fortran, fortran_ops$r, warn_missing = FALSE),
          value = as.numeric(value)
        ) %>%
        select(column, operator, value)

      ignore_df <- unique(bind_rows(ignore_df, ignore_df2))
    }

    if(!is.null(ignore_df))
      ignore_df <- ignore_df %>% mutate(value = as.numeric(value))
  }

  compartments <- NULL
  advan <- NULL
  sub_extra_files <- NULL
  # $SUBROUTINE----
  if (length(records$SUBROUTINE) > 0) {
    if (verbose) print("Parsing $SUBROUTINE...")

    sub_text <- lines[records$SUBROUTINE$start:records$SUBROUTINE$end]
    advan_match <- str_match(sub_text, "ADVAN=?[0-9]+")
    advan <- ifelse(length(advan_match), as.character(advan_match), NA)

    if (!is.na(advan)) {
      advan <- str_remove(advan, "=")
      if (advan %in% c("ADVAN1", "ADVAN10")) {
        compartments <- tibble(cmt = 1, name = "Central", dv_target = TRUE)
      } else if (advan == "ADVAN2") {
        compartments <- tibble(cmt = 1:2, name = c("Depot", "Central"), dv_target = c(FALSE, TRUE))
      } else if (advan == "ADVAN3") {
        compartments <- tibble(cmt = 1:2, name = c("Central", "Peripheral"), dv_target = c(TRUE, FALSE))
      } else if (advan == "ADVAN4") {
        compartments <- tibble(cmt = 1:3, name = c("Depot", "Central", "Peripheral"), dv_target = c(FALSE, TRUE, FALSE))
      } else if (advan == "ADVAN11") {
        compartments <- tibble(cmt = 1:3, name = c("Central", "Peripheral1", "Peripheral2"), dv_target = c(TRUE, FALSE, FALSE))
      } else if (advan == "ADVAN12") {
        compartments <- tibble(cmt = 1:4, name = c("Depot", "Central", "Peripheral1", "Peripheral2"), dv_target = c(FALSE, TRUE, FALSE, FALSE))
      }
    }

    other_pattern <- "\\b(OTHER|INFN)=([^\\s]+)"
    if (str_detect(sub_text, other_pattern)) {
      other_matches <- str_match_all(sub_text, other_pattern)[[1]]
      sub_extra_files <- other_matches[, 3] %>% str_replace_all("'|\"", "")
    }
  }

  # $MODEL----

  if (length(records$MODEL) > 0) {
    if (verbose) print("Parsing $MODEL...")

    model_lines <- lines[records$MODEL$start:records$MODEL$end]

    cmt_pattern <- "\\b(COMP|COMPARTMENT)=?\\(?['\\\"]?([\\w]+)['\\\"]?\\)?"

    mod_rec_lines <- str_c(model_lines, collapse = "\n")

    cp_matches <- str_match_all(mod_rec_lines, cmt_pattern)
    cp_names <- cp_matches[[1]][, 3]

    if (length(cp_names) > 0) {
      compartments <- tibble(
        cmt = seq_along(cp_names),
        name = cp_names
      )
    } else {
      n_cmt_pattern <- "\\bNCOMP\\w*\\s*=?\\s*([0-9]+)"
      n_cmt_matches <- str_match(mod_rec_lines, n_cmt_pattern)
      if (length(n_cmt_matches) > 0) {
        n_cmts <- as.integer(n_cmt_matches[, 2])
        compartments <- tibble(
          cmt = seq_len(n_cmts),
          name = str_c("CMT_", cmt)
        )
      } else {
        warning(simpleWarning("Compartment names not found."))
      }
    }
  }

  # $MIX
  n_subpop <- 1L
  if (length(records$MIX) > 0) {
    if (verbose) print("Parsing $MIX")

    mix_text <- lines[records$MIX$start:records$MIX$end]
    nspop_line <- str_subset(mix_text, "NSPOP=([0-9]+)")
    if (length(nspop_line) == 1) {
      n_subpop <- str_extract(nspop_line, "[0-9]+") %>% as.integer()
    }
  }

  # $PK---- or $PRED
  parameters_definitions <- NULL
  mu_definitions <- NULL

  main_record_name <- "PK"

  if (is.null(records$PK)) {
    if (!is.null(records$PRED)) {
      main_record_name <- "PRED"
    }
  }

  main_record <- records[[main_record_name]]

  if (length(main_record) > 0) {
    if (verbose) print(sprintf("Parsing $%s...", main_record_name))

    pk_lines <- lines[main_record$start:main_record$end]

    # look also in $ERROR for declared THETA
    if (length(records$ERROR) > 0) {
      pk_lines <- c(
        pk_lines,
        lines[records$ERROR$start:records$ERROR$end]
      )
    }

    # remove all spaces to facilitate regex analysis
    pk_lines <- pk_lines %>% str_replace_all("\\s", "")

    # Extract theta names
    theta_pattern <- "^(\\b(?![0-9])\\w+)=THETA\\(([0-9]+)\\)$"
    theta_lines <- str_which(pk_lines, theta_pattern)
    theta_matches <- NULL

    if (length(theta_lines) > 0) {
      theta_matches <- str_match(pk_lines[theta_lines], theta_pattern) %>%
        as_tibble() %>%
        mutate(V3 = str_trim(V3)) %>%
        mutate(type = "theta", id = str_c("THETA", V3), n = as.integer(V3)) %>%
        rename(name = V2) %>%
        select(n, id, type, name)

      parameters_definitions <- theta_matches
    }

    # look for THETAs not explicitely declared
    theta_pattern_2 <- "\\bTHETA\\(([0-9]+)\\)"
    theta_lines_2 <- str_which(pk_lines, theta_pattern_2)
    if (length(theta_lines_2) > 0) {
      theta_matches_2 <- str_match_all(pk_lines[theta_lines_2], theta_pattern_2) %>%
        map_df(as_tibble) %>%
        rename(n = V2) %>%
        select(n) %>%
        mutate(n = as.integer(as.character(n)), type = "theta", id = str_c("THETA", n), name = id) %>%
        select(n, id, type, name) %>%
        as_tibble()

      if (!is.null(theta_matches)) {
        theta_matches_2 <- theta_matches_2 %>% filter(!(n %in% theta_matches$n))
      }

      parameters_definitions <- bind_rows(parameters_definitions, theta_matches_2)
    }

    # Extract eta names
    eta_pattern <- "^(\\b(?![0-9])\\w+)=ETA\\(([0-9]+)\\)$"
    eta_lines <- str_which(pk_lines, eta_pattern)
    eta_matches <- NULL

    if (length(eta_lines) > 0) {
      eta_matches <- str_match(pk_lines[eta_lines], eta_pattern) %>%
        as_tibble() %>%
        mutate(V3 = str_trim(V3)) %>%
        mutate(type = "eta", id = str_c("ETA", V3), n = as.integer(V3)) %>%
        rename(name = V2) %>%
        select(n, id, type, name)

      parameters_definitions <- bind_rows(parameters_definitions, eta_matches)
    }

    # look for ETAs not explicitely declared
    eta_pattern_2 <- "\\bETA\\(([0-9]+)\\)"
    eta_lines_2 <- str_which(pk_lines, eta_pattern_2)
    if (length(eta_lines_2) > 0) {
      eta_matches_2 <- str_match_all(pk_lines[eta_lines_2], eta_pattern_2) %>%
        map_df(as_tibble) %>%
        rename(n = V2) %>%
        select(n) %>%
        mutate(n = as.integer(as.character(n)), type = "eta", id = str_c("ETA", n), name = id) %>%
        select(n, id, type, name) %>%
        as_tibble() %>%
        unique() # important to not duplicate if ETA(n) is used for different parameters calculations

      if (!is.null(eta_matches)) {
        eta_matches_2 <- eta_matches_2 %>% filter(!(n %in% eta_matches$n))
      }

      parameters_definitions <- bind_rows(parameters_definitions, eta_matches_2)
    }

    # Extract eps names
    eps_pattern <- "^(\\b(?![0-9])\\w+)=(EPS|ERR)\\(([0-9]+)\\)$"
    eps_lines <- str_which(pk_lines, eps_pattern)
    eps_matches <- NULL

    if (length(eps_lines) > 0) {
      eps_matches <- str_match(pk_lines[eps_lines], eps_pattern) %>%
        as_tibble() %>%
        mutate(V4 = str_trim(V4)) %>%
        mutate(type = "epsilon", id = str_c(V3, V4), n = as.integer(V4)) %>%
        rename(name = V2) %>%
        select(n, id, type, name)

      parameters_definitions <- bind_rows(parameters_definitions, eps_matches)
    }

    # look for EPSs not explicitely declared
    eps_pattern_2 <- "\\b(EPS|ERR)\\(([0-9]+)\\)"
    eps_lines_2 <- str_which(pk_lines, eps_pattern_2)
    if (length(eps_lines_2) > 0) {
      eps_matches_2 <- str_match_all(pk_lines[eps_lines_2], eps_pattern_2) %>%
        map_df(as_tibble) %>%
        mutate(n = as.integer(as.character(V3)), type = "epsilon", id = str_c(V2, V3), name = id) %>%
        select(n, id, type, name) %>%
        as_tibble() %>%
        unique() # important to not duplicate if EPS(n) is used for different parameters calculations

      if (!is.null(eps_matches)) {
        eps_matches_2 <- eps_matches_2 %>% filter(!(n %in% eps_matches$n))
      }

      parameters_definitions <- bind_rows(parameters_definitions, eps_matches_2)
    }

    known_thetas <- filter(parameters_definitions, type == "theta")

    # Look for MU_referencing
    mu_pattern <- sprintf(
      "^(\\bMU_([0-9]+))=(.*(THETA\\(([0-9]+)\\)|%s).*)$",
      str_c(known_thetas$name, collapse = "|")
    )
    mu_lines <- str_which(pk_lines, mu_pattern)
    mu_matches <- NULL

    if (length(mu_lines) > 0) {
      mu_matches <- str_match(pk_lines[mu_lines], mu_pattern) %>%
        as_tibble() %>%
        mutate(type = "mu", id = str_c("MU_", V3), n = as.integer(V3)) %>%
        rename(rhs = V4) %>%
        select(n, id, type, rhs) %>%
        arrange(n)

      mu_definitions <- mu_matches

      # if(nrow(mu_definitions) > 0){
      #   parameters_definitions <- parameters_definitions %>% filter(!(name %in% mu_definitions$id))
      # }
    }
  }

  # $ABBR
  if (length(records$ABBR) > 0) {
    if (verbose) print("Parsing $ABBR...")

    abbr_lines <- lines[records$ABBR$start:records$ABBR$end]

    for (i in seq_along(abbr_lines)) {
      line <- abbr_lines[i]
      # Look for MU_referencing
      abbr_replace_pattern <-
        "^\\$ABBR\\sREPLACE\\s(THETA|ETA|EPS|ERR)\\((.+)\\)\\s?=\\s?(THETA|ETA|EPS|ERR)\\((.+)\\)$"
      abbr_replace_lines <- str_which(abbr_lines, abbr_replace_pattern)
      abbr_replace_matches <- NULL

      if (length(abbr_replace_lines) > 0) {
        abbr_replace_matches <- str_match(abbr_lines[abbr_replace_lines], abbr_replace_pattern) %>%
          as_tibble() %>%
          mutate(type = ifelse(V2 == "THETA", "theta", ifelse(V2 == "ETA", "eta", "epsilon")),
                 n = map2(V3, V5, function(lhs, rhs){
                   name_params <- str_split(lhs, pattern = ",")[[1]] %>% str_trim

                   n_params <- NA_integer_
                   if(str_detect(tolower(rhs), "to|:")){
                     params_seq <- str_split(rhs, pattern = "to|:")[[1]] %>% str_trim %>% parse_number
                     n_params <- params_seq[1]:params_seq[2]
                   } else if(str_detect(tolower(rhs), ",")) {
                     n_params <- str_split(rhs, pattern = ",")[[1]] %>% str_trim %>% parse_number
                   } else {
                     as.integer(n_params)
                   }
                   tibble(name = name_params,
                          n = n_params,
                          multiple = length(name_params) != length(n_params))
                 })) %>%
          unnest(c(n)) %>%
          mutate(name = ifelse(multiple, str_c(name, n), name),
                 id = str_c(V2, n), n = as.integer(n)) %>%
          select(n, id, type, name) %>%
          arrange(n) %>%
          filter(!(id %in% parameters_definitions$id))

        if (nrow(abbr_replace_matches) > 0) {
          parameters_definitions <- bind_rows(parameters_definitions, abbr_replace_matches)
        }
      }
    }
  }


  initial_values <- NULL
  blocks <- NULL

  etas_df <- epsilons_df <- NULL

  if (!is.null(parameters_definitions)) {
    etas_df <- parameters_definitions %>% filter(type == "eta")
    epsilons_df <- parameters_definitions %>% filter(type == "epsilon")
  }

  if (read_initial_values) {
    # $THETA
    if (length(records$THETA) > 0) {
      if (verbose) print("Parsing $THETA...")

      initial_theta_values <- parse_vector_record(lines[records$THETA$start:records$THETA$end], record_name = "THETA")

      initial_values$THETA <- right_join(parameters_definitions %>% filter(type == "theta") %>% select(n, id, name),
        initial_theta_values,
        by = "n"
      )
    }

    # $THETAP
    if (length(records$THETAP) > 0) {
      if (verbose) print("Parsing $THETAP")

      initial_thetap_values <- parse_vector_record(lines[records$THETAP$start:records$THETAP$end], record_name = "THETAP")

      initial_values$THETAP <- right_join(parameters_definitions %>% filter(type == "theta") %>% select(n, id, name),
        initial_thetap_values,
        by = "n"
      )
    }

    # $THETAPV
    if (length(records$THETAPV) > 0) {
      if (verbose) print("Parsing $THETAPV...")

      thetapv_record <- parse_matrix_record(lines[records$THETAPV$start:records$THETAPV$end], record_name = "THETAPV")

      thetas_df <- parameters_definitions %>% filter(type == "theta")

      thetapv_record_values <- thetapv_record$values %>%
        mutate(
          theta1 = plyr::mapvalues(n_val1, thetas_df$n, thetas_df$name, warn_missing = FALSE),
          theta2 = plyr::mapvalues(n_val2, thetas_df$n, thetas_df$name, warn_missing = FALSE)
        ) %>%
        select(n, theta1, theta2, value, in_block, fixed)

      initial_values$THETAPV <- thetapv_record_values
      blocks$THETAPV <- thetapv_record$blocks
    }

    # $OMEGA
    if (length(records$OMEGA) > 0) {
      if (verbose) print("Parsing $OMEGA...")

      omega_record <- parse_matrix_record(lines[records$OMEGA$start:records$OMEGA$end], record_name = "OMEGA")

      omega_record_values <- omega_record$values %>%
        mutate(
          eta1 = plyr::mapvalues(n_val1, etas_df$n, etas_df$name, warn_missing = FALSE),
          eta2 = plyr::mapvalues(n_val2, etas_df$n, etas_df$name, warn_missing = FALSE)
        ) %>%
        select(n, eta1, eta2, value, in_block, fixed)

      initial_values$OMEGA <- omega_record_values
      blocks$OMEGA <- omega_record$blocks
    }

    # $OMEGAP
    if (length(records$OMEGAP) > 0) {
      if (verbose) print("Parsing $OMEGAP...")

      omegap_record <- parse_matrix_record(lines[records$OMEGAP$start:records$OMEGAP$end], record_name = "OMEGAP")

      omegap_record_values <- omegap_record$values %>%
        mutate(
          eta1 = plyr::mapvalues(n_val1, etas_df$n, etas_df$name, warn_missing = FALSE),
          eta2 = plyr::mapvalues(n_val2, etas_df$n, etas_df$name, warn_missing = FALSE)
        ) %>%
        select(n, eta1, eta2, value, in_block, fixed)

      initial_values$OMEGAP <- omegap_record_values
      blocks$OMEGAP <- omegap_record$blocks
    }

    # $OMEGAPD
    if (length(records$OMEGAPD) > 0) {
      if (verbose) print("Parsing $OMEGAPD...")

      initial_omegapd_values <- parse_vector_record(lines[records$OMEGAPD$start:records$OMEGAPD$end], record_name = "OMEGAPD")

      initial_values$OMEGAPD <- initial_omegapd_values
    }

    # $SIGMA
    if (length(records$SIGMA) > 0) {
      if (verbose) print("Parsing $SIGMA...")

      sigma_record <- parse_matrix_record(lines[records$SIGMA$start:records$SIGMA$end], record_name = "SIGMA")

      epsilons_df <- parameters_definitions %>% filter(type == "epsilon")

      sigma_record_values <- sigma_record$values %>%
        mutate(
          epsilon1 = plyr::mapvalues(n_val1, epsilons_df$n, epsilons_df$name, warn_missing = FALSE),
          epsilon2 = plyr::mapvalues(n_val2, epsilons_df$n, epsilons_df$name, warn_missing = FALSE)
        ) %>%
        select(n, epsilon1, epsilon2, value, in_block, fixed)

      initial_values$SIGMA <- sigma_record_values
      blocks$SIGMA <- sigma_record$blocks
    }

    # $SIGMAP
    if (length(records$SIGMAP) > 0) {
      if (verbose) print("Parsing $SIGMAP...")

      sigmap_record <- parse_matrix_record(lines[records$SIGMAP$start:records$SIGMAP$end], record_name = "SIGMAP")

      sigmap_record_values <- sigmap_record$values %>%
        mutate(
          eta1 = plyr::mapvalues(n_val1, epsilons_df$n, epsilons_df$name, warn_missing = FALSE),
          eta2 = plyr::mapvalues(n_val2, epsilons_df$n, epsilons_df$name, warn_missing = FALSE)
        ) %>%
        select(n, eta1, eta2, value, in_block, fixed)

      initial_values$SIGMAP <- sigmap_record_values
      blocks$SIGMAP <- sigmap_record$blocks
    }

    # $SIGMAPD
    if (length(records$SIGMAPD) > 0) {
      if (verbose) print("Parsing $SIGMAPD...")

      initial_sigmapd_values <- parse_vector_record(lines[records$SIGMAPD$start:records$SIGMAPD$end], record_name = "SIGMAPD")

      initial_values$SIGMAPD <- initial_sigmapd_values
    }
  }


  # $ESTIMATION
  est_matches <- NULL
  if (length(records$ESTIMATION) > 0) {
    if (verbose) print("Parsing $ESTIMATION...")

    est_matches <- tibble(
      method = character(),
      interaction = logical()
    )

    est_pattern <- ".+\bMETHOD\b=([^\\s]+)\\s*((NOLAP|LAP)[^\\s]*)?\\s*(CENTERING)?"
    est_pattern <- ".+METHOD=([^\\s]+)\\s*((NOLAP|LAP)[^\\s]*)?\\s*(CENTERING)?"

    # est_lines <- lines[records$ESTIMATION$start:records$ESTIMATION$end] %>%
    #   toupper() %>%
    #   str_c(collapse = " ") %>%
    #   str_split("\\s(?=\\$)")

    est_records_df <- cleaned_records_df %>%
      filter(name == "ESTIMATION")

    if (nrow(est_records_df) > 0) {
      est_matches <- map_df(seq_len(nrow(est_records_df)), .id = NULL, function(i) {
        x <- est_records_df[i, ]

        start <- x$start
        end <- x$end

        est_def <- lines[start:end] %>% toupper() %>% str_c(collapse = " ")

        if (str_detect(est_def, est_pattern)) {
          m <- str_match(est_def, est_pattern)

          method <- m[, 2]

          method_txt <- ifelse(str_detect(method, "^0|ZERO$"),
            "FO",
            ifelse(str_detect(method, "^(1|COND)"),
              ifelse(!is.na(m[, 3]) & str_detect(m[, 3], "^LAP"),
                "LAPLACE",
                "FOCE"
              ),
              method
            )
          )

          if (str_detect(method, "^1|COND")) {
            method_txt <- str_c(ifelse(!is.na(m[, 5]), "CENTERING ", ""), method_txt)
          }

          tibble(
            n = i,
            method = method_txt,
            start = start,
            end = end
          )
        } else {
          NULL
        }
      })
    }
  }

  # COVARIANCE
  covariance <- NULL
  if (length(records$COVARIANCE) > 0) {
    if (verbose) print("Parsing $COVARIANCE")


    cov_text <- lines[records$COVARIANCE$start:records$COVARIANCE$end] %>%
      str_c(collapse = "\n")

    cov_matrix_pattern <- "MATRIX=(R|S)"

    if (str_detect(cov_text, cov_matrix_pattern)) {
      mat_m <- str_match(cov_text, cov_matrix_pattern)

      covariance$matrix <- mat_m[, 2]
    } else {
      covariance$matrix <- "DEFAULT"
    }

    sir_pattern <- "SIRSAMPLE=([\\-0-9]+)"

    if (str_detect(cov_text, sir_pattern)) {
      sir_m <- str_match(cov_text, sir_pattern)

      covariance$sir <- list(sample = as.numeric(sir_m[, 2]))
    }
  }


  # $SIMULATION
  sim_matches <- NULL
  if (length(records$SIMULATION) > 0) {
    if (verbose) print("Parsing $SIMULATION...")

    sim_matches <- tibble(
      method = character(),
      interaction = logical()
    )

    sim_pattern <- ".+(N?SUB\\w*)=([0-9]+)"

    sim_lines <- lines[records$SIMULATION$start:records$SIMULATION$end] %>%
      str_c(collapse = " ") %>%
      str_split("\\s(?=\\$)")

    sim_records_df <- cleaned_records_df %>%
      filter(name == "SIMULATION")

    if (nrow(sim_records_df) > 0) {
      sim_matches <- map_df(seq_len(nrow(sim_records_df)), .id = NULL, function(i) {
        x <- sim_records_df[i, ]

        start <- x$start
        end <- x$end

        sim_def <- lines[start:end] %>% str_c(collapse = " ")

        if (str_detect(sim_def, sim_pattern)) {
          m <- str_match(sim_def, sim_pattern)

          n_subpb <- m[, 3]

          tibble(
            n = i,
            n_subproblems = as.integer(n_subpb)
          )
        } else {
          NULL
        }
      })
    }
  }

  # Extra files
  include_pattern <- "^(include|INCLUDE)\\s([^\\s]+)"
  include_files <- NULL
  if (length(include_lines <- str_subset(lines, include_pattern)) > 0) {
    include_text <- str_c(include_lines, collapse = " ")
    include_matches <- str_match_all(include_text, include_pattern)[[1]]
    include_files <- include_matches[, 3] %>% str_replace_all("'|\"", "")
  }

  # $TABLE----
  tab_matches <- NULL
  if (length(records$TABLE) > 0) {
    if (verbose) print("Parsing $TABLE...")

    tab_pattern <- ".+\\bFILE=([^\\s]+).*"
    tab_format_pattern <- ".+FORMAT=([^\\s]+)"

    tab_records_df <- cleaned_records_df %>%
      filter(name == "TABLE")

    if (nrow(tab_records_df) > 0) {
      tab_matches <- tab_records_df %>%
        split(.$start) %>%
        map_df(function(x) {
          start <- x$start
          end <- x$end

          tab_def <- lines[start:end] %>%
             .[!str_detect(., include_pattern)] %>% # remove INCLUDE instructions, if any
            str_c(collapse = " ") %>%
            str_replace("file", "FILE") %>%
            str_replace("format", "FORMAT")

          if (str_detect(tab_def, tab_pattern)) {
            m <- str_match(tab_def, tab_pattern)

            file <- m[, 2]

            has_trailing_numbers <- str_detect(file, "[0-9]+$")

            if (has_trailing_numbers) {
              num_loc <- str_locate(file, "[0-9]+$") # [[1]]
              text_loc <- invert_match(num_loc)
              file <- str_sub(file, text_loc[, "start"], text_loc[, "end"])[[1]]
            }

            nm_tab_args <- c(
              "PRINT", "NOPRINT", "FILE",
              "NOHEADER", "ONEHEADER", "ONEHEADERALL",
              "NOTITLE", "NOLABEL",
              "FIRSTONLY", "LASTONLY", "FIRSTLASTONLY", "NOFORWARD", "FORWARD",
              "APPEND", "NOAPPEND",
              "FORMAT", "LFORMAT", "RFORMAT",
              "NOSUB", "EXCLUDE_BY",
              "PARAFILE",
              "ESAMPLE", "WRESCHOL", "SEED",
              "RANMETHOD", "VARCALC", "FIXEDETAS",
              "UNCONDITIONAL", "CONDITIONAL", "OMITTED"
            )


            etas_list_pattern <- "ETAS\\s*\\(\\s*([0-9]+)\\s*:\\s*(LAST|[0-9]+)\\s*\\)"
            etas_list <- NULL

            # ETA list in output table ETAS(1:LAST) or ETAS(1:N)
            if (str_detect(toupper(tab_def), etas_list_pattern)) {
              etas_list_match <- str_match_all(toupper(tab_def), etas_list_pattern)[[1]]

              etas_list_df <- etas_list_match %>%
                as_tibble() %>%
                mutate(
                  start = as.integer(V2),
                  end = as.integer(ifelse(V3 == "LAST", max(etas_df$n), V3))
                ) %>%
                mutate(etas_names = map2(start, end, ~str_c("ETA", ..1:..2)))

              etas_list <- unlist(etas_list_df$etas_names)

              for (i in seq_len(nrow(etas_list_df))) {
                tab_def <- toupper(tab_def) %>%
                  str_replace(fixed(etas_list_df[i, ]$V1), str_c(unlist(etas_list_df[i, ]$etas_names), collapse = " "))
              }
            }

            table_column_names <- str_replace_all(tab_def, "\\s+=\\s+", "=") %>%
              str_split("\\s+") %>%
              unlist() %>%
              .[!str_detect(., sprintf("\\$|(\\b(%s)\\b)", str_c(nm_tab_args, collapse = "|")))] %>%
              str_replace_all("\\(|\\)", "")

            mf <- str_match(tab_def, tab_format_pattern)

            t_df <- tibble(
              file = file,
              firstonly = str_detect(m[, 1], "FIRSTONLY"),
              noheader = str_detect(m[, 1], "NOHEADER"),
              oneheader = str_detect(m[, 1], "ONEHEADER"),
              noappend = str_detect(m[, 1], "NOAPPEND"),
              notitle = str_detect(m[, 1], "NOTITLE"),
              format = mf[, 2],
              columns = list(table_column_names), # str_c(tab_col_names %>% str_replace_all("[^\\w\\(\\):]", ""), collapse = "|"),
              task = x$task
            )

            est_sim_recs %>% select(n_task, task) %>% inner_join(t_df, by = c("n_task" = "task"))
          } else {
            NULL
          }
        })

      if (nrow(tab_matches) > 0) {
        tab_matches <- tab_matches %>%
          fill(format)

        table_filenames <- tab_matches$file
        dup_tables <- table_filenames[duplicated(table_filenames)] %>% unique()

        if (length(dup_tables)) {
          warning(simpleWarning(sprintf("Duplicated $TABLE statements: %s.", str_c(dup_tables, collapse = ", "))))

          tab_matches <- tab_matches %>% group_by(file) %>% slice(dplyr::n()) %>% ungroup()
          #        tab_matches <- tab_matches %>% slice(which(!duplicated(table_filenames)))
        }
      }
    }
  }

  if (!is.null(parameters_definitions)) {
    parameters_definitions <- parameters_definitions %>%
      arrange(type, n) %>%
      distinct()
  }


  # MSFI file
  msfi_file <- NULL
  if (length(records$MSFI) > 0) {
    if (verbose) print("Parsing $MSFI...")

    msfi_lines <- lines[records$MSFI$start:records$MSFI$end]

    msfi_pattern <- "\\s[\"']?([^\\s\"']+)"

    msfi_file <- msfi_match <- str_match(msfi_lines, msfi_pattern)[, 2]
  }

  cs_data <- list(
    code = content_lines,
    # cleaned_code = lines,
    records = grouped_records_df %>% select(name, n, source_start, source_end) %>% rename(start = source_start, end = source_end),
    problem = pb_text,
    dataset_file = data_filename,
    ignore = list(
      C = identical(toupper(ignore_txt), "C"),
      `@` = identical(ignore_txt, "@"),
      `#` = identical(ignore_txt, "#"),
      txt = ignore_txt,
      data = ignore_df
    ),
    subroutine = list(
      ADVAN = advan,
      FILES = sub_extra_files
    ),
    model_compartments = compartments,
    subpopulations = n_subpop,
    input = input_matches,
    parameters = list(
      definition = parameters_definitions,
      initialization = initial_values,
      blocks = blocks
    ),
    mu_referencing = mu_definitions,
    estimations = est_matches,
    covariance = covariance,
    simulations = sim_matches,
    tables = tab_matches,
    extra_files = unique(c(include_files, sub_extra_files, msfi_file))
  )


  class(cs_data) <- "nonmem_control_stream"

  invisible(cs_data)

  # not_null_cs_data <- cs_data %>% sapply(function(x) !is.null(x)) %>% cs_data[.]
  # not_null_cs_data <- Filter(length, cs_data)
  #
  # not_null_cs_data
}
