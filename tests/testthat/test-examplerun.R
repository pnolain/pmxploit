context("test-examplerun")

test_that("Load EXAMPLERUN", {
  # run <- EXAMPLERUN
  expect_error({
    run <- load_nm_run("../../data-raw/TMDD_Djebli_al")
  },
  NA)
})

test_that("EXAMPLERUN object check", {

  run <- load_nm_run("../../data-raw/TMDD_Djebli_al")

  expect_false(is.null(run))
  expect_equal(length(run$estimations), 1)

  expect_false(run$control_stream$ignore$C)
  expect_true(run$control_stream$ignore$`@`)
  expect_false(run$control_stream$ignore$`#`)
  expect_equal(run$control_stream$ignore$txt, "@")

  expect_equal(run$control_stream$dataset_file, "dataset.csv")

  expect_equal(run$control_stream$parameters$initialization,
               structure(list(THETA = structure(list(n = 1:13,
                                                     id = c("THETA1",  "THETA2", "THETA3", "THETA4", "THETA5", "THETA6", "THETA7", "THETA8",
                                                            "THETA9", "THETA10", "THETA11", "THETA12", "THETA13"),
                                                     name = c("TVCL",  "TKON", "TKIN", "TKDE", "TVQ", "TVV1", "TVV2", "TVKA", "TVF",
                                                              "TVEP", "TVEA", "TVLAG", "COV1"),
                                                     value = c(0.2, 559, 0.25, 2, 1, 4, 2.612, 0.5, 0.75, 0.3, 0.05, 0.05, 1),
                                                     lower_bound = c(0, 559, 0, 0, 0, 2, 2.612, 0, 0.55, 0, 0, 0, NA),
                                                     upper_bound = c(1e+06,  559, 1e+06, 1e+06, 1e+06, 1e+06, 2.612, 1e+06, 1, 1e+06, 1e+06, 100, NA),
                                                     fixed = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
                                                row.names = c(NA, -13L),
                                                class = c("tbl_df", "tbl", "data.frame"),
                                                .Names = c("n", "id", "name", "value", "lower_bound", "upper_bound", "fixed")),
                              OMEGA = structure(list(n = 1:9,
                                                     eta1 = c("ETCL", "EKON", "EKIN", "EKDE", "ETQ", "ETV1", "ETV2", "ETKA", "ETF"),
                                                     eta2 = c("ETCL", "EKON", "EKIN", "EKDE", "ETQ", "ETV1", "ETV2", "ETKA", "ETF"),
                                                     value = c(0.5, 0, 0.5, 0.5, 0.5, 0.5, 0, 0.5, 0.5),
                                                     in_block = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                                                     fixed = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
                                                class = c("tbl_df", "tbl", "data.frame"),
                                                row.names = c(NA, -9L),
                                                .Names = c("n", "eta1", "eta2", "value", "in_block", "fixed")),
                              SIGMA = structure(list(n = 1L, epsilon1 = "EPS1", epsilon2 = "EPS1", value = 1, in_block = FALSE, fixed = TRUE),
                                                class = c("tbl_df", "tbl", "data.frame"),
                                                row.names = c(NA, -1L),
                                                .Names = c("n", "epsilon1", "epsilon2", "value", "in_block", "fixed"))),
                         .Names = c("THETA", "OMEGA", "SIGMA")))

  expect_equal(run$estimations$`1: focei`$omega_matrix,
               structure(c(0.279195982109823, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0580908084573451, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0.170534475475969, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0691065862986685,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0990369682744904, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.464462143306044,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0.289551101707683), .Dim = c(9L, 9L
                           ),
                         .Dimnames = list(c("ETCL", "EKON", "EKIN", "EKDE", "ETQ",
                                            "ETV1", "ETV2", "ETKA", "ETF"),
                                          c("ETCL", "EKON", "EKIN", "EKDE",
                                            "ETQ", "ETV1", "ETV2", "ETKA", "ETF"))))
})
