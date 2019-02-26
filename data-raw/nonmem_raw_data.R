nonmem_txtmsgs <- read.csv("data-raw/nonmem_txtmsgs.csv", stringsAsFactors = FALSE)

#
nm_data_reserved_names <- c("ID", "L2", "DV", "MDV")
predpp_reserved_names <- c("TIME", "EVID", "AMT", "RATE", "SS", "II", "ADDL", "CMT", "PCMT", "CALL", "CONT")
nmtran_data_reserved_names <- c("DATE", "DAT1", "DAT2", "DAT3", "L1")

#
nm_reserved_names <- c(
  nm_data_reserved_names, predpp_reserved_names, nmtran_data_reserved_names,
  "PRED", "RES", "WRES"
)

# Predictions names
nm_predictions <- c(
  "PRED", "PREDI",
  "IPRED", "IPREDI", "IPRD",
  "CPRED", "CPREDI",
  "CIPRED", "CIPREDI",
  "NPRED", "NIPRED",
  "EPRED", "EIPRED"
)
# Residuals names
nm_residuals <- c(
  "CWRES", "CWRESI",
  "NPDE", "NPD",
  "WRES", "WRESI",
  "IWRES", "IWRESI", "IWRS",
  "RES", "RESI",
  "CRES", "CRESI",
  "IRES", "IRESI", "IRS",
  "CIRES", "CIWRES",
  "CIRESI", "CIWRESI",
  "NRES", "NWRES",
  "NIRES", "NIWRES",
  "ERES", "EWRES", "ECWRES",
  "EIRES", "EIWRES"
)

# NONMEM run object example
EXAMPLERUN <- load_nm_run("data-raw/TMDD_Djebli_al") %>%
  recode_categorical_covariates(
    covariates_levels =
      list(
        DISST = list("Healthy volunteer" = 0, "Patient" = 1),
        ISC = list("IV administration" = 0, "SC administration" = 1),
        SEX = list("Male" = 0, "Female" = 1),
        STATIN = list("Without statin" = 0, "With statin" = 1)
      )
  ) %>%
  rename_covariates(covariates = c(
    "Disease status" = "DISST",
    "Administration route" = "ISC",
    "Gender" = "SEX",
    "Statin co-administration" = "STATIN",
    "Study" = "STUD",
    "Age (y)" = "AGE",
    "BMI (kg/m²)" = "BMI",
    "Baseline LDLC (mg/dL)" = "BSLDLC",
    "Creatinine clearance (mL/min)" = "CLCR",
    "Baseline free PCSK9 (nM)" = "FBSPCSK",
    "Glomerular filtration rate (mL/min)" = "GFR",
    "Baseline total PCSK9 (nM)" = "TBSPCSK",
    "Weight (kg)" = "WT"
  ))

EXAMPLERUN$info$path <- "~/pmxploit/example.tar.gz"

usethis::use_data(nm_reserved_names, nonmem_txtmsgs, nm_predictions, nm_residuals, EXAMPLERUN, internal = TRUE, overwrite = TRUE)
