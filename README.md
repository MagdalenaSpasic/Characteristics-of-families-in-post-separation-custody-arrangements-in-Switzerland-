# Characteristics-of-families-in-post-separation-custody-arrangements-in-Switzerland-
R code for the paper "Characteristics of families in post-separation custody arrangements in Switzerland"
# ========================================================================
# Paper: Characteristics of families in post-separation custody arrangements
#        in Switzerland
# Author: Magdalena Spasic
# 20 October 2026 | UTF-8
# ========================================================================
# PURPOSE
# Create the 23 study variables from the designated survey export and the four
# required auxiliary files; produce descriptive statistics, bivariate and
# adjusted multinomial average marginal effects (AMEs), multiple-imputation
# estimates, and Equal SPC minus Unequal SPC Wald comparisons.
#
# SETUP AND EXECUTION
# The following five input files are required:
#    - FamyCH_W1_EP_without_calendar.sav
#    - Niveaux_geographiques.xlsx
#    - FAMYCH24A_Date_questionnaire.csv
#    - Verification_OP_matching.xlsx
#    - Education_coded.xlsx
#    All five files are needed to run the script. Their expected contents are
#    described under REQUIRED INPUTS below. 
# Open this file in RStudio and set data_dir and output_dir below. Paths are
#    relative to your working directory unless you supply absolute paths.
# Run the entire file for all analyses. The imputation stage creates 50
#    imputations with 20 iterations and can take substantial time.
# For the descriptive table only, select and run from the first line through
#    ft_descriptive at the END OF DESCRIPTIVE-ONLY EXECUTION marker. There is no
#    enclosing analysis switch. Display ft_descriptive (or ft) in RStudio.
#    Continue sequentially for bivariate, adjusted CC, and imputed analyses.
#
# REQUIRED INPUTS (original column names and French response labels)
# - FamyCH_W1_EP_without_calendar.sav: designated wave-1 survey export. 
#   It must contain Intact, IntactNotLivingdTogether and Widow. These
#   supplied family-status flags are inputs; this script does not derive them.
# - Niveaux_geographiques.xlsx: sheet 1; OFS_NOCOMM and Canton.
# - FAMYCH24A_Date_questionnaire.csv: HOUSEHOLD_ID and
#   DataCollection_FinishTime_Exported, in month/day/year hour:minute:second.
# - Verification_OP_matching.xlsx: sheet "To recode" (surrounding spaces allowed);
#   HOUSEHOLD_ID and OtherParent_PartnerIndex_Coded (1-7 or missing).
# - Education_coded.xlsx: sheet 1; HOUSEHOLD_ID and EDU01_R_Autre_coded.
#
# DIAGNOSTICS
# Run through END OF CODING DIAGNOSTICS for coding checks without fitting models.
# Inspect coding_diagnostics, employment_above_100, calendar_incomplete_details,
# country_diagnostics, adversity_diagnostics, duration_diagnostics,
# incoherence_diagnostics, birth_month_special_codes, age_sentinel_diagnostics,
# intact_previous_separations, infidelity_response_diagnostics and
# op_birth_year_diagnostics. Counts are computed from the supplied data.
# Unknown age/date/adversity/country/calendar rules flagged by these diagnostics
# are retained pending explicit substantive decisions; they are not silently fixed.
# ========================================================================

# Configuration -----------------------------------------------------------
data_dir <- path.expand("~/OneDrive - Université de Lausanne/Data")
output_dir <- file.path(data_dir, "Results")
# Use absolute paths internally; do not change the session working directory.
if (!dir.exists(data_dir)) stop("Set data_dir to the folder containing the five input files.")
data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

required_packages <- c("dplyr", "lubridate", "readxl", "readr", "haven",
                       "forcats", "psych", "tidyr", "purrr", "nnet", "gtsummary", "flextable",
                       "officer", "mice", "tibble", "marginaleffects", "stringr", "stringi")
missing_packages <- required_packages[!vapply(required_packages,
                                              requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install the missing packages before running this script:\ninstall.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}
invisible(lapply(required_packages, function(package) {
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}))
options(contrasts = c("contr.treatment", "contr.poly"))
set.seed(2026)
package_versions <- tibble::tibble(
  Package = required_packages,
  Version = vapply(required_packages, function(x) as.character(utils::packageVersion(x)), character(1))
)
print(package_versions, n = Inf)
writeLines(capture.output(print(package_versions, n = Inf)),
           file.path(output_dir, "Package_versions.txt"))

# This script uses original numeric codes for ID, roster sex and calendar fields,
# and SPSS labels for textual responses. Never use as.numeric(factor) for percentages.
if (!dir.exists(data_dir)) stop("Set data_dir to the folder containing the source files.")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
required_files <- c("FamyCH_W1_EP_without_calendar.sav", "Niveaux_geographiques.xlsx",
                    "FAMYCH24A_Date_questionnaire.csv", "Verification_OP_matching.xlsx",
                    "Education_coded.xlsx")
absent_files <- required_files[!file.exists(file.path(data_dir, required_files))]
if (length(absent_files)) stop("Required source files are missing: ", paste(absent_files, collapse = ", "))
FamyCH_w1 <- haven::read_sav(file.path(data_dir, "FamyCH_W1_EP_without_calendar.sav"), user_na = TRUE)
FamyCH_raw <- FamyCH_w1 # Unmodified input retained for coding diagnostics.
if (anyDuplicated(names(FamyCH_w1))) stop("Duplicate source column names.")
if (anyNA(FamyCH_w1$HOUSEHOLD_ID) || anyDuplicated(FamyCH_w1$HOUSEHOLD_ID)) stop("Invalid household key.")
# Family-status flags must be supplied in the designated export.
# Do not infer them from current marital status.
if (!all(c("Intact", "IntactNotLivingdTogether", "Widow") %in% names(FamyCH_w1))) {
  stop("Missing source family flags: Intact, IntactNotLivingdTogether or Widow. Supply their original coding.")
}
for (nm in c("Intact", "IntactNotLivingdTogether", "Widow")) {
  FamyCH_w1[[nm]] <- as.numeric(haven::zap_labels(haven::zap_missing(FamyCH_w1[[nm]])))
}
source_rows <- nrow(FamyCH_w1)
source_ids <- as.character(FamyCH_w1$HOUSEHOLD_ID)
check_lookup <- function(x, key) {
  if (anyNA(x[[key]]) || anyDuplicated(x[[key]])) stop("Lookup requires unique non-missing keys: ", key)
}

# ---------------------- QUESTIONNAIRE COMPLETION ----------------------
questionnaire_dates <- readr::read_csv(file.path(data_dir, "FAMYCH24A_Date_questionnaire.csv"),
                                       col_types = cols(HOUSEHOLD_ID = col_character(), DataCollection_FinishTime_Exported = col_character()),
                                       show_col_types = FALSE) %>%
  transmute(.household_join_id = HOUSEHOLD_ID, DataCollection_FinishTime_Exported) %>% distinct()
check_lookup(questionnaire_dates, ".household_join_id")
FamyCH_w1 <- FamyCH_w1 %>% select(-any_of("DataCollection_FinishTime_Exported")) %>%
  mutate(.household_join_id = as.character(HOUSEHOLD_ID)) %>%
  left_join(questionnaire_dates, by = ".household_join_id", na_matches = "never") %>%
  select(-.household_join_id)
# Observed CSV format: 11/20/2024 9:33:32, i.e. month/day/year.
FamyCH_w1$Interview_DateTime <- mdy_hms(FamyCH_w1$DataCollection_FinishTime_Exported, tz = "UTC")
if (any(is.na(FamyCH_w1$Interview_DateTime))) stop("Missing or unparseable questionnaire timestamps.")
FamyCH_w1$Interview_Date <- as.Date(FamyCH_w1$Interview_DateTime)
FamyCH_w1$Interview_Year <- year(FamyCH_w1$Interview_DateTime)

# ---------------------- ORIGINAL MANUAL CORRECTIONS -------------------
# Apply the study-specific corrections to roster fields before calculating ages.
set_source_value <- function(data, column, household, value) {
  if (!column %in% names(data)) stop("Missing manual-correction source: ", column)
  x <- data[[column]]
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (is.factor(x)) x <- as.character(x)
  i <- which(as.character(data$HOUSEHOLD_ID) == household)
  if (is.numeric(x)) value <- as.numeric(value)
  x[i] <- value
  data[[column]] <- x
  data
}
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne3_MoisNaissance", "1033830", "1")
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne3_MoisNaissance", "1151190", "1")
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne4_MoisNaissance", "1064540", "1")
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne5_MoisNaissance", "1064540", "1")
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne5_MoisNaissance", "1035830", "1")
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne1_AnneeNaissance", "1146780", "1992")
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne2_AnneeNaissance", "1146780", "2019")
FamyCH_w1 <- set_source_value(FamyCH_w1, "Personne2_AnneeNaissance", "1094890", "2011")
FamyCH_w1 <- set_source_value(FamyCH_w1, "ChildRef_Id", "1033830", "103383003")
FamyCH_w1 <- set_source_value(FamyCH_w1, "ChildRef_Id", "1016760", "101676002")
FamyCH_w1 <- set_source_value(FamyCH_w1, "ChildRef_Id", "1064540", "106454003")
FamyCH_w1 <- set_source_value(FamyCH_w1, "ChildRef_Id", "1151190", "115119003")
FamyCH_w1 <- set_source_value(FamyCH_w1, "ChildRef_Id", "1159350", "115935004")
FamyCH_w1 <- set_source_value(FamyCH_w1, "PersonId_3", "1151190", "115119003")
FamyCH_w1 <- set_source_value(FamyCH_w1, "PersonId_3", "1033830", "103383003")

# Retain RespondentParent_Id from the designated survey export.
# Original missing birth months -1/-2 become January, before roster ages are calculated.
# Roster ages use day 15 consistently with parental birth dates; unknown ages are NA.
for (j in 1:12) {
  nm <- paste0("Personne", j, "_MoisNaissance")
  x <- haven::zap_labels(FamyCH_w1[[nm]])
  if (is.factor(x)) x <- as.character(x)
  x[which(as.character(x) %in% c("-1", "-2"))] <- if (is.numeric(x)) 1 else "1"
  FamyCH_w1[[nm]] <- x
  yy <- suppressWarnings(as.numeric(as.character(haven::zap_labels(FamyCH_w1[[paste0("Personne", j, "_AnneeNaissance")]]))))
  mm <- suppressWarnings(as.numeric(as.character(x)))
  dd <- make_date(yy, mm, 15)
  aa <- floor(time_length(interval(dd, FamyCH_w1$Interview_Date), "years"))
  aa[is.na(aa) | aa < 0 | aa > 120] <- NA_real_
  FamyCH_w1[[paste0("Personne", j, "_age")]] <- aa
}
# ---------------------------- GEOGRAPHY -------------------------------
municipality_corrections <- c(
  "1048850"="297", "1021210"="1065", "1065270"="2097", "1110020"="2239",
  "1099930"="3360", "1158220"="4063", "1043760"="6513", "1136230"="6513",
  "1124320"="6513", "1139250"="6513", "1046080"="6513", "1059290"="6513",
  "1133560"="6513", "1042810"="6513", "1079980"="6513")
# Apply the original correction to OFS_NOCOMM itself, not just a temporary join key.
for (hh in names(municipality_corrections)) {
  FamyCH_w1 <- set_source_value(FamyCH_w1, "OFS_NOCOMM", hh, municipality_corrections[[hh]])
}
geo <- readxl::read_excel(file.path(data_dir, "Niveaux_geographiques.xlsx"), sheet = 1) %>%
  transmute(.municipality_key = trimws(as.character(OFS_NOCOMM)), Canton = as.character(Canton)) %>%
  filter(!is.na(.municipality_key)) %>% distinct()
check_lookup(geo, ".municipality_key")
FamyCH_w1 <- FamyCH_w1 %>% select(-any_of("Canton")) %>%
  mutate(.municipality_key = as.character(OFS_NOCOMM)) %>%
  left_join(geo, by = ".municipality_key", na_matches = "never") %>% select(-.municipality_key)
stopifnot(nrow(FamyCH_w1) == source_rows)


# Record the exact input files used in this run.
writeLines(capture.output(tools::md5sum(file.path(data_dir, required_files))),
           file.path(output_dir, "Input_checksums.txt"))

# ------------------------------------------------------------
# Shared data-coding helpers
# ------------------------------------------------------------
lookup_household_person <- function(data, target_id, field) {
  require_columns(data, c(target_id, paste0("PersonId_", 1:12),
                          paste0("Personne", 1:12, "_", field)))
  result <- rep(NA_character_, nrow(data))
  for (j in 12:1) {
    id <- as.character(haven::zap_labels(data[[paste0("PersonId_", j)]]))
    target <- as.character(haven::zap_labels(data[[target_id]]))
    value <- data[[paste0("Personne", j, "_", field)]]
    if (inherits(value, "haven_labelled")) value <- haven::zap_labels(value)
    matched <- which(!is.na(id) & !is.na(target) &
                       !id %in% c("", "0", "-1", "-2") & id == target)
    result[matched] <- as.character(value[matched])
  }
  result
}

recode_sep_month <- function(x) {
  x <- stringr::str_to_lower(trimws(as.character(x)))
  months_fr <- c("janvier", "février", "mars", "avril", "mai", "juin",
                 "juillet", "août", "septembre", "octobre", "novembre", "décembre")
  result <- suppressWarnings(as.numeric(x))
  matched <- match(x, months_fr)
  result[!is.na(matched)] <- matched[!is.na(matched)]
  # Assign approximate months to seasonal separation responses.
  season_map <- c("début d'année" = 2, "printemps" = 4,
                  "été" = 7, "automne/fin d'année" = 10,
                  "91" = 2, "92" = 4, "93" = 7, "94" = 10)
  season <- unname(season_map[x])
  result[!is.na(season)] <- season[!is.na(season)]
  result[!is.na(result) & !(result %in% 1:12)] <- NA_real_
  result
}

# Confirm that the import above produced a survey dataset.
if (!exists("FamyCH_w1") || !is.data.frame(FamyCH_w1)) {
  stop("The survey import did not produce a data frame.")
}
# Dates, parent indices and birth-year variables are created below.

# ------------------------------------------------------------
# Shared helpers for birth dates and household roster matching
# ------------------------------------------------------------
numeric_birth_value <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(haven::zap_missing(x))
  }
  suppressWarnings(as.numeric(as.character(x)))
}

month_birth_value <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(haven::zap_missing(x))
  }
  text <- stringr::str_to_lower(trimws(as.character(x)))
  month_names <- c("janvier", "février", "mars", "avril", "mai", "juin",
                   "juillet", "août", "septembre", "octobre", "novembre", "décembre")
  result <- suppressWarnings(as.numeric(text))
  matched <- match(text, month_names)
  result[!is.na(matched)] <- matched[!is.na(matched)]
  result[!is.na(result) & !(result %in% 1:12)] <- NA_real_
  result
}

labelled_text_value <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(haven::zap_missing(x))
  }
  as.character(x)
}

require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop("Missing source variables: ", paste(missing, collapse = ", "))
}

# Select one roster/history column per row without mixed-type rowwise operations.
select_indexed_value <- function(data, index, columns, convert) {
  require_columns(data, columns)
  result <- rep(NA_real_, nrow(data))
  for (j in seq_along(columns)) {
    selected <- which(!is.na(index) & index == j)
    result[selected] <- convert(data[[columns[j]]][selected])
  }
  result
}

validate_birth_year <- function(x, interview_year) {
  result <- numeric_birth_value(x)
  valid <- !is.na(result) & result == floor(result) &
    result >= 1900 & !is.na(interview_year) & result <= interview_year
  result[!valid] <- NA_real_
  result
}

# ------------------------------------------------------------
# Harmonize SPSS categorical responses used in text-based recodings
# ------------------------------------------------------------
source_response_text <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(haven::zap_missing(x))
  }
  as.character(x)
}
text_response_columns <- unique(c(
  "OP07_O", "CPARTREL01_P", "CPART17_R", "ORIG01_R", "Canton", "LAWLA01_R",
  "DIV01_R_Y", "EDU01date_R_Y",
  paste0("PART16_Ra_", 1:7, "_Annee"),
  paste0("PART04_Ra_", 1:7, "_Annee"),
  paste0("PART16_Ra_", 1:7, "_Mois"),
  paste0("PRIORDIV05_R_", 1:4),
  paste0("PART05_Ra_", 1:7),
  paste0("PART15_Ra_", 1:7),
  sprintf("PSYCONFLO%02d_R_2", 1:9),
  paste0("CP07_R_", 1:4),
  grep("^OP12_O", names(FamyCH_w1), value = TRUE)
))
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(across(any_of(text_response_columns), source_response_text))

# CODING
# ------------------------------------------------------------

# ------------------------------------------------------------
# Creation of variables
# ------------------------------------------------------------
# Index other parent - integrating the correct variable indicating OP's position in the partnership history 
# The excel file "Verification_OP_matching" is needed 
sheets <- readxl::excel_sheets(file.path(data_dir, "Verification_OP_matching.xlsx"))
selected_sheet <- sheets[trimws(sheets) == "To recode"]
if (length(selected_sheet) != 1L) stop("Cannot identify the To recode sheet.")
Verification_OP_matching <- readxl::read_excel(
  file.path(data_dir, "Verification_OP_matching.xlsx"), sheet = selected_sheet
) %>% transmute(.household_join_id = as.character(HOUSEHOLD_ID),
                OtherParent_PartnerIndex_Coded = numeric_birth_value(OtherParent_PartnerIndex_Coded)) %>% distinct()
check_lookup(Verification_OP_matching, ".household_join_id")
idx <- Verification_OP_matching$OtherParent_PartnerIndex_Coded
if (any(!is.na(idx) & !(idx %in% 1:7))) stop("Other-parent indices must be 1 to 7 or NA.")
FamyCH_w1 <- FamyCH_w1 %>%
  select(-any_of("OtherParent_PartnerIndex_Coded")) %>%
  mutate(.household_join_id = as.character(HOUSEHOLD_ID)) %>%
  left_join(Verification_OP_matching, by = ".household_join_id", na_matches = "never") %>%
  select(-.household_join_id) %>%
  mutate(OtherParent_PartnerIndex_Coded = if_else(Intact == 1, NA_real_, OtherParent_PartnerIndex_Coded))

# ------------------------------------------------------------
# Gender of the respondent
FamyCH_w1$Respondent_Parent_Gender_chiffres <- lookup_household_person(FamyCH_w1, "RespondentParent_Id", "Sexe")

FamyCH_w1$Respondent_Parent_Gender <- factor(
  dplyr::case_when(
    FamyCH_w1$Respondent_Parent_Gender_chiffres == "1" ~ "Man",
    FamyCH_w1$Respondent_Parent_Gender_chiffres == "2" ~ "Woman",
    TRUE ~ NA_character_
  ),
  levels = c("Man", "Woman")
)

# ------------------------------------------------------------
# Convert SPSS codes to numeric values
as_code <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(haven::zap_missing(x))
  }
  
  suppressWarnings(as.numeric(as.character(x)))
}

# Recode gender from numeric codes or text labels
recode_parent_gender <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(haven::zap_missing(x))
  }
  
  x <- as.character(x)
  
  dplyr::case_when(
    x %in% c("1", "Homme", "Man", "Male") ~ "Man",
    x %in% c("2", "Femme", "Woman", "Female") ~ "Woman",
    TRUE ~ NA_character_
  )
}

gender_history_vars <- paste0(
  "PART03_07_08_Ra_", 1:7, "_Sexe"
)

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    across(all_of(gender_history_vars), as_code),
    
    OP_Gender_Separated_code = case_when(
      OtherParent_PartnerIndex_Coded == 1 ~ PART03_07_08_Ra_1_Sexe,
      OtherParent_PartnerIndex_Coded == 2 ~ PART03_07_08_Ra_2_Sexe,
      OtherParent_PartnerIndex_Coded == 3 ~ PART03_07_08_Ra_3_Sexe,
      OtherParent_PartnerIndex_Coded == 4 ~ PART03_07_08_Ra_4_Sexe,
      OtherParent_PartnerIndex_Coded == 5 ~ PART03_07_08_Ra_5_Sexe,
      OtherParent_PartnerIndex_Coded == 6 ~ PART03_07_08_Ra_6_Sexe,
      OtherParent_PartnerIndex_Coded == 7 ~ PART03_07_08_Ra_7_Sexe,
      TRUE ~ NA_real_
    ),
    
    OP_Gender_Separated = if_else(
      is.na(OP_Gender_Separated_code),
      recode_parent_gender(OP01_O),
      recode_parent_gender(OP_Gender_Separated_code)
    ),
    
    OP_Gender_Separated = factor(
      OP_Gender_Separated,
      levels = c("Woman", "Man")
    )
  )

# ------------------------------------------------------------
# Identify the respondent and the other parent in the household roster
# ------------------------------------------------------------
require_columns(FamyCH_w1, c("RespondentParent_Id", paste0("PersonId_", 1:12)))
respondent_matches <- sapply(1:12, function(j) {
  id <- FamyCH_w1[[paste0("PersonId_", j)]]
  !is.na(id) & !is.na(FamyCH_w1$RespondentParent_Id) &
    as.character(id) == as.character(FamyCH_w1$RespondentParent_Id)
})
FamyCH_w1$Respondent_PersonIndex <- NA_integer_
unique_respondent <- which(rowSums(respondent_matches) == 1)
if (length(unique_respondent)) {
  FamyCH_w1$Respondent_PersonIndex[unique_respondent] <-
    max.col(respondent_matches[unique_respondent, , drop = FALSE], ties.method = "first")
}
FamyCH_w1$Respondent_Intact_Index <- FamyCH_w1$Respondent_PersonIndex

relation_columns <- vapply(1:12, function(j) {
  candidates <- grep(paste0("^Personne", j, "_RelationAvecEnfant"),
                     names(FamyCH_w1), value = TRUE)
  if (length(candidates) != 1) {
    stop("Cannot identify the child-relationship column for household member ", j)
  }
  candidates
}, character(1))
parent_matches <- sapply(relation_columns, function(column) {
  labelled_text_value(FamyCH_w1[[column]]) %in%
    c("Parent biologique", "Parent adoptif")
})
for (j in 1:12) {
  selected <- which(!is.na(FamyCH_w1$Respondent_PersonIndex) &
                      FamyCH_w1$Respondent_PersonIndex == j)
  parent_matches[selected, j] <- FALSE
}
FamyCH_w1$OP_Intact_Index <- NA_integer_
unique_other_parent <- which(rowSums(parent_matches) == 1)
if (length(unique_other_parent)) {
  FamyCH_w1$OP_Intact_Index[unique_other_parent] <-
    max.col(parent_matches[unique_other_parent, , drop = FALSE], ties.method = "first")
}

# Other-parent gender in intact households
FamyCH_w1$OP_Gender_NS_code <- NA_real_
for (j in 1:12) {
  selected <- which(!is.na(FamyCH_w1$OP_Intact_Index) & FamyCH_w1$OP_Intact_Index == j)
  FamyCH_w1$OP_Gender_NS_code[selected] <- suppressWarnings(
    as.numeric(as.character(FamyCH_w1[[paste0("Personne", j, "_Sexe")]][selected]))
  )
}
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(OP_Gender_NS = factor(case_when(
    OP_Gender_NS_code == 1 ~ "Man",
    OP_Gender_NS_code == 2 ~ "Woman",
    TRUE ~ NA_character_
  ), levels = c("Woman", "Man")))

FamyCH_w1$OP_Gender_NS[FamyCH_w1$HOUSEHOLD_ID %in% c(1097710, 1037300)] <- NA
FamyCH_w1$OP_Gender_Separated[FamyCH_w1$HOUSEHOLD_ID %in% c(1097710, 1037300)] <- NA

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    OP_Gender_final = case_when(
      Intact %in% 1 | IntactNotLivingdTogether %in% 1 ~
        coalesce(as.character(OP_Gender_NS), as.character(OP_Gender_Separated)),
      TRUE ~ as.character(OP_Gender_Separated)
    ),
    
    OP_Gender_final = factor(
      OP_Gender_final,
      levels = c("Woman", "Man")
    )
  )

# ------------------------------------------------------------
# Final parental genders and analytical eligibility
# Keep the observed final genders, including in same-gender couples. SameSex is
# calculated from those same final variables and is never overridden by ID.
# SameSex == 1 is excluded from analyses; unknown SameSex alone does not exclude.
manual_exclusion_ids <- character(0) # No blanket exclusion by household ID.
FamyCH_w1 <- FamyCH_w1 %>% mutate(
  SameSex = case_when(
    is.na(Respondent_Parent_Gender) | is.na(OP_Gender_final) ~ NA_real_,
    as.character(Respondent_Parent_Gender) == as.character(OP_Gender_final) ~ 1,
    TRUE ~ 0
  ),
  Gender_Role_Mapping_Eligible = SameSex %in% 0,
  Mother_Is_Respondent = Respondent_Parent_Gender %in% "Woman" & !(OP_Gender_final %in% "Woman"),
  Mother_Is_OP = OP_Gender_final %in% "Woman" & !(Respondent_Parent_Gender %in% "Woman"),
  Father_Is_Respondent = Respondent_Parent_Gender %in% "Man" & !(OP_Gender_final %in% "Man"),
  Father_Is_OP = OP_Gender_final %in% "Man" & !(Respondent_Parent_Gender %in% "Man"),
  Manual_Exclusion = as.character(HOUSEHOLD_ID) %in% manual_exclusion_ids,
  Analysis_Eligible = !(SameSex %in% 1) # Exclude known same-gender couples only.
)

# ------------------------------------------------------------
# Convert SPSS-labelled values to text labels
as_text_label <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(haven::zap_missing(x))
  }
  
  as.character(x)
}

# Date of the separation: identify how the relationship ended
relationship_end_vars <- paste0("PART15_Ra_", 1:7)

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    across(
      all_of(relationship_end_vars),
      as_text_label
    ),
    
    TypeOfEndWithOP_raw = case_when(
      OtherParent_PartnerIndex_Coded == 1 ~ PART15_Ra_1,
      OtherParent_PartnerIndex_Coded == 2 ~ PART15_Ra_2,
      OtherParent_PartnerIndex_Coded == 3 ~ PART15_Ra_3,
      OtherParent_PartnerIndex_Coded == 4 ~ PART15_Ra_4,
      OtherParent_PartnerIndex_Coded == 5 ~ PART15_Ra_5,
      OtherParent_PartnerIndex_Coded == 6 ~ PART15_Ra_6,
      OtherParent_PartnerIndex_Coded == 7 ~ PART15_Ra_7,
      TRUE ~ NA_character_
    ),
    
    TypeOfEndWithOP = case_when(
      TypeOfEndWithOP_raw == "Nous nous sommes séparé·e·s" ~ "Sep",
      TypeOfEndWithOP_raw == "Le/la partenaire est décédé·e" ~ "Death",
      TRUE ~ NA_character_
    ),
    
    TypeOfEndWithOP = factor(
      TypeOfEndWithOP,
      levels = c("Sep", "Death")
    )
  )

# Harmonize the types
for (i in 1:7) {
  FamyCH_w1[[paste0("PART16_Ra_", i, "_Mois")]] <-
    as.character(FamyCH_w1[[paste0("PART16_Ra_", i, "_Mois")]])
  
  FamyCH_w1[[paste0("PART16_Ra_", i, "_Annee")]] <-
    as.character(FamyCH_w1[[paste0("PART16_Ra_", i, "_Annee")]])
}

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    SepWithOP_Month_raw = case_when(
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 1 ~ PART16_Ra_1_Mois,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 2 ~ PART16_Ra_2_Mois,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 3 ~ PART16_Ra_3_Mois,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 4 ~ PART16_Ra_4_Mois,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 5 ~ PART16_Ra_5_Mois,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 6 ~ PART16_Ra_6_Mois,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 7 ~ PART16_Ra_7_Mois,
      TRUE ~ NA_character_
    ),
    
    SepWithOP_Year_raw = case_when(
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 1 ~ PART16_Ra_1_Annee,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 2 ~ PART16_Ra_2_Annee,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 3 ~ PART16_Ra_3_Annee,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 4 ~ PART16_Ra_4_Annee,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 5 ~ PART16_Ra_5_Annee,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 6 ~ PART16_Ra_6_Annee,
      TypeOfEndWithOP == "Sep" & OtherParent_PartnerIndex_Coded == 7 ~ PART16_Ra_7_Annee,
      TRUE ~ NA_character_
    ),
    
    SepWithOP_Month = recode_sep_month(SepWithOP_Month_raw),
    
    SepWithOP_Year = case_when(
      SepWithOP_Year_raw %in% c("Je ne sais pas", "Ne s'applique pas") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(SepWithOP_Year_raw))
    ),
    
    Date_SeparationWithOP = case_when(
      !is.na(SepWithOP_Year) & !is.na(SepWithOP_Month) ~
        make_date(
          year = SepWithOP_Year,
          month = SepWithOP_Month,
          day = 15
        ),
      TRUE ~ as.Date(NA)
    )
  )

# Harmonized separation sources. A year-only response is approximated by July 1.
# Available history month is retained when DIV supplies only the missing year.
FamyCH_w1 <- FamyCH_w1 %>% mutate(
  Separation_Year_DIV = suppressWarnings(as.numeric(source_response_text(DIV01_R_Y))),
  Separation_Year_DIV = if_else(
    !is.na(Separation_Year_DIV) & Separation_Year_DIV == floor(Separation_Year_DIV) &
      between(Separation_Year_DIV, 1900, Interview_Year), Separation_Year_DIV, NA_real_),
  Separation_History_Year_Invalid = !is.na(SepWithOP_Year) &
    (SepWithOP_Year != floor(SepWithOP_Year) | SepWithOP_Year < 1900 | SepWithOP_Year > Interview_Year),
  Separation_DIV_Year_Raw = suppressWarnings(as.numeric(source_response_text(DIV01_R_Y))),
  Separation_DIV_Year_Invalid = !is.na(Separation_DIV_Year_Raw) &
    !Separation_DIV_Year_Raw %in% c(-1, -2) &
    (Separation_DIV_Year_Raw != floor(Separation_DIV_Year_Raw) |
       Separation_DIV_Year_Raw < 1900 | Separation_DIV_Year_Raw > Interview_Year),
  SepWithOP_Year = replace(SepWithOP_Year, Separation_History_Year_Invalid, NA_real_),
  YearSepOP_combined = coalesce(SepWithOP_Year, Separation_Year_DIV),
  Separation_Date_Source = case_when(
    !is.na(SepWithOP_Year) & !is.na(SepWithOP_Month) ~ "Partnership history year and month",
    !is.na(YearSepOP_combined) & !is.na(SepWithOP_Month) ~ "DIV year and history month",
    !is.na(YearSepOP_combined) ~ "Year only: July 1 approximation",
    TRUE ~ NA_character_),
  Separation_Date_Approximate = !is.na(YearSepOP_combined) & is.na(SepWithOP_Month),
  Date_SeparationWithOP = make_date(YearSepOP_combined,
                                    coalesce(SepWithOP_Month, 7), if_else(is.na(SepWithOP_Month), 1, 15)),
  # A within-month midpoint or year-only July 1 can lie after the interview.
  # Bound that approximation at the interview only if its known month/year
  # is compatible with a separation already having occurred.
  Separation_Approximation_Bounded = !is.na(Date_SeparationWithOP) &
    Date_SeparationWithOP > Interview_Date & YearSepOP_combined == Interview_Year &
    (is.na(SepWithOP_Month) | SepWithOP_Month == month(Interview_Date)),
  Date_SeparationWithOP = if_else(Separation_Approximation_Bounded,
                                  Interview_Date, Date_SeparationWithOP),
  Separation_Date_Future = !is.na(Date_SeparationWithOP) & Date_SeparationWithOP > Interview_Date,
  Protect_Separation_Date = Separation_Date_Future |
    (is.na(YearSepOP_combined) & (Separation_History_Year_Invalid | Separation_DIV_Year_Invalid)),
  Separation_Date_Approximate = Separation_Date_Approximate | Separation_Approximation_Bounded,
  Date_SeparationWithOP = replace(Date_SeparationWithOP,
                                  Protect_Separation_Date | Widow %in% 1, as.Date(NA)),
  YearSepOP_combined = replace(YearSepOP_combined,
                               Protect_Separation_Date | Widow %in% 1, NA_real_)
)

# ------------------------------------------------------------
# Number of years since separation
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Interview_DateTime = as.POSIXct(Interview_Date),
    Interview_Year = lubridate::year(Interview_Date),
    
    YearsSinceSep_final = case_when(
      !is.na(Date_SeparationWithOP) & !is.na(Interview_Date) ~
        as.numeric(Interview_Date - Date_SeparationWithOP) / 365.2425,
      TRUE ~ NA_real_
    )
  )
# ------------------------------------------------------------
# Start year of the relationship with the other parent
# ------------------------------------------------------------

# Harmonize the types of relationship-start years
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    across(
      all_of(paste0("PART04_Ra_", 1:7, "_Annee")),
      ~ as.character(haven::zap_labels(haven::zap_missing(.x)))
    ),
    
    # Select the relationship identified by the manually coded index
    Year_Start_OP_final = case_when(
      OtherParent_PartnerIndex_Coded == 1 ~ PART04_Ra_1_Annee,
      OtherParent_PartnerIndex_Coded == 2 ~ PART04_Ra_2_Annee,
      OtherParent_PartnerIndex_Coded == 3 ~ PART04_Ra_3_Annee,
      OtherParent_PartnerIndex_Coded == 4 ~ PART04_Ra_4_Annee,
      OtherParent_PartnerIndex_Coded == 5 ~ PART04_Ra_5_Annee,
      OtherParent_PartnerIndex_Coded == 6 ~ PART04_Ra_6_Annee,
      OtherParent_PartnerIndex_Coded == 7 ~ PART04_Ra_7_Annee,
      TRUE ~ NA_character_
    ),
    
    Year_Start_OP_final = suppressWarnings(
      as.numeric(Year_Start_OP_final)
    ),
    
    # Preserve the original valid-year range
    Year_Start_OP_final = if_else(
      between(Year_Start_OP_final, 1900, 2025),
      Year_Start_OP_final,
      NA_real_
    )
  )

# ------------------------------------------------------------
# Previous-separation count must precede start-year cleaning.
# Subsequent cleaning of the start year must not change this count.
# Previous separations
# Respondent
FamyCH_w1 <- FamyCH_w1 %>%
  rowwise() %>%
  mutate(
    PreviousSD_Resp_Separated = sum(c(
      ifelse(!is.na(PART15_Ra_1) && PART15_Ra_1 == "Nous nous sommes séparé·e·s" &&
               !is.na(PART16_Ra_1_Annee) &&
               suppressWarnings(as.numeric(PART16_Ra_1_Annee)) < Year_Start_OP_final &&
               OtherParent_PartnerIndex_Coded != 1, 1, 0),
      
      ifelse(!is.na(PART15_Ra_2) && PART15_Ra_2 == "Nous nous sommes séparé·e·s" &&
               !is.na(PART16_Ra_2_Annee) &&
               suppressWarnings(as.numeric(PART16_Ra_2_Annee)) < Year_Start_OP_final &&
               OtherParent_PartnerIndex_Coded != 2, 1, 0),
      
      ifelse(!is.na(PART15_Ra_3) && PART15_Ra_3 == "Nous nous sommes séparé·e·s" &&
               !is.na(PART16_Ra_3_Annee) &&
               suppressWarnings(as.numeric(PART16_Ra_3_Annee)) < Year_Start_OP_final &&
               OtherParent_PartnerIndex_Coded != 3, 1, 0),
      
      ifelse(!is.na(PART15_Ra_4) && PART15_Ra_4 == "Nous nous sommes séparé·e·s" &&
               !is.na(PART16_Ra_4_Annee) &&
               suppressWarnings(as.numeric(PART16_Ra_4_Annee)) < Year_Start_OP_final &&
               OtherParent_PartnerIndex_Coded != 4, 1, 0),
      
      ifelse(!is.na(PART15_Ra_5) && PART15_Ra_5 == "Nous nous sommes séparé·e·s" &&
               !is.na(PART16_Ra_5_Annee) &&
               suppressWarnings(as.numeric(PART16_Ra_5_Annee)) < Year_Start_OP_final &&
               OtherParent_PartnerIndex_Coded != 5, 1, 0),
      
      ifelse(!is.na(PART15_Ra_6) && PART15_Ra_6 == "Nous nous sommes séparé·e·s" &&
               !is.na(PART16_Ra_6_Annee) &&
               suppressWarnings(as.numeric(PART16_Ra_6_Annee)) < Year_Start_OP_final &&
               OtherParent_PartnerIndex_Coded != 6, 1, 0),
      
      ifelse(!is.na(PART15_Ra_7) && PART15_Ra_7 == "Nous nous sommes séparé·e·s" &&
               !is.na(PART16_Ra_7_Annee) &&
               suppressWarnings(as.numeric(PART16_Ra_7_Annee)) < Year_Start_OP_final &&
               OtherParent_PartnerIndex_Coded != 7, 1, 0)
    ))
  ) %>%
  ungroup()


# ------------------------------------------------------------
# Duration relationship respondent and other parent
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Duration_Relationship_OP_final = YearSepOP_combined - Year_Start_OP_final
  )

# ------------------------------------------------------------
# Child age at separation ------------------------------------------------
# Approximate birth date from the integer age at interview, then calculate age at separation.
FamyCH_w1$Reference_child_age <- as.numeric(lookup_household_person(FamyCH_w1, "ChildRef_Id", "age"))
FamyCH_w1$Reference_child_age[FamyCH_w1$Reference_child_age == 99999] <- NA_real_
FamyCH_w1$Reference_child_birth_date_approx <-
  FamyCH_w1$Interview_Date %m-% years(FamyCH_w1$Reference_child_age)

FamyCH_w1$Age_Child_AtSep <- FamyCH_w1$Reference_child_age -
  as.numeric(FamyCH_w1$Interview_Date - FamyCH_w1$Date_SeparationWithOP) / 365.2425
FamyCH_w1$Age_Child_AtSep <- ifelse(FamyCH_w1$Age_Child_AtSep < 0, 0, FamyCH_w1$Age_Child_AtSep)

# ------------------------------------------------------------
# Gender of the child
FamyCH_w1$Reference_child_Gender_chiffres <- lookup_household_person(FamyCH_w1, "ChildRef_Id", "Sexe")

FamyCH_w1$Reference_child_Gender <- factor(
  dplyr::case_when(
    FamyCH_w1$Reference_child_Gender_chiffres == "1" ~ "Boy",
    FamyCH_w1$Reference_child_Gender_chiffres == "2" ~ "Girl",
    TRUE ~ NA_character_
  ),
  levels = c("Boy", "Girl")
)

# ------------------------------------------------------------
# Respondent's birth date and age at interview
# ------------------------------------------------------------
FamyCH_w1$Resp_BirthYear <- select_indexed_value(
  FamyCH_w1, FamyCH_w1$Respondent_PersonIndex,
  paste0("Personne", 1:12, "_AnneeNaissance"), numeric_birth_value
)
FamyCH_w1$Resp_BirthMonth <- select_indexed_value(
  FamyCH_w1, FamyCH_w1$Respondent_PersonIndex,
  paste0("Personne", 1:12, "_MoisNaissance"), month_birth_value
)
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Resp_Invalid_BirthYear = !is.na(Resp_BirthYear) & !Resp_BirthYear %in% c(-1, -2) &
      (Resp_BirthYear < 1900 | Resp_BirthYear > Interview_Year | Resp_BirthYear != floor(Resp_BirthYear)),
    Resp_BirthYear = validate_birth_year(Resp_BirthYear, Interview_Year),
    Resp_BirthDate = lubridate::make_date(Resp_BirthYear, Resp_BirthMonth, 15)
  )

# ------------------------------------------------------------
# Other parent's birth date and age: partnership history
# ------------------------------------------------------------
FamyCH_w1$OP_birth_year_sep <- select_indexed_value(
  FamyCH_w1, FamyCH_w1$OtherParent_PartnerIndex_Coded,
  paste0("PART07_Ra_", 1:7, "_AnneeNaissance"), numeric_birth_value
)
FamyCH_w1$OP_birth_month_sep <- select_indexed_value(
  FamyCH_w1, FamyCH_w1$OtherParent_PartnerIndex_Coded,
  paste0("PART07_Ra_", 1:7, "_MoisNaissance"), month_birth_value
)

# Preserve the four manual overrides using OP02.
ids_OP02 <- c(1116420, 1115240, 1122780, 1030640)
selected <- which(as.character(FamyCH_w1$HOUSEHOLD_ID) %in% as.character(ids_OP02))
if (length(selected)) {
  require_columns(FamyCH_w1, c("OP02_O_M", "OP02_O_Y"))
  FamyCH_w1$OP_birth_month_sep[selected] <- month_birth_value(FamyCH_w1$OP02_O_M[selected])
  FamyCH_w1$OP_birth_year_sep[selected] <- numeric_birth_value(FamyCH_w1$OP02_O_Y[selected])
}
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    OP_Invalid_BirthYear_History = !is.na(OP_birth_year_sep) & !OP_birth_year_sep %in% c(-1, -2) &
      (OP_birth_year_sep < 1900 | OP_birth_year_sep > Interview_Year | OP_birth_year_sep != floor(OP_birth_year_sep)),
    OP_birth_year_sep = validate_birth_year(OP_birth_year_sep, Interview_Year),
    OP_birth_date_sep = lubridate::make_date(OP_birth_year_sep, OP_birth_month_sep, 15),
    OP_age_sep = floor(lubridate::time_length(
      lubridate::interval(OP_birth_date_sep, Interview_Date), "years"
    ))
  )

# ------------------------------------------------------------
# Other parent's birth date and age: household roster
# ------------------------------------------------------------
FamyCH_w1$OP_Intact_Birth_Year <- select_indexed_value(
  FamyCH_w1, FamyCH_w1$OP_Intact_Index,
  paste0("Personne", 1:12, "_AnneeNaissance"), numeric_birth_value
)
FamyCH_w1$OP_Intact_Birth_Month <- select_indexed_value(
  FamyCH_w1, FamyCH_w1$OP_Intact_Index,
  paste0("Personne", 1:12, "_MoisNaissance"), month_birth_value
)
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    OP_Invalid_BirthYear_Roster = !is.na(OP_Intact_Birth_Year) & !OP_Intact_Birth_Year %in% c(-1, -2) &
      (OP_Intact_Birth_Year < 1900 | OP_Intact_Birth_Year > Interview_Year | OP_Intact_Birth_Year != floor(OP_Intact_Birth_Year)),
    OP_Intact_Birth_Year = validate_birth_year(OP_Intact_Birth_Year, Interview_Year),
    OP_Intact_Birth_Date = lubridate::make_date(OP_Intact_Birth_Year, OP_Intact_Birth_Month, 15),
    OP_age_intact = floor(lubridate::time_length(
      lubridate::interval(OP_Intact_Birth_Date, Interview_Date), "years"
    ))
  )

# ------------------------------------------------------------
# Second parent in intact families not living together
# ------------------------------------------------------------
require_columns(FamyCH_w1, c("SecondParentMoisNaissance", "SecondParentAnneeNaissance",
                             "IntactNotLivingdTogether"))
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    SecondParentMoisNaissance = month_birth_value(SecondParentMoisNaissance),
    SecondParentYear_Raw = numeric_birth_value(SecondParentAnneeNaissance),
    OP_Invalid_BirthYear_SecondParent = !is.na(SecondParentYear_Raw) & !SecondParentYear_Raw %in% c(-1, -2) &
      (SecondParentYear_Raw < 1900 | SecondParentYear_Raw > Interview_Year | SecondParentYear_Raw != floor(SecondParentYear_Raw)),
    SecondParentAnneeNaissance = validate_birth_year(SecondParentAnneeNaissance, Interview_Year),
    SecondParent_Birth_Date = lubridate::make_date(
      SecondParentAnneeNaissance, SecondParentMoisNaissance, 15
    ),
    SecondParent_age = floor(lubridate::time_length(
      lubridate::interval(SecondParent_Birth_Date, Interview_Date), "years"
    )),
    OP_age = case_when(
      IntactNotLivingdTogether %in% 1 ~ SecondParent_age,
      Intact %in% 1 ~ OP_age_intact,
      TRUE ~ OP_age_sep
    )
  )

# ------------------------------------------------------------
# Other-parent birth year: use the corrected partnership index consistently.
# The selected partnership source (including the four documented OP02 source
# corrections above) is shared with OP_birth_year_sep. Use the roster for intact
# co-resident families and SecondParent fields for intact non-co-resident families.
# OP02 is a fallback for separated families only when no corrected index exists;
# a missing year within an identified partnership is not silently replaced.
FamyCH_w1 <- FamyCH_w1 %>% mutate(
  OP02_BirthYear_Raw = numeric_birth_value(OP02_O_Y),
  OP02_Invalid_BirthYear = !is.na(OP02_BirthYear_Raw) & !OP02_BirthYear_Raw %in% c(-1, -2) &
    (OP02_BirthYear_Raw < 1900 | OP02_BirthYear_Raw > Interview_Year |
       OP02_BirthYear_Raw != floor(OP02_BirthYear_Raw)),
  OP02_BirthMonth = if ("OP02_O_M" %in% names(FamyCH_w1)) month_birth_value(OP02_O_M) else NA_real_,
  OP02_BirthDate = make_date(validate_birth_year(OP02_BirthYear_Raw, Interview_Year), OP02_BirthMonth, 15),
  OP_birth_year = case_when(
    IntactNotLivingdTogether %in% 1 ~ SecondParentAnneeNaissance,
    Intact %in% 1 ~ OP_Intact_Birth_Year,
    !is.na(OtherParent_PartnerIndex_Coded) ~ OP_birth_year_sep,
    TRUE ~ validate_birth_year(OP02_O_Y, Interview_Year)
  )
)

# One cleaning decision per parental source; apply it to current and separation ages.
# A known invalid value is distinguished from an ordinary missing source for MI.
FamyCH_w1 <- FamyCH_w1 %>% mutate(
  Resp_age = floor(time_length(interval(Resp_BirthDate, Interview_Date), "years")),
  Resp_age = coalesce(Resp_age, Interview_Year - Resp_BirthYear),
  OP_age_from_year = Interview_Year - OP_birth_year,
  OP_BirthDate_Selected = case_when(
    IntactNotLivingdTogether %in% 1 ~ SecondParent_Birth_Date,
    Intact %in% 1 ~ OP_Intact_Birth_Date,
    !is.na(OtherParent_PartnerIndex_Coded) ~ OP_birth_date_sep,
    TRUE ~ OP02_BirthDate),
  OP_age_unclean = coalesce(floor(time_length(
    interval(OP_BirthDate_Selected, Interview_Date), "years")), OP_age_from_year),
  Resp_Invalid_Age_Source = Resp_Invalid_BirthYear | (!is.na(Resp_age) & (Resp_age < 0 | Resp_age > 120)) |
    (!is.na(Resp_age) & !is.na(Reference_child_age) & Resp_age - Reference_child_age <= 18),
  OP_Invalid_BirthYear_Selected = case_when(
    IntactNotLivingdTogether %in% 1 ~ OP_Invalid_BirthYear_SecondParent,
    Intact %in% 1 ~ OP_Invalid_BirthYear_Roster,
    !is.na(OtherParent_PartnerIndex_Coded) ~ OP_Invalid_BirthYear_History,
    TRUE ~ OP02_Invalid_BirthYear),
  OP_Invalid_Age_Source = OP_Invalid_BirthYear_Selected | (!is.na(OP_age_unclean) & (OP_age_unclean < 0 | OP_age_unclean > 120)) |
    (!is.na(OP_age_unclean) & !is.na(Reference_child_age) & OP_age_unclean - Reference_child_age <= 18) |
    as.character(HOUSEHOLD_ID) %in% c("1097710", "1037300", "1137990"),
  Resp_BirthYear = replace(Resp_BirthYear, Resp_Invalid_Age_Source, NA_real_),
  Resp_BirthMonth = replace(Resp_BirthMonth, Resp_Invalid_Age_Source, NA_real_),
  Resp_BirthDate = replace(Resp_BirthDate, Resp_Invalid_Age_Source, as.Date(NA)),
  Resp_age = replace(Resp_age, Resp_Invalid_Age_Source, NA_real_),
  OP_birth_year = replace(OP_birth_year, OP_Invalid_Age_Source, NA_real_),
  OP_age = replace(OP_age_unclean, OP_Invalid_Age_Source, NA_real_),
  OP_birth_year_sep = replace(OP_birth_year_sep, OP_Invalid_Age_Source, NA_real_),
  OP_birth_month_sep = replace(OP_birth_month_sep, OP_Invalid_Age_Source, NA_real_),
  OP_birth_date_sep = replace(OP_birth_date_sep, OP_Invalid_Age_Source, as.Date(NA)),
  OP_age_sep = replace(coalesce(OP_age_sep, Interview_Year - OP_birth_year_sep),
                       OP_Invalid_Age_Source, NA_real_),
  OP_Intact_Birth_Year = replace(OP_Intact_Birth_Year, OP_Invalid_Age_Source, NA_real_),
  OP_Intact_Birth_Month = replace(OP_Intact_Birth_Month, OP_Invalid_Age_Source, NA_real_),
  OP_Intact_Birth_Date = replace(OP_Intact_Birth_Date, OP_Invalid_Age_Source, as.Date(NA)),
  OP_age_intact = replace(coalesce(OP_age_intact, Interview_Year - OP_Intact_Birth_Year),
                          OP_Invalid_Age_Source, NA_real_),
  SecondParentAnneeNaissance = replace(SecondParentAnneeNaissance, OP_Invalid_Age_Source, NA_real_),
  SecondParentMoisNaissance = replace(SecondParentMoisNaissance, OP_Invalid_Age_Source, NA_real_),
  SecondParent_Birth_Date = replace(SecondParent_Birth_Date, OP_Invalid_Age_Source, as.Date(NA)),
  SecondParent_age = replace(coalesce(SecondParent_age, Interview_Year - SecondParentAnneeNaissance),
                             OP_Invalid_Age_Source, NA_real_)
)

# Apply each parental source exclusion to that identified parent's roster age too.
for (j in 1:12) {
  idx_r <- FamyCH_w1$Respondent_PersonIndex %in% j
  idx_op <- FamyCH_w1$OP_Intact_Index %in% j
  bad <- (idx_r & FamyCH_w1$Resp_Invalid_Age_Source) |
    (idx_op & FamyCH_w1$OP_Invalid_Age_Source)
  FamyCH_w1[[paste0("Personne", j, "_age")]][bad] <- NA_real_
}

# Age of the parents at the time of the separation
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    # Convert years to numeric values
    YearSep_num = suppressWarnings(
      as.numeric(as.character(zap_labels(YearSepOP_combined)))
    ),
    Resp_BirthYear_num = suppressWarnings(
      as.numeric(as.character(zap_labels(Resp_BirthYear)))
    ),
    OP_BirthYear_num = suppressWarnings(
      as.numeric(as.character(zap_labels(OP_birth_year)))
    ),
    # Respondent and other-parent ages at separation
    Resp_Age_AtSep = YearSep_num - Resp_BirthYear_num,
    OP_Age_AtSep   = YearSep_num - OP_BirthYear_num,
    # Mother's age at separation
    Age_Mother_AtSep = case_when(
      Mother_Is_Respondent ~ Resp_Age_AtSep,
      Mother_Is_OP          ~ OP_Age_AtSep,
      TRUE ~ NA_real_
    ),
    # Father's age at separation
    Age_Father_AtSep = case_when(
      Father_Is_Respondent ~ Resp_Age_AtSep,
      Father_Is_OP          ~ OP_Age_AtSep,
      TRUE ~ NA_real_
    )
  )

# Snapshot the condition to avoid across() updating the variable used by later columns.
duration_before_cleaning <- FamyCH_w1 %>% select(
  HOUSEHOLD_ID, Duration_Relationship_OP_final, YearSepOP_combined,
  Year_Start_OP_final, Age_Child_AtSep, Age_Mother_AtSep, Age_Father_AtSep)
# Negative durations alone are missing; unknown durations do not erase other data.
.duration_negative <- !is.na(FamyCH_w1$Duration_Relationship_OP_final) &
  FamyCH_w1$Duration_Relationship_OP_final < 0
FamyCH_w1$Duration_Relationship_OP_final[.duration_negative] <- NA_real_

# Cleaning
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Diff_Mother_Child = Age_Mother_AtSep - Age_Child_AtSep,
    Diff_Father_Child = Age_Father_AtSep - Age_Child_AtSep
  )

# Objective checks from corrected values, before replacing invalid ages.
# A relationship beginning before the parent's birth is temporally impossible.
# The old <16-at-start rule and household-specific age combinations are retained
# only as review flags; they do not automatically erase otherwise usable ages.
FamyCH_w1 <- FamyCH_w1 %>% mutate(
  Age_Mother_StartRel = Age_Mother_AtSep - Duration_Relationship_OP_final,
  Age_Father_StartRel = Age_Father_AtSep - Duration_Relationship_OP_final,
  History_Negative_Duration = .duration_negative,
  History_Mother_Under18_AtSep = !is.na(Age_Mother_AtSep) & Age_Mother_AtSep < 18,
  History_Father_Under18_AtSep = !is.na(Age_Father_AtSep) & Age_Father_AtSep < 18,
  History_Start_Before_Parent_Birth =
    (!is.na(Age_Mother_StartRel) & Age_Mother_StartRel < 0) |
    (!is.na(Age_Father_StartRel) & Age_Father_StartRel < 0),
  History_Young_AtStart_Review =
    (!is.na(Age_Mother_StartRel) & Age_Mother_StartRel < 16) |
    (!is.na(Age_Father_StartRel) & Age_Father_StartRel < 16),
  Incoherence_Hist_OP = case_when(
    History_Negative_Duration | History_Mother_Under18_AtSep |
      History_Father_Under18_AtSep | History_Start_Before_Parent_Birth |
      Resp_Invalid_Age_Source | OP_Invalid_Age_Source | Protect_Separation_Date ~ 1,
    is.na(Duration_Relationship_OP_final) & is.na(Age_Mother_AtSep) &
      is.na(Age_Father_AtSep) ~ NA_real_,
    TRUE ~ 0
  ),
  Age_Mother_StartRel = replace(Age_Mother_StartRel,
                                !is.na(Age_Mother_StartRel) & Age_Mother_StartRel < 0, NA_real_),
  Age_Father_StartRel = replace(Age_Father_StartRel,
                                !is.na(Age_Father_StartRel) & Age_Father_StartRel < 0, NA_real_)
)

# ------------------------------------------------------------
# Use the same minimum parental age in complete-case and imputed analyses.
parental_ages_before_minimum <- FamyCH_w1 %>%
  select(HOUSEHOLD_ID, Age_Mother_AtSep, Age_Father_AtSep)
FamyCH_w1 <- FamyCH_w1 %>% mutate(across(
  c(Age_Mother_AtSep, Age_Father_AtSep),
  ~ replace(.x, !is.na(.x) & .x < 18, NA_real_)))

# Protected analysis cells: ordinary missing values remain imputable, but cleaned
# invalid values and unassignable mother/father roles stay NA in CC and MI.
FamyCH_w1 <- FamyCH_w1 %>% mutate(
  Resp_Under18_AtSep = !is.na(Resp_Age_AtSep) & Resp_Age_AtSep < 18,
  OP_Under18_AtSep = !is.na(OP_Age_AtSep) & OP_Age_AtSep < 18,
  Resp_Age_AtSep = replace(Resp_Age_AtSep, Resp_Under18_AtSep, NA_real_),
  OP_Age_AtSep = replace(OP_Age_AtSep, OP_Under18_AtSep, NA_real_),
  Protect_Mother_Age =
    (Mother_Is_Respondent & Resp_Invalid_Age_Source) |
    (Mother_Is_OP & OP_Invalid_Age_Source) |
    History_Mother_Under18_AtSep | Protect_Separation_Date | !(Mother_Is_Respondent | Mother_Is_OP),
  Protect_Father_Age =
    (Father_Is_Respondent & Resp_Invalid_Age_Source) |
    (Father_Is_OP & OP_Invalid_Age_Source) |
    History_Father_Under18_AtSep | Protect_Separation_Date | !(Father_Is_Respondent | Father_Is_OP),
  Age_Mother_AtSep = replace(Age_Mother_AtSep, Protect_Mother_Age, NA_real_),
  Age_Father_AtSep = replace(Age_Father_AtSep, Protect_Father_Age, NA_real_)
)

# Education level respondent
education_as_text <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(haven::zap_missing(x))
  }
  
  trimws(as.character(x))
}

# Read the required manual coding of other education responses.
education_lookup <- readxl::read_excel(file.path(data_dir, "Education_coded.xlsx"), sheet = 1) %>%
  transmute(.household_join_id = as.character(HOUSEHOLD_ID),
            EDU01_R_Autre_coded = education_as_text(EDU01_R_Autre_coded)) %>% distinct()
check_lookup(education_lookup, ".household_join_id")
FamyCH_w1 <- FamyCH_w1 %>%
  select(-any_of("EDU01_R_Autre_coded")) %>%
  mutate(.household_join_id = as.character(HOUSEHOLD_ID)) %>%
  left_join(education_lookup, by = ".household_join_id", na_matches = "never") %>%
  select(-.household_join_id) %>%
  mutate(
    EDU01_R_text = education_as_text(EDU01_R),
    EDU01_R_Other = if_else(EDU01_R_text == "Autre (précisez)&#160;:",
                            EDU01_R_Autre_coded, EDU01_R_text),
    EDU01_R_Other = if_else(is.na(EDU01_R_Other) | EDU01_R_Other == "NA",
                            "Other/missing", EDU01_R_Other)
  )

FamyCH_w1$Education_Level <- FamyCH_w1$EDU01_R_Other %>%
  fct_recode(
    "0/ Less than primary / 1/ Primary education" = "Moins que l'école primaire",
    "0/ Less than primary / 1/ Primary education" = "École primaire ou premier cycle de l'enseignement de base",
    "2/ Lower secondary education" = "École secondaire inférieure, second cycle de l'enseignement de base jusqu'à la fin de l'école obligatoire",
    "3/ Upper secondary education" = "Éducation secondaire supérieure, préparation à l'enseignement supérieur <span class='brackets-text'>(avec diplôme de fin",
    "3/ Upper secondary education" = "Éducation secondaire supérieure, préparation pour une formation professionnelle ultérieure <span class='brackets-text'>(",
    "6/ Bachelor’s or equivalent level" = "Niveau licence ou équivalent",
    "7/ Master’s or equivalent level" = "Niveau master ou équivalent",
    "8/ Doctoral or equivalent level" = "Niveau doctorat ou équivalent",
    "4/ Post-secondary non-tertiary / 5/ Short-cycle tertiary" = "Éducation tertiaire de cycle court/premier cycle, éducation professionnelle <span class='brackets-text'>(Höhere Fachschu",
    "3/ Upper secondary education" = "Éducation secondaire supérieure, entrée sur le marché du travail <span class='brackets-text'>(formation professionnelle",
    "4/ Post-secondary non-tertiary / 5/ Short-cycle tertiary" = "Éducation postsecondaire non tertiaire <span class='brackets-text'>(préparation pour une institution d'enseignement supé",
    "4/ Post-secondary non-tertiary / 5/ Short-cycle tertiary" = "Éducation tertiaire de cycle court/premier cycle, éducation générale <span class='brackets-text'>(université, université"
  )
FamyCH_w1$Education_Level <- as.factor(as.character(FamyCH_w1$Education_Level))


FamyCH_w1$Education_Level_3 <- FamyCH_w1$Education_Level %>%
  fct_recode(
    "1/ Obligatory school" = "0/ Less than primary / 1/ Primary education",
    "1/ Obligatory school" = "2/ Lower secondary education",
    "2/ Post-obligatory education" = "3/ Upper secondary education",
    "2/ Post-obligatory education" = "4/ Post-secondary non-tertiary / 5/ Short-cycle tertiary",
    "3/ Tertiary education" = "6/ Bachelor’s or equivalent level",
    "3/ Tertiary education" = "7/ Master’s or equivalent level",
    "3/ Tertiary education" = "8/ Doctoral or equivalent level"
  )
FamyCH_w1$Education_Level_3 <- as.factor(as.character(FamyCH_w1$Education_Level_3))
FamyCH_w1$Education_Level_3[FamyCH_w1$Education_Level_3 == "Other/missing"] <- NA
FamyCH_w1$Education_Level_3 <- as.factor(as.character(FamyCH_w1$Education_Level_3))

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Education_Level_3 = fct_recode(
      Education_Level_3,
      "Compulsory school" = "1/ Obligatory school",
      "Post-compulsory education" = "2/ Post-obligatory education",
      "Tertiary education" = "3/ Tertiary education"
    ),
    Education_Level_3 = factor(
      Education_Level_3,
      levels = c(
        "Compulsory school",
        "Post-compulsory education",
        "Tertiary education"
      )
    )
  )

# ------------------------------------------------------------
# Mother's education at separation
# Compare numeric years; nonresponse labels and other nonnumeric responses
# become missing. Preserve the original survey response columns.
# Both years must be known. A qualification obtained after separation does not
# establish the mother's earlier education level, which remains missing.
parse_education_year <- function(x) {
  text <- trimws(source_response_text(x))
  text <- gsub("\u2019", "'", text, fixed = TRUE)
  text[text %in% c("Je ne sais pas", "Ne s'applique pas", "")] <- NA_character_
  suppressWarnings(as.numeric(text))
}

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Education_Completion_Year = parse_education_year(EDU01date_R_Y),
    Separation_Year_For_Education = parse_education_year(DIV01_R_Y),
    Education_AtSep_Mother = case_when(
      Mother_Is_Respondent &
        !is.na(Education_Completion_Year) &
        !is.na(Separation_Year_For_Education) &
        Education_Completion_Year <= Separation_Year_For_Education ~
        as.character(Education_Level_3),
      TRUE ~ NA_character_
    )
  )

FamyCH_w1$Education_AtSep_Mother <- factor(
  FamyCH_w1$Education_AtSep_Mother,
  levels = c(
    "Compulsory school",
    "Post-compulsory education",
    "Tertiary education"
  ),
  ordered = TRUE
)

# ------------------------------------------------------------
# Employment before separation: respondent and other parent

employment_as_text <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::as_factor(haven::zap_missing(x))
  }
  x <- trimws(as.character(x))
  gsub("\u2019", "'", x, fixed = TRUE)
}

employment_as_numeric <- function(x) {
  if (inherits(x, "haven_labelled")) {
    x <- haven::zap_labels(haven::zap_missing(x))
  }
  suppressWarnings(as.numeric(as.character(x)))
}

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    PRIORDIV01_R_text = employment_as_text(PRIORDIV01_R),
    PRIORDIV02_R_text = employment_as_text(PRIORDIV02_R),
    PRIORDIV03_R_text = employment_as_text(PRIORDIV03_R),
    PRIORDIV02_R_num = employment_as_numeric(PRIORDIV02_R),
    PRIORDIV04_R_num = employment_as_numeric(PRIORDIV04_R),
    
    EmploymentBeforeSDResp = case_when(
      PRIORDIV01_R_text %in% c("Ne s'applique pas", "Je ne sais pas") |
        PRIORDIV02_R_text %in% c("Ne s'applique pas", "Je ne sais pas") ~ NA_character_,
      PRIORDIV02_R_num >= 100 ~ "Full-time",
      PRIORDIV02_R_num > 0 & PRIORDIV02_R_num < 100 ~ "Part-time",
      PRIORDIV01_R_text %in% c(
        "Non, autre situation", "Non, j'étais au chômage",
        "Non, j'étais rentier (AVS, invalidité)", "Non, je n'avais pas de revenu"
      ) ~ "Not in employment",
      TRUE ~ NA_character_
    ),
    EmploymentBeforeSDResp_fct = factor(
      EmploymentBeforeSDResp,
      levels = c("Full-time", "Part-time", "Not in employment")
    ),
    
    EmploymentBeforeSD_OP = case_when(
      PRIORDIV03_R_text == "Ne s'applique pas" ~ "Not in employment",
      PRIORDIV03_R_text %in% c("Je ne sais pas", "Ne s'applique pas") ~ NA_character_,
      PRIORDIV03_R_text %in% c(
        "Non, il/elle était rentier (AVS, invalidité)",
        "Non, il/elle était au chômage", "Non, il/elle n'avait pas de revenu"
      ) ~ "Not in employment",
      PRIORDIV03_R_text == "Oui" & PRIORDIV04_R_num >= 100 ~ "Full-time",
      PRIORDIV03_R_text == "Oui" & PRIORDIV04_R_num > 0 &
        PRIORDIV04_R_num < 100 ~ "Part-time",
      TRUE ~ NA_character_
    ),
    EmploymentBeforeSD_OP = factor(
      EmploymentBeforeSD_OP,
      levels = c("Full-time", "Part-time", "Not in employment")
    )
  )

# Inspect source labels and derived employment categories.
table(FamyCH_w1$PRIORDIV03_R_text, useNA = "ifany")
table(FamyCH_w1$EmploymentBeforeSDResp, useNA = "ifany")
table(FamyCH_w1$EmploymentBeforeSD_OP, useNA = "ifany")

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    EmploymentBeforeSD_Mother = case_when(
      Mother_Is_Respondent ~ as.character(EmploymentBeforeSDResp),
      Mother_Is_OP ~ as.character(EmploymentBeforeSD_OP),
      TRUE ~ NA_character_
    ),
    
    EmploymentBeforeSD_Father = case_when(
      Father_Is_Respondent ~ as.character(EmploymentBeforeSDResp),
      Father_Is_OP ~ as.character(EmploymentBeforeSD_OP),
      TRUE ~ NA_character_
    ),
    
    EmploymentBeforeSD_Mother = factor(
      EmploymentBeforeSD_Mother,
      levels = c("Full-time", "Part-time", "Not in employment")
    ),
    EmploymentBeforeSD_Father = factor(
      EmploymentBeforeSD_Father,
      levels = c("Full-time", "Part-time", "Not in employment")
    )
  )

# ------------------------------------------------------------
# User-defined convention: no reported history means no prior separation
# for intact-family respondents, including when all seven history ends are NA.
# Create this dependency before combining intact and separated respondents.
cols_part15 <- paste0("PART15_Ra_", 1:7)
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(across(all_of(cols_part15), source_response_text)) %>%
  rowwise() %>%
  mutate(
    PreviousSep_Resp_Intact = case_when(
      Intact %in% 1 & any(c_across(all_of(cols_part15)) ==
                            "Nous nous sommes séparé·e·s", na.rm = TRUE) ~ "Yes",
      Intact %in% 1 ~ "No",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    PreviousSD_Resp_Final = case_when(
      PreviousSep_Resp_Intact == "Yes" |
        (!is.na(PreviousSD_Resp_Separated) & PreviousSD_Resp_Separated > 0) ~ "Yes",
      
      PreviousSep_Resp_Intact == "No" |
        (!is.na(PreviousSD_Resp_Separated) & PreviousSD_Resp_Separated == 0) ~ "No",
      
      TRUE ~ NA_character_
    ),
    PreviousSD_Resp_Final = factor(
      PreviousSD_Resp_Final,
      levels = c("No", "Yes")
    )
  )

# Other parent
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    PreviousSD_OP = case_when(
      OP07_O %in% c("Je ne sais pas", "Ne s'applique pas") ~ NA_character_,
      OP07_O == "Non" ~ "No",
      OP07_O == "Oui" ~ "Yes",
      TRUE ~ NA_character_
    ),
    PreviousSD_OP = factor(PreviousSD_OP, levels = c("No", "Yes"))
  )

# Intact
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    PreviousSep_Part_Intact = case_when(
      Intact == 1 & CPARTREL01_P == "Oui" ~ "Yes",
      Intact == 1 & CPARTREL01_P == "Non" ~ "No",
      Intact == 1 & is.na(CPARTREL01_P)   ~ "Je ne sais pas",
      TRUE ~ NA_character_
    )
  )

# For all
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    PreviousSD_OP_all = case_when(
      PreviousSep_Part_Intact == "Yes" |
        PreviousSD_OP == "Yes" ~ "Yes",
      
      PreviousSep_Part_Intact == "No" |
        PreviousSD_OP == "No" ~ "No",
      
      TRUE ~ NA_character_
    ),
    
    PreviousSD_OP_all = factor(
      PreviousSD_OP_all,
      levels = c("No", "Yes")
    )
  )

# Both
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    PreviousSD_Mother = case_when(
      Mother_Is_Respondent ~ as.character(PreviousSD_Resp_Final),
      Mother_Is_OP ~ as.character(PreviousSD_OP_all),
      TRUE ~ NA_character_
    ),
    
    PreviousSD_Father = case_when(
      Father_Is_Respondent ~ as.character(PreviousSD_Resp_Final),
      Father_Is_OP ~ as.character(PreviousSD_OP_all),
      TRUE ~ NA_character_
    ),
    
    PreviousSD_WM_final = case_when(
      PreviousSD_Mother == "Yes" & PreviousSD_Father == "Yes" ~ "Both",
      PreviousSD_Mother == "No"  & PreviousSD_Father == "No"  ~ "None",
      PreviousSD_Mother == "Yes" & PreviousSD_Father == "No"  ~ "Mother only",
      PreviousSD_Mother == "No"  & PreviousSD_Father == "Yes" ~ "Father only",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------
# Parental adversities before separation
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    serious_physical_illness = case_when(
      PRIORDIV05_R_1 == "Oui, pour moi" ~ 1,
      PRIORDIV05_R_1 == "Oui, pour Autre parent" ~ 2,
      PRIORDIV05_R_1 == "Oui, pour nous deux" ~ 3,
      PRIORDIV05_R_1 %in% c("Non", "Ne s'applique pas") ~ 0,
      PRIORDIV05_R_1 == "Je ne sais pas" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    psychological_problems = case_when(
      PRIORDIV05_R_2 == "Oui, pour moi" ~ 1,
      PRIORDIV05_R_2 == "Oui, pour Autre parent" ~ 2,
      PRIORDIV05_R_2 == "Oui, pour nous deux" ~ 3,
      PRIORDIV05_R_2 %in% c("Non", "Ne s'applique pas") ~ 0,
      PRIORDIV05_R_2 == "Je ne sais pas" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    violence_or_addictions = case_when(
      PRIORDIV05_R_3 == "Oui, pour moi" ~ 1,
      PRIORDIV05_R_3 == "Oui, pour Autre parent" ~ 2,
      PRIORDIV05_R_3 == "Oui, pour nous deux" ~ 3,
      PRIORDIV05_R_3 %in% c("Non", "Ne s'applique pas") ~ 0,
      PRIORDIV05_R_3 == "Je ne sais pas" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    police_contact = case_when(
      PRIORDIV05_R_4 == "Oui, pour moi" ~ 1,
      PRIORDIV05_R_4 == "Oui, pour Autre parent" ~ 2,
      PRIORDIV05_R_4 == "Oui, pour nous deux" ~ 3,
      PRIORDIV05_R_4 %in% c("Non", "Ne s'applique pas") ~ 0,
      PRIORDIV05_R_4 == "Je ne sais pas" ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Adversity_Respondent = rowSums(across(
      c(serious_physical_illness, psychological_problems, violence_or_addictions, police_contact),
      ~ . %in% c(1, 3))) > 0,
    Adversity_OtherParent = rowSums(across(
      c(serious_physical_illness, psychological_problems, violence_or_addictions, police_contact),
      ~ . %in% c(2, 3))) > 0,
    Adversity_N_Observed = rowSums(!is.na(across(
      c(serious_physical_illness, psychological_problems, violence_or_addictions, police_contact)))),
    parental_issues = case_when(
      Intact %in% 1 ~ NA_real_,
      Adversity_Respondent & Adversity_OtherParent ~ 3,
      Adversity_Respondent ~ 1,
      Adversity_OtherParent ~ 2,
      Adversity_N_Observed == 0 ~ NA_real_,
      TRUE ~ 0
    )
  )

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    parental_issues = factor(
      parental_issues,
      levels = c(0, 1, 2, 3),
      labels = c(
        "None",
        "Only respondent",
        "Only other parent",
        "Both parents"
      )
    )
  )

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    parental_issues_WM = case_when(
      Mother_Is_Respondent & parental_issues == "Only respondent" ~ "Only mother",
      Father_Is_Respondent   & parental_issues == "Only respondent" ~ "Only father",
      Father_Is_OP & parental_issues == "Only other parent" ~ "Only father",
      Mother_Is_OP & parental_issues == "Only other parent" ~ "Only mother",
      parental_issues %in% c("Both parents", "None") ~ as.character(parental_issues),
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------
# Type of union

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    TypeOfUnion_Intact = case_when(
      Intact == 1 & CPART17_R == "Oui" ~ "Married",
      Intact == 1 & CPART17_R == "Non" ~ "Cohabiting",
      TRUE ~ NA_character_
    )
  )

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Married_OtherParent_final = case_when(
      OtherParent_PartnerIndex_Coded == 1 ~ PART05_Ra_1,
      OtherParent_PartnerIndex_Coded == 2 ~ PART05_Ra_2,
      OtherParent_PartnerIndex_Coded == 3 ~ PART05_Ra_3,
      OtherParent_PartnerIndex_Coded == 4 ~ PART05_Ra_4,
      OtherParent_PartnerIndex_Coded == 5 ~ PART05_Ra_5,
      OtherParent_PartnerIndex_Coded == 6 ~ PART05_Ra_6,
      OtherParent_PartnerIndex_Coded == 7 ~ PART05_Ra_7,
      TRUE ~ NA_character_
    ),
    
    Married_OtherParent_final = case_when(
      Married_OtherParent_final == "Oui" ~ "Yes",
      Married_OtherParent_final == "Non" ~ "No",
      TRUE ~ NA_character_
    ),
    
    Married_OtherParent_final = factor(
      Married_OtherParent_final,
      levels = c("Yes", "No")
    ),
    
    Type_Union_final = case_when(
      Intact == 1 ~ as.character(TypeOfUnion_Intact),
      Married_OtherParent_final == "Yes" ~ "Married",
      Married_OtherParent_final == "No"  ~ "Cohabiting",
      TRUE ~ NA_character_
    ),
    
    Type_Union_final = factor(
      Type_Union_final,
      levels = c("Married", "Cohabiting")
    )
  )

# ------------------------------------------------------------
# Conflict before separation
# Variables

vars_psy <- c(
  "PSYCONFLO01_R_2",
  "PSYCONFLO02_R_2",
  "PSYCONFLO03_R_2",
  "PSYCONFLO04_R_2",
  "PSYCONFLO05_R_2",
  "PSYCONFLO06_R_2",
  "PSYCONFLO07_R_2",
  "PSYCONFLO08_R_2",
  "PSYCONFLO09_R_2"
)

# Coding of items

FamyCH_psy <- FamyCH_w1 %>%
  mutate(
    
    # ------------------------------------------------------------
    # PSYCONFLO01-02
    # ------------------------------------------------------------
    
    across(
      c(
        PSYCONFLO01_R_2,
        PSYCONFLO02_R_2
      ),
      ~ case_when(
        
        . %in% c(
          "Je ne sais pas",
          "Ne s'applique pas"
        ) ~ NA_real_,
        
        . == "Une fois par an ou moins" ~ 0,
        . == "Tous les 4 à 6 mois" ~ 1,
        . == "Tous les 2 à 3 mois" ~ 2,
        . == "Une ou deux fois par mois" ~ 3,
        . == "Une ou deux fois par semaine" ~ 4,
        . == "Tous les jours ou plus d'une fois par jour" ~ 5,
        
        TRUE ~ NA_real_
      )
    ),
    
    # ------------------------------------------------------------
    # PSYCONFLO03-09
    # ------------------------------------------------------------
    
    across(
      c(
        PSYCONFLO03_R_2,
        PSYCONFLO04_R_2,
        PSYCONFLO05_R_2,
        PSYCONFLO06_R_2,
        PSYCONFLO07_R_2,
        PSYCONFLO08_R_2,
        PSYCONFLO09_R_2
      ),
      ~ case_when(
        
        . %in% c(
          "Je ne sais pas",
          "Ne s'applique pas"
        ) ~ NA_real_,
        
        . == "Jamais" ~ 0,
        . == "Rarement" ~ 1,
        . == "Parfois" ~ 2,
        . == "Souvent" ~ 3,
        
        TRUE ~ NA_real_
      )
    )
  ) %>%
  select(
    all_of(vars_psy)
  )

# Check coding

summary(FamyCH_psy)

# Number of items per person

n_items_conflict <- rowSums(
  !is.na(FamyCH_psy)
)

table(n_items_conflict)

# Polychoric correlations

poly_corr <- psych::polychoric(
  FamyCH_psy
)$rho

round(
  poly_corr,
  2
)

# Parallel analysis

set.seed(2026)
grDevices::pdf(file.path(output_dir, "Conflict_parallel_analysis.pdf"))
tryCatch(
  psych::fa.parallel(poly_corr, n.obs = nrow(FamyCH_psy), fa = "fa"),
  finally = grDevices::dev.off()
)

# Exploratory factor analysis
# General factor

efa_conflict <- psych::fa(
  poly_corr,
  nfactors = 1,
  n.obs = nrow(FamyCH_psy),
  fm = "minres"
)

print(
  efa_conflict,
  sort = TRUE
)

# Alpha

alpha_conflict <- psych::alpha(
  FamyCH_psy
)

alpha_conflict$total$raw_alpha

# Final score
# Minimum 5 out of 9 items
# Loadings

loadings_conflict <- as.numeric(
  efa_conflict$loadings[,1]
)

names(loadings_conflict) <- rownames(
  efa_conflict$loadings
)

# Loading-weighted score using available items

Conflict_Before_Sep_Final <- apply(
  FamyCH_psy,
  1,
  function(x) {
    
    valid <- !is.na(x)
    
    # At least 5 items
    if(sum(valid) < 5){
      return(NA_real_)
    }
    
    sum(
      x[valid] *
        loadings_conflict[names(x)[valid]]
    ) /
      sum(
        loadings_conflict[names(x)[valid]]
      )})

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Conflict_Before_Sep_Final =
      .env$Conflict_Before_Sep_Final
  )

# ------------------------------------------------------------
# Division of childcare before separation
recode_implication <- function(x) {
  recode(x,
         "(Presque) complètement Autre parent" = -2,
         "Pour la plupart Autre parent"        = -1,
         "Environ 50/50"                       =  0,
         "Pour la plupart moi"                 =  1,
         "(Presque) complètement moi"          =  2,
         "Autre"                               = NA_real_,
         "Je ne sais pas"                      = NA_real_,
         "Ne s'applique pas"                   = NA_real_,
         .default = NA_real_
  )
}

# CP07
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    CP07_R_1_num = as.numeric(recode_implication(CP07_R_1)),
    CP07_R_2_num = as.numeric(recode_implication(CP07_R_2)),
    CP07_R_3_num = as.numeric(recode_implication(CP07_R_3)),
    CP07_R_4_num = as.numeric(recode_implication(CP07_R_4))
  )

# Mean score
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Implication_score = rowMeans(
      select(., CP07_R_1_num, CP07_R_2_num, CP07_R_3_num, CP07_R_4_num),
      na.rm = TRUE
    )
  )

# 3 categories
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Implication_Parents = case_when(
      is.na(Implication_score) ~ NA_character_,
      Implication_score > 0    ~ "Respondent more",
      Implication_score < 0    ~ "Other parent more",
      Implication_score == 0   ~ "50-50"
    )
  )

FamyCH_w1 <- FamyCH_w1 %>%
  rename(Implication_Parents_Past = Implication_Parents)

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Implication_Parents_Past_WM = case_when(
      Mother_Is_Respondent & Implication_Parents_Past == "Respondent more"   ~ "Mother more",
      Father_Is_Respondent   & Implication_Parents_Past == "Respondent more"   ~ "Father more",
      Father_Is_OP & Implication_Parents_Past == "Other parent more" ~ "Father more",
      Mother_Is_OP & Implication_Parents_Past == "Other parent more" ~ "Mother more",
      Implication_Parents_Past == "50-50"                                                 ~ "50-50",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------
# Reason separation
# Long format 
FamyCH_long <- FamyCH_w1 %>%
  pivot_longer(
    cols = all_of(sprintf("OP12_O%02d", 1:23)),
    names_to = "item",
    values_to = "reason"
  ) %>%
  filter(
    !is.na(reason),
    !reason %in% c(
      "Je ne sais pas",
      "Ne s'applique pas",
      "Autre raison"
    )
  )

# Categories
FamyCH_long <- FamyCH_long %>%
  mutate(reason_group = case_when(
    reason %in% c("Nous ne nous correspondions pas", "Nous nous étions éloigné·e·s",
                  "Manque d'attention, de compréhension et d'amour", "Manque de liberté",
                  "Orientation sexuelle") ~ "Incompatibility",
    
    reason %in% c("Jalousie", "Divergences d'opinion sur les habitudes de dépenses, les activités de loisirs",
                  "Trop occupé·e  par le travail", "Désaccord sur la répartition des tâches ménagères",
                  "Problèmes avec la famille, les connaissances ou les amis", "Problèmes financiers",
                  "Problèmes de communication dans la relation", "Problèmes sexuels") ~ "Conjugal_conflict",
    
    reason %in% c("Divergences d'opinion sur le désir d'avoir des enfants",
                  "Divergences d'opinion sur l'éducation des enfants",
                  "Difficultés à avoir des enfants") ~ "Parental_disagreement",
    
    reason %in% c("Problèmes de violence physique", "Conflits fréquents et / ou violence psychologique",
                  "Problèmes d'alcool ou autres addictions") ~ "Violence_Addictions",
    
    reason %in% c("Je suis entré·e dans une autre relation",
                  "Autre parent est entré·e dans une autre relation",
                  "J'ai trompé", "Autre parent m’a trompé avec quelqu'un d'autre",
                  "J'ai trompéAutre parent m’a trompé avec quelqu'un d'autre") ~ "Infidelity",
    
    TRUE ~ NA_character_
  ))

# Summary per person
reasons_by_person <- FamyCH_long %>%
  mutate(value = 1) %>%
  distinct(HOUSEHOLD_ID, reason_group, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = HOUSEHOLD_ID,
    names_from = reason_group,
    values_from = value,
    values_fill = 0
  )

# Technical safeguard: absent categories must be zero for households with a recorded reason.
for (nm in c("Violence_Addictions", "Infidelity", "Parental_disagreement", "Conjugal_conflict", "Incompatibility")) {
  if (!nm %in% names(reasons_by_person)) reasons_by_person[[nm]] <- 0
}
# Yes/No versions
reasons_by_person <- reasons_by_person %>%
  mutate(across(
    -HOUSEHOLD_ID,
    ~ ifelse(.x == 1, "Yes", "No"),
    .names = "{.col}_YN"
  ))

# Adding the variable
FamyCH_w1 <- FamyCH_w1 %>%
  left_join(reasons_by_person, by = "HOUSEHOLD_ID")

# Additional study-specific variable exclusions are applied in the final coding block.
reason_vars <- c("Violence_Addictions_YN", "Infidelity_YN", "Parental_disagreement_YN",
                 "Conjugal_conflict_YN", "Incompatibility_YN")
FamyCH_w1 <- FamyCH_w1 %>% mutate(across(all_of(reason_vars),
                                         ~ replace(.x, is.na(Intact) | Intact %in% 1, NA)))

# ------------------------------------------------------------
# Type of agreement
# Code NAs
FamyCH_w1$LAWLA01_R[
  FamyCH_w1$LAWLA01_R %in% c("Autre", "Je ne sais pas", "Ne s'applique pas")
] <- NA

# Translation
arr_fr <- c(
  "Ni accord, ni décision de justice",
  "Accord écrit entre nous (pas soumis au juge pour approbation)",
  "Accord entre nous ratifié par un juge",
  "Décision judiciaire",
  "Procédure en cours"
)

arr_en <- c(
  "No agreement, no court decision",
  "Written agreement (not submitted to judge)",
  "Agreement ratified by a judge",
  "Court decision",
  "Ongoing procedure"
)

# Translated variable
FamyCH_w1$Decision_CA <- arr_en[match(FamyCH_w1$LAWLA01_R, arr_fr)]

# Ordered
FamyCH_w1$Decision_CA <- factor(
  FamyCH_w1$Decision_CA,
  levels = arr_en,
  ordered = TRUE
)

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Decision_CA_2 = case_when(
      Decision_CA == "Court decision" ~ "Court decision",
      Decision_CA == "Agreement ratified by a judge" ~ "Agreement ratified by a judge",
      Decision_CA == "Written agreement (not submitted to judge)" ~ "Written agreement (not submitted to judge)",
      Decision_CA == "No agreement, no court decision" ~ "No agreement, no court decision",
      Decision_CA == "Ongoing procedure" ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------
# Language region
FamyCH_w1$Language_Region <- as.factor(FamyCH_w1$Canton %>%
                                         fct_recode(
                                           "German" = "AG",
                                           "German" = "AI",
                                           "German" = "AR",
                                           "German" = "BE",
                                           "German" = "BL",
                                           "German" = "BS",
                                           "French" = "FR",
                                           "French" = "GE",
                                           "German" = "GL",
                                           "German" = "GR",
                                           "French" = "JU",
                                           "German" = "LU",
                                           "French" = "NE",
                                           "German" = "NW",
                                           "German" = "OW",
                                           "German" = "SG",
                                           "German" = "SH",
                                           "German" = "SO",
                                           "German" = "SZ",
                                           "German" = "TG",
                                           "Italian" = "TI",
                                           "German" = "UR",
                                           "French" = "VD",
                                           "French" = "VS",
                                           "German" = "ZG",
                                           "German" = "ZH"))

# ------------------------------------------------------------
# Parents' migration background: preserve the original nested-ifelse semantics.
country_source_text <- function(x) {
  text <- source_response_text(x)
  code <- numeric_birth_value(x)
  text[which(code == 8100)] <- "8100"
  text
}

#Respondent born in Switzerland
FamyCH_w1$Respondent_born_Switzerland <- as.character(FamyCH_w1$ORIG01_R)
FamyCH_w1$Respondent_born_Switzerland[FamyCH_w1$Respondent_born_Switzerland == "Oui"] <- "Yes"
FamyCH_w1$Respondent_born_Switzerland[FamyCH_w1$Respondent_born_Switzerland == "Non"] <- "No"

#Other parent born in Switzerland
FamyCH_w1$CPART06_P <- country_source_text(FamyCH_w1$CPART06_P)
FamyCH_w1$OP04_O <- country_source_text(FamyCH_w1$OP04_O)



country_op_source <- ifelse(FamyCH_w1$Intact %in% 1,
                            FamyCH_w1$CPART06_P, FamyCH_w1$OP04_O)
FamyCH_w1$Second_parent_born_Switzerland <- case_when(
  is.na(country_op_source) | country_op_source %in%
    c("Sans réponse", "Ne s'applique pas", "Je ne sais pas", "No partner", "No other parent", "") ~ NA_character_,
  country_op_source == "8100" ~ "Yes",
  TRUE ~ "No"
)
FamyCH_w1$Respondent_born_Switzerland[
  !FamyCH_w1$Respondent_born_Switzerland %in% c("Yes", "No")] <- NA_character_

FamyCH_w1$Parents_born_Switzerland <- 
  ifelse(FamyCH_w1$Respondent_born_Switzerland == "Yes" & FamyCH_w1$Second_parent_born_Switzerland == "Yes", "1/ Both parents born in Switzerland", 
         ifelse(FamyCH_w1$Respondent_born_Switzerland == "Yes" & FamyCH_w1$Second_parent_born_Switzerland == "No", "2/ Only one parent born in Switzerland", 
                ifelse(FamyCH_w1$Respondent_born_Switzerland == "No" & FamyCH_w1$Second_parent_born_Switzerland == "Yes", "2/ Only one parent born in Switzerland", 
                       ifelse(FamyCH_w1$Respondent_born_Switzerland == "No" & FamyCH_w1$Second_parent_born_Switzerland == "No", "5/ Both parents born outside Switzerland", 
                              ifelse(FamyCH_w1$Respondent_born_Switzerland == "No" & FamyCH_w1$Second_parent_born_Switzerland == "Missing", "4/ One parent born outside Switzerland, other missing",
                                     ifelse(FamyCH_w1$Respondent_born_Switzerland == "Yes" & FamyCH_w1$Second_parent_born_Switzerland == "Missing", "3/ One parent born in Switzerland, other missing",
                                            "No information"))))))

FamyCH_w1$Country_birth <- factor(
  dplyr::case_when(
    FamyCH_w1$Respondent_born_Switzerland == "Yes" &
      FamyCH_w1$Second_parent_born_Switzerland == "Yes" ~
      "Both parents born in Switzerland",
    
    (FamyCH_w1$Respondent_born_Switzerland == "Yes" &
       FamyCH_w1$Second_parent_born_Switzerland == "No") |
      (FamyCH_w1$Respondent_born_Switzerland == "No" &
         FamyCH_w1$Second_parent_born_Switzerland == "Yes") ~
      "Only one parent born in Switzerland",
    
    FamyCH_w1$Respondent_born_Switzerland == "No" &
      FamyCH_w1$Second_parent_born_Switzerland == "No" ~
      "Both parents born outside Switzerland",
    
    TRUE ~ NA_character_
  ),
  levels = c(
    "Both parents born in Switzerland",
    "Only one parent born in Switzerland",
    "Both parents born outside Switzerland"
  )
)

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    Country_birth_2 = case_when(
      Country_birth == "Both parents born in Switzerland" ~ "Both parents born in Switzerland",
      Country_birth %in% c("Only one parent born in Switzerland",
                           "Both parents born outside Switzerland") ~ "At least one parent born outside Switzerland",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------
# Calendar
# ------------------------------------------------------------
# Usage: FamyCH_w1 <- calculate_calendar(FamyCH_w1)
# Source variables must retain their numeric SPSS codes.
# ------------------------------------------------------------

calculate_calendar <- function(data) {
  if (!is.data.frame(data)) stop("data must be a data.frame or tibble.")
  
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  positions <- seq(2, 14, by = 2)
  prefixes <- c("CA04_C_week1", "CA04_C_week2",
                "CA06_C_week3", "CA06_C_week4")
  sources <- unlist(lapply(prefixes, function(prefix) {
    unlist(lapply(positions, function(position) {
      paste0(prefix, "_", position, "_",
             c("SelfOrOtherParent1", "SelfOrOtherParent2", "OtherPerson"))
    }), use.names = FALSE)
  }), use.names = FALSE)
  missing_columns <- setdiff(sources, names(data))
  if (length(missing_columns)) {
    stop("Missing variables: ", paste(missing_columns, collapse = ", "))
  }
  
  # SPSS MISSING() includes user-defined missing values.
  # haven::zap_missing() converts imported user-defined missing values to NA
  # when read_sav(..., user_na = TRUE) was used. Source codes are preserved.
  convert_codes <- function(x, name) {
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      if (!requireNamespace("haven", quietly = TRUE)) {
        stop("Install haven to process labelled SPSS variables.")
      }
      x <- haven::zap_labels(haven::zap_missing(x))
    }
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x[trimws(x) == ""] <- NA_character_
      result <- suppressWarnings(as.numeric(x))
      if (any(!is.na(x) & is.na(result))) {
        stop("The variable ", name,
             " contains non-numeric labels. Use the original SPSS numeric codes.")
      }
      return(result)
    }
    if (!is.numeric(x)) stop("Non-numeric type for ", name, ".")
    as.numeric(x)
  }
  codes <- setNames(lapply(sources, function(name) {
    convert_codes(data[[name]], name)
  }), sources)
  
  n <- nrow(data)
  # SPSS IF updates a value only when its condition is true.
  # which() ignores missing conditions rather than propagating them.
  true_rows <- function(condition) which(!is.na(condition) & condition)
  
  # ------------------------------------------------------------
  # Daily allocation, preserving the order of SPSS IF statements
  # ------------------------------------------------------------
  calculate_day <- function(p1, p2, other_person) {
    R <- OP <- other <- numeric(n)
    R[true_rows(p1 == 1 & is.na(p2) & is.na(other_person))] <- 1
    OP[true_rows(p1 == 2 & is.na(other_person))] <- 1
    other[true_rows(is.na(p1) & is.na(p2) & other_person >= 1 & other_person <= 5)] <- 1
    
    both_parents <- true_rows(p1 == 1 & p2 == 2)
    R[both_parents] <- 0.5
    OP[both_parents] <- 0.5
    
    R[true_rows(p1 == 1 & other_person >= 1 & other_person <= 4)] <- 1
    OP[true_rows(p1 == 2 & other_person >= 1 & other_person <= 4)] <- 1
    other[true_rows((p1 == 1 | p1 == 2) & other_person == 5)] <- 1
    
    total <- R + OP + other
    i <- true_rows(total > 0)
    R[i] <- R[i] / total[i]
    OP[i] <- OP[i] / total[i]
    other[i] <- other[i] / total[i]
    list(R = R, OP = OP, other = other, total = total)
  }
  
  # ------------------------------------------------------------
  # Four weeks and weekly percentages
  # ------------------------------------------------------------
  for (week in 1:4) {
    suffix <- if (week == 1) "" else as.character(week)
    for (j in seq_along(days)) {
      source <- paste0(prefixes[week], "_", positions[j], "_")
      result <- calculate_day(
        codes[[paste0(source, "SelfOrOtherParent1")]],
        codes[[paste0(source, "SelfOrOtherParent2")]],
        codes[[paste0(source, "OtherPerson")]]
      )
      for (person in c("R", "OP", "other")) {
        data[[paste0(days[j], suffix, "_N_", person)]] <- result[[person]]
      }
      data[[paste0("Total", suffix, days[j])]] <- result$total
    }
    
    total_prefix <- paste0("Total", suffix)
    for (person in c("R", "OP", "other")) {
      columns <- paste0(days, suffix, "_N_", person)
      data[[paste0(total_prefix, "_", person)]] <-
        rowSums(as.matrix(data[, columns, drop = FALSE]), na.rm = FALSE)
    }
    total <- data[[paste0(total_prefix, "_R")]] +
      data[[paste0(total_prefix, "_OP")]] +
      data[[paste0(total_prefix, "_other")]]
    data[[paste0(total_prefix, "_")]] <- total
    
    for (person in c("R", "OP", "other")) {
      percentage <- rep(NA_real_, n)
      i <- true_rows(total > 0)
      percentage[i] <- data[[paste0(total_prefix, "_", person)]][i] / 7 * 100
      data[[paste0(total_prefix, "_", person, "_Percent")]] <- percentage
    }
    data[[paste0(total_prefix, "_Percent")]] <-
      data[[paste0(total_prefix, "_R_Percent")]] +
      data[[paste0(total_prefix, "_OP_Percent")]] +
      data[[paste0(total_prefix, "_other_Percent")]]
  }
  
  # ------------------------------------------------------------
  # Average weeks 1-2, weeks 3-4, and all four weeks
  # As with SPSS addition, a missing component produces a missing average.
  # ------------------------------------------------------------
  for (person in c("R", "OP", "other")) {
    data[[paste0("Total1_2_", person, "_Percent")]] <-
      (data[[paste0("Total_", person, "_Percent")]] +
         data[[paste0("Total2_", person, "_Percent")]]) / 2
    data[[paste0("Total3_4_", person, "_Percent")]] <-
      (data[[paste0("Total3_", person, "_Percent")]] +
         data[[paste0("Total4_", person, "_Percent")]]) / 2
  }
  for (pair in c("1_2", "3_4")) {
    data[[paste0("Total", pair, "_Percent")]] <-
      data[[paste0("Total", pair, "_R_Percent")]] +
      data[[paste0("Total", pair, "_OP_Percent")]] +
      data[[paste0("Total", pair, "_other_Percent")]]
  }
  i <- true_rows(data$Total3_4_Percent > 0)
  for (person in c("R", "OP", "other")) {
    result <- rep(NA_real_, n)
    result[i] <- (data[[paste0("Total1_2_", person, "_Percent")]][i] +
                    data[[paste0("Total3_4_", person, "_Percent")]][i]) / 2
    data[[paste0("Total1234_", person)]] <- result
  }
  components <- as.matrix(data[, paste0("Total1234_", c("R", "OP", "other")),
                               drop = FALSE])
  # SPSS SUM() ignores missing values but returns NA when all components are missing.
  data$Total_1234 <- rowSums(components, na.rm = TRUE)
  data$Total_1234[rowSums(!is.na(components)) == 0] <- NA_real_
  
  # ------------------------------------------------------------
  # Final percentages: use four weeks when available,
  # otherwise use weeks 1-2
  # ------------------------------------------------------------
  for (person in c("R", "OP", "other")) {
    result <- data[[paste0("Total1_2_", person, "_Percent")]]
    four_weeks <- data[[paste0("Total1234_", person)]]
    i <- which(!is.na(four_weeks))
    result[i] <- four_weeks[i]
    data[[paste0("Total_", person, "_Final")]] <- result
  }
  data$Total_Final <- data$Total1_2_Percent
  i <- which(!is.na(data$Total_1234))
  data$Total_Final[i] <- data$Total_1234[i]
  data$CA_pct <- rep(NA_real_, n)
  data$CA_pct[true_rows(data$Total_Final >= 99)] <- 100
  
  data$CA_other <- numeric(n)
  data$CA_other[true_rows(data$Total_other_Final >= 30)] <- 1
  data$Total_parents <- rep(NA_real_, n)
  i <- true_rows(data$CA_other == 0)
  data$Total_parents[i] <- data$Total_R_Final[i] + data$Total_OP_Final[i]
  data$Total_R_Final2 <- data$Total_OP_Final2 <- rep(NA_real_, n)
  i <- true_rows(data$Total_parents > 0)
  data$Total_R_Final2[i] <- data$Total_R_Final[i] / data$Total_parents[i] * 100
  data$Total_OP_Final2[i] <- data$Total_OP_Final[i] / data$Total_parents[i] * 100
  data$Total_parents2 <- rep(NA_real_, n)
  i <- true_rows(data$CA_other == 0)
  data$Total_parents2[i] <- data$Total_R_Final2[i] + data$Total_OP_Final2[i]
  data
}

# ------------------------------------------------------------
# RUN ON THE CALENDAR DATA
# ------------------------------------------------------------
FamyCH_w1 <- calculate_calendar(FamyCH_w1)

# ------------------------------------------------------------
# Create custody variables
# ------------------------------------------------------------

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    
    # ------------------------------------------------------------
    # Custody classification by respondent and other parent
    # ------------------------------------------------------------
    # Total_other_Final is the existing other-person share, not a separately
    # identified institution-only share. 
    CA_full_start_final = case_when(
      !is.na(Total_other_Final) & Total_other_Final >= 30 ~ "Other",
      !is.na(Total_R_Final2) & Total_R_Final2 > 70 ~ "LPC Resp",
      !is.na(Total_OP_Final2) & Total_OP_Final2 > 70 ~ "LPC OP",
      !is.na(Total_R_Final2) &
        Total_R_Final2 >= 55 & Total_R_Final2 <= 70 ~ "SPC Resp primary",
      !is.na(Total_OP_Final2) &
        Total_OP_Final2 >= 55 & Total_OP_Final2 <= 70 ~ "SPC OP primary",
      (!is.na(Total_R_Final2) & Total_R_Final2 >= 45 & Total_R_Final2 <= 55) |
        (!is.na(Total_OP_Final2) & Total_OP_Final2 >= 45 & Total_OP_Final2 <= 55) ~ "Equal SPC",
      TRUE ~ NA_character_
    ),
    
    CA_full_start_final = factor(
      CA_full_start_final,
      levels = c(
        "LPC Resp",
        "SPC Resp primary",
        "Equal SPC",
        "SPC OP primary",
        "LPC OP",
        "Other"
      )
    ),
    
    # ------------------------------------------------------------
    # Custody classification by mother and father
    # ------------------------------------------------------------
    CA_full_final = case_when(
      
      # Mother LPC
      (CA_full_start_final == "LPC Resp" &
         Respondent_Parent_Gender == "Woman") |
        (CA_full_start_final == "LPC OP" &
           OP_Gender_final == "Woman") ~ "Mother LPC",
      
      # SPC mother primary
      (CA_full_start_final == "SPC Resp primary" &
         Respondent_Parent_Gender == "Woman") |
        (CA_full_start_final == "SPC OP primary" &
           OP_Gender_final == "Woman") ~ "SPC mother primary",
      
      # Equal SPC
      CA_full_start_final == "Equal SPC" ~ "Equal SPC",
      
      # SPC father primary
      (CA_full_start_final == "SPC Resp primary" &
         Respondent_Parent_Gender == "Man") |
        (CA_full_start_final == "SPC OP primary" &
           OP_Gender_final == "Man") ~ "SPC father primary",
      
      # Father LPC
      (CA_full_start_final == "LPC Resp" &
         Respondent_Parent_Gender == "Man") |
        (CA_full_start_final == "LPC OP" &
           OP_Gender_final == "Man") ~ "Father LPC",
      
      # Other
      CA_full_start_final == "Other" ~ "Other",
      
      TRUE ~ NA_character_
    ),
    
    CA_full_final = factor(
      CA_full_final,
      levels = c(
        "Mother LPC",
        "SPC mother primary",
        "Equal SPC",
        "SPC father primary",
        "Father LPC",
        "Other"
      )
    )
  )


# ------------------------------------------------------------
# CA40: intact flags override the calendar; father LPC and Other are excluded.
FamyCH_w1 <- FamyCH_w1 %>% mutate(
  CA40 = case_when(
    !Analysis_Eligible ~ NA_character_,
    Intact == 1 | IntactNotLivingdTogether == 1 ~ NA_character_,
    CA_full_final == "Mother LPC" ~ "Mother LPC",
    CA_full_final %in% c("SPC mother primary", "SPC father primary") ~ "Unequal SPC",
    CA_full_final == "Equal SPC" ~ "Equal SPC",
    TRUE ~ NA_character_
  ),
  CA40 = factor(CA40, levels = c("Mother LPC", "Unequal SPC", "Equal SPC"))
)

# ------------------------------------------------------------
# Common categorical orders used in all analysis sections
# ------------------------------------------------------------
# The first category is the model reference. All tables label that reference
# directly from the data used to fit the model.
common_factor_levels <- list(
  CA40 = c("Mother LPC", "Unequal SPC", "Equal SPC"),
  Reference_child_Gender = c("Boy", "Girl"),
  Respondent_Parent_Gender = c("Woman", "Man"),
  Country_birth_2 = c("Both parents born in Switzerland",
                      "At least one parent born outside Switzerland"),
  Education_AtSep_Mother = c("Compulsory school", "Post-compulsory education", "Tertiary education"),
  EmploymentBeforeSD_Mother = c("Full-time", "Not in employment", "Part-time"),
  EmploymentBeforeSD_Father = c("Full-time", "Not in employment", "Part-time"),
  PreviousSD_WM_final = c("None", "Both", "Father only", "Mother only"),
  parental_issues_WM = c("None", "Both parents", "Only father", "Only mother"),
  Type_Union_final = c("Cohabiting", "Married"),
  Implication_Parents_Past_WM = c("Mother more", "Father more", "50-50"),
  Violence_Addictions_YN = c("No", "Yes"),
  Infidelity_YN = c("No", "Yes"),
  Parental_disagreement_YN = c("No", "Yes"),
  Conjugal_conflict_YN = c("No", "Yes"),
  Incompatibility_YN = c("No", "Yes"),
  Decision_CA_2 = c("Court decision", "Agreement ratified by a judge",
                    "No agreement, no court decision", "Written agreement (not submitted to judge)"),
  Language_Region = c("French", "German", "Italian")
)

apply_study_levels <- function(data, ordered_education = FALSE) {
  for (variable in intersect(names(common_factor_levels), names(data))) {
    values <- as.character(data[[variable]])
    unexpected <- setdiff(unique(values[!is.na(values)]), common_factor_levels[[variable]])
    if (length(unexpected)) {
      stop("Unexpected categories in ", variable, ": ", paste(unexpected, collapse = " | "))
    }
    data[[variable]] <- factor(values, levels = common_factor_levels[[variable]],
                               ordered = ordered_education && variable == "Education_AtSep_Mother")
  }
  data
}
FamyCH_w1 <- apply_study_levels(FamyCH_w1)
readr::write_csv(tibble::tibble(
  Variable = names(common_factor_levels),
  Reference = vapply(common_factor_levels, `[`, character(1), 1),
  Category_order = vapply(common_factor_levels, paste, character(1), collapse = " | ")
), file.path(output_dir, "Category_orders_and_references.csv"))


# Mask calendar-derived variables for SameSex == 1 without deleting rows.
# Keep raw calendar responses and all other usable study information unchanged.
calendar_derived_variables <- unique(c(
  unlist(lapply(1:4, function(week) {
    suffix <- if (week == 1L) "" else as.character(week)
    total_prefix <- paste0("Total", suffix)
    c(
      unlist(lapply(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), function(day) {
        c(paste0(day, suffix, "_N_", c("R", "OP", "other")),
          paste0("Total", suffix, day))
      }), use.names = FALSE),
      paste0(total_prefix, "_", c("R", "OP", "other")),
      paste0(total_prefix, "_"),
      paste0(total_prefix, "_", c("R", "OP", "other"), "_Percent"),
      paste0(total_prefix, "_Percent")
    )
  }), use.names = FALSE),
  unlist(lapply(c("1_2", "3_4"), function(pair) {
    c(paste0("Total", pair, "_", c("R", "OP", "other"), "_Percent"),
      paste0("Total", pair, "_Percent"))
  }), use.names = FALSE),
  paste0("Total1234_", c("R", "OP", "other")),
  "Total_1234", paste0("Total_", c("R", "OP", "other"), "_Final"),
  "Total_Final", "CA_pct", "CA_other", "Total_parents",
  "Total_R_Final2", "Total_OP_Final2", "Total_parents2",
  "CA_full_start_final", "CA_full_final", "CA40"
))
require_columns(FamyCH_w1, calendar_derived_variables)
coded_before_exclusions <- FamyCH_w1
same_sex_excluded <- FamyCH_w1$SameSex %in% 1
for (variable in calendar_derived_variables) {
  FamyCH_w1[[variable]][same_sex_excluded] <- NA
}
stopifnot(
  !any(FamyCH_w1$Analysis_Eligible[same_sex_excluded]),
  all(vapply(FamyCH_w1[calendar_derived_variables], function(x)
    all(is.na(x[same_sex_excluded])), logical(1)))
)

exclusion_register <- FamyCH_w1 %>% select(HOUSEHOLD_ID,
                                           Respondent_Parent_Gender, OP_Gender_final, SameSex,
                                           Gender_Role_Mapping_Eligible, Manual_Exclusion, Analysis_Eligible)
# Final-gender invariants: exclusions do not alter observed genders.
stopifnot(identical(FamyCH_w1$SameSex, with(FamyCH_w1, case_when(
  is.na(Respondent_Parent_Gender) | is.na(OP_Gender_final) ~ NA_real_,
  as.character(Respondent_Parent_Gender) == as.character(OP_Gender_final) ~ 1,
  TRUE ~ 0))))

readr::write_csv(FamyCH_w1 %>% select(HOUSEHOLD_ID, OtherParent_PartnerIndex_Coded,
                                      OP_birth_year_sep, OP_Intact_Birth_Year, OP_age_sep, OP_age_intact, OP_age, OP_birth_year,
                                      Resp_BirthYear, YearSepOP_combined, Date_SeparationWithOP, Duration_Relationship_OP_final,
                                      Reference_child_age, Reference_child_birth_date_approx, Incoherence_Hist_OP),
                 file.path(output_dir, "Age_dependencies.csv"))
# Save the study variables without deleting any household rows.
article_variables <- c("CA40", "Age_Child_AtSep", "Reference_child_Gender", "Age_Mother_AtSep",
                       "Age_Father_AtSep", "Respondent_Parent_Gender", "Country_birth_2", "Education_AtSep_Mother",
                       "EmploymentBeforeSD_Mother", "EmploymentBeforeSD_Father", "PreviousSD_WM_final", "parental_issues_WM",
                       "Type_Union_final", "YearsSinceSep_final", "Conflict_Before_Sep_Final", "Implication_Parents_Past_WM",
                       "Violence_Addictions_YN", "Infidelity_YN", "Parental_disagreement_YN", "Conjugal_conflict_YN",
                       "Incompatibility_YN", "Decision_CA_2", "Language_Region")
require_columns(FamyCH_w1, article_variables)
stopifnot(nrow(FamyCH_w1) == source_rows,
          identical(as.character(FamyCH_w1$HOUSEHOLD_ID), source_ids))
saveRDS(FamyCH_w1, file.path(output_dir, "FamyCH_W1_Study_Data.rds"))
readr::write_csv(FamyCH_w1 %>% select(HOUSEHOLD_ID, all_of(article_variables)),
                 file.path(output_dir, "Article_variables.csv"))
readr::write_csv(tibble(Variable = article_variables,
                        N_missing = vapply(FamyCH_w1[article_variables], function(x) sum(is.na(x)), integer(1))),
                 file.path(output_dir, "Missing_counts.csv"))

writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))

# Coding diagnostics: run before analyses ----------------------------------
# Detailed diagnostics are written to Coding_diagnostics.txt; R objects remain available.
# Raw response availability differs from a night recognized by the allocation
# algorithm. Weeks 3-4 are optional: report completeness for the window used.
raw_numeric <- function(column) numeric_birth_value(FamyCH_raw[[column]])
raw_text <- function(column) source_response_text(FamyCH_raw[[column]])

employment_above_100 <- bind_rows(lapply(
  c("PRIORDIV02_R", "PRIORDIV04_R"), function(column) tibble(
    HOUSEHOLD_ID = as.character(FamyCH_raw$HOUSEHOLD_ID),
    Source = column, Percentage = raw_numeric(column),
    Response_Label = raw_text(column)
  ))) %>% filter(!is.na(Percentage), Percentage > 100) %>%
  left_join(FamyCH_w1 %>% transmute(HOUSEHOLD_ID = as.character(HOUSEHOLD_ID),
                                    Respondent_Parent_Gender, OP_Gender_final, SameSex, Analysis_Eligible,
                                    EmploymentBeforeSD_Mother, EmploymentBeforeSD_Father), by = "HOUSEHOLD_ID")
employment_above_100_values <- employment_above_100 %>% count(Source, Percentage)

adversity_items <- c("serious_physical_illness", "psychological_problems",
                     "violence_or_addictions", "police_contact")
adversity_matrix <- as.matrix(FamyCH_w1[adversity_items])
adversity_diagnostics <- tibble(
  HOUSEHOLD_ID = FamyCH_w1$HOUSEHOLD_ID,
  N_observed = rowSums(!is.na(adversity_matrix)),
  Respondent_positive = rowSums(adversity_matrix == 1 | adversity_matrix == 3, na.rm = TRUE) > 0,
  Other_parent_positive = rowSums(adversity_matrix == 2 | adversity_matrix == 3, na.rm = TRUE) > 0,
  Both_positive_same_item = rowSums(adversity_matrix == 3, na.rm = TRUE) > 0,
  Derived = FamyCH_w1$parental_issues,
  Analysis_Eligible = FamyCH_w1$Analysis_Eligible
)

country_diagnostics <- tibble(
  HOUSEHOLD_ID = FamyCH_w1$HOUSEHOLD_ID,
  Intact = FamyCH_w1$Intact,
  Partner_source = raw_text("CPART06_P"),
  Other_parent_source = raw_text("OP04_O"),
  Partner_source_missing = is.na(haven::zap_missing(FamyCH_raw$CPART06_P)),
  Other_parent_source_missing = is.na(haven::zap_missing(FamyCH_raw$OP04_O)),
  Derived_second_parent = FamyCH_w1$Second_parent_born_Switzerland,
  Derived_before_exclusions = coded_before_exclusions$Country_birth_2,
  Analysis_Eligible = FamyCH_w1$Analysis_Eligible
) %>% mutate(Missing_source_classified_abroad =
               if_else(Intact %in% 1, Partner_source_missing, Other_parent_source_missing) &
               Derived_second_parent %in% "No")

duration_diagnostics <- duration_before_cleaning %>% mutate(
  Negative = !is.na(Duration_Relationship_OP_final) & Duration_Relationship_OP_final < 0,
  Unknown = is.na(Duration_Relationship_OP_final),
  Child_age_removed = !is.na(Age_Child_AtSep) &
    is.na(coded_before_exclusions$Age_Child_AtSep),
  Incoherence_Hist_OP = FamyCH_w1$Incoherence_Hist_OP
)
incoherence_diagnostics <- FamyCH_w1 %>% select(HOUSEHOLD_ID,
                                                Incoherence_Hist_OP, Respondent_Parent_Gender, OP_Gender_final) %>%
  left_join(duration_before_cleaning %>% select(HOUSEHOLD_ID,
                                                Duration_Relationship_OP_final, Age_Mother_AtSep, Age_Father_AtSep,
                                                Age_Child_AtSep), by = "HOUSEHOLD_ID") %>% mutate(
                                                  Mother_age_start_before_cleaning = Age_Mother_AtSep - Duration_Relationship_OP_final,
                                                  Father_age_start_before_cleaning = Age_Father_AtSep - Duration_Relationship_OP_final)

birth_month_diagnostics <- bind_rows(lapply(1:12, function(j) tibble(
  HOUSEHOLD_ID = FamyCH_w1$HOUSEHOLD_ID, Person_index = j,
  Person_ID = as.character(FamyCH_w1[[paste0("PersonId_", j)]]),
  Raw_month_code = suppressWarnings(as.numeric(as.character(
    haven::zap_labels(FamyCH_raw[[paste0("Personne", j, "_MoisNaissance")]])))),
  Raw_month_label = raw_text(paste0("Personne", j, "_MoisNaissance")),
  Used_month = numeric_birth_value(FamyCH_w1[[paste0("Personne", j, "_MoisNaissance")]]),
  Used_age = FamyCH_w1[[paste0("Personne", j, "_age")]],
  Is_reference_child = as.character(FamyCH_w1$ChildRef_Id) == as.character(FamyCH_w1[[paste0("PersonId_", j)]]),
  Is_respondent = FamyCH_w1$Respondent_PersonIndex %in% j,
  Is_other_parent = FamyCH_w1$OP_Intact_Index %in% j
)))
birth_month_special_codes <- birth_month_diagnostics %>%
  filter(Raw_month_code %in% c(-1, -2))
age_missing_diagnostics <- birth_month_diagnostics %>%
  filter(is.na(Used_age), !is.na(Person_ID),
         !trimws(Person_ID) %in% c("", "0", "-1", "-2"))
age_missing_summary <- age_missing_diagnostics %>% summarise(
  N_persons = n_distinct(HOUSEHOLD_ID, Person_ID),
  N_households = n_distinct(HOUSEHOLD_ID))

intact_history_observed <- rowSums(!is.na(FamyCH_w1[paste0("PART15_Ra_", 1:7)]))
intact_previous_separations <- FamyCH_w1 %>%
  mutate(N_observed_history_ends = intact_history_observed) %>%
  filter(Intact %in% 1) %>%
  select(HOUSEHOLD_ID, PreviousSep_Resp_Intact, N_observed_history_ends)
# User-defined convention: all-missing intact respondent history is No prior separation.

infidelity_response_diagnostics <- FamyCH_long %>%
  count(reason, reason_group, sort = TRUE)
infidelity_possible_unmatched <- FamyCH_long %>% filter(is.na(reason_group)) %>%
  filter(stringr::str_detect(stringr::str_to_lower(reason),
                             "tromp|infid|relation|adult")) %>% count(reason, sort = TRUE)

calendar_diagnostics <- tibble(
  HOUSEHOLD_ID = FamyCH_w1$HOUSEHOLD_ID,
  Intact = FamyCH_w1$Intact,
  IntactNotLivingdTogether = FamyCH_w1$IntactNotLivingdTogether,
  Analysis_Eligible = FamyCH_w1$Analysis_Eligible
)
calendar_prefixes <- c("CA04_C_week1", "CA04_C_week2", "CA06_C_week3", "CA06_C_week4")
calendar_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
for (week in 1:4) {
  suffix <- if (week == 1) "" else as.character(week)
  any_answer <- vapply(seq_along(calendar_days), function(j) {
    columns <- paste0(calendar_prefixes[week], "_", 2*j, "_",
                      c("SelfOrOtherParent1", "SelfOrOtherParent2", "OtherPerson"))
    rowSums(vapply(columns, function(column)
      !is.na(haven::zap_missing(FamyCH_raw[[column]])), logical(nrow(FamyCH_raw)))) > 0
  }, logical(nrow(FamyCH_w1)))
  allocated <- vapply(calendar_days, function(day) {
    total <- Reduce(`+`, lapply(c("R", "OP", "other"), function(person)
      FamyCH_w1[[paste0(day, suffix, "_N_", person)]]))
    !is.na(total) & total > 0
  }, logical(nrow(FamyCH_w1)))
  calendar_diagnostics[[paste0("Answered_week", week)]] <- rowSums(any_answer)
  calendar_diagnostics[[paste0("Allocated_week", week)]] <- rowSums(allocated)
}
calendar_diagnostics <- calendar_diagnostics %>% mutate(
  Answered_first14 = Answered_week1 + Answered_week2,
  Answered_last14 = Answered_week3 + Answered_week4,
  Allocated_first14 = Allocated_week1 + Allocated_week2,
  Allocated_last14 = Allocated_week3 + Allocated_week4,
  Four_weeks_used = !is.na(FamyCH_w1$Total_1234),
  Expected_nights = if_else(Four_weeks_used, 28L, 14L),
  Allocated_nights_used = if_else(Four_weeks_used,
                                  Allocated_first14 + Allocated_last14, Allocated_first14),
  Missing_nights_used = Expected_nights - Allocated_nights_used,
  Coverage_percent = 100 * Allocated_nights_used / Expected_nights,
  Incomplete_window = Missing_nights_used > 0,
  CA40_before_exclusions = coded_before_exclusions$CA40,
  CA40_final = FamyCH_w1$CA40,
  CA40_observed = !is.na(CA40_final),
  Classified_final = CA40_observed, # compatibility alias: this measures CA40 only
  Calendar_category_available = !is.na(FamyCH_w1$CA_full_final),
  Coverage_band = case_when(
    Coverage_percent == 0 ~ "0%",
    Coverage_percent < 50 ~ "1-49%",
    Coverage_percent < 100 ~ "50-99%",
    TRUE ~ "100%")
)
calendar_completeness_summary <- calendar_diagnostics %>%
  filter(!(Intact %in% 1 | IntactNotLivingdTogether %in% 1)) %>%
  count(Analysis_Eligible, Four_weeks_used, Coverage_band, Classified_final, name = "N")
calendar_incomplete_details <- calendar_diagnostics %>%
  filter(!(Intact %in% 1 | IntactNotLivingdTogether %in% 1), Incomplete_window)

op_birth_year_diagnostics <- FamyCH_w1 %>% select(HOUSEHOLD_ID,
                                                  OtherParent_PartnerIndex_Coded, OP_birth_year_sep, OP_Intact_Birth_Year,
                                                  SecondParentAnneeNaissance, OP_birth_year, OP_age, Intact, IntactNotLivingdTogether)
if ("OtherParent_PartnerIndex" %in% names(FamyCH_raw)) {
  op_birth_year_diagnostics$Original_index <- numeric_birth_value(FamyCH_raw$OtherParent_PartnerIndex)
}

coding_diagnostics <- list(
  Package_versions = package_versions,
  Employment_above_100_values = employment_above_100_values,
  Employment_above_100_households = employment_above_100,
  Adversity_patterns = adversity_diagnostics %>% count(N_observed,
                                                       Respondent_positive, Other_parent_positive, Both_positive_same_item, Derived),
  Missing_country_source_classified_abroad = country_diagnostics %>%
    filter(Missing_source_classified_abroad),
  Duration_patterns = duration_diagnostics %>% count(Negative, Unknown, Child_age_removed),
  Widow_CA40_check = FamyCH_w1 %>% filter(Widow %in% 1) %>% count(CA40, name = "N"),
  Separation_date_checks = FamyCH_w1 %>% count(Protect_Separation_Date, Separation_Date_Future, Separation_Approximation_Bounded),
  Incoherence_patterns = incoherence_diagnostics %>% count(Incoherence_Hist_OP),
  Incoherence_rules = FamyCH_w1 %>% select(HOUSEHOLD_ID, starts_with("History_"), Resp_Invalid_Age_Source, OP_Invalid_Age_Source, Incoherence_Hist_OP),
  Birth_month_special_codes = birth_month_special_codes %>% count(Raw_month_code, Raw_month_label, Used_month),
  Missing_age_counts = age_missing_summary,
  Missing_age_details = age_missing_diagnostics,
  Missing_age_by_role = age_missing_diagnostics %>%
    count(Is_reference_child, Is_respondent, Is_other_parent),
  Intact_previous_separations = intact_previous_separations %>%
    count(N_observed_history_ends, PreviousSep_Resp_Intact),
  Exclusions = exclusion_register %>% filter(!Analysis_Eligible),
  Possible_unmatched_infidelity = infidelity_possible_unmatched,
  Calendar_completeness = calendar_completeness_summary,
  Calendar_incomplete_classified = calendar_incomplete_details %>%
    count(Expected_nights, Missing_nights_used, Classified_final)
)
diagnostics_file <- file.path(output_dir, "Coding_diagnostics.txt")
diagnostics_text <- capture.output({
  cat("FamyCH wave-1 coding diagnostics\n")
  cat("Generated: ", format(Sys.time()), "\n", sep = "")
  for (name in names(coding_diagnostics)) {
    cat("\n", name, "\n", sep = "")
    print(tibble::as_tibble(coding_diagnostics[[name]]), n = Inf, width = Inf)
  }
})
writeLines(diagnostics_text, diagnostics_file, useBytes = TRUE)
message("Coding diagnostics saved to: ", diagnostics_file)
# END OF CODING DIAGNOSTICS ------------------------------------------------
# Execute through this marker to inspect the coding without fitting models.

# Descriptive analysis ----------------------------------------------------
# ------------------------------------------------------------

# ------------------------------------------------------------
# Descriptive table
# ------------------------------------------------------------
# DESCRIPTIVE TABLE BY CA40
# Overall p-values from multinomial logistic regressions
# ------------------------------------------------------------
# Ensure continuous variables are numeric
# ------------------------------------------------------------

vars_numeric_cibles <- c(
  "Age_Child_AtSep",
  "Age_Mother_AtSep",
  "Age_Father_AtSep",
  "Conflict_Before_Sep_Final",
  "YearsSinceSep_final"
)

FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    across(
      all_of(vars_numeric_cibles),
      ~ suppressWarnings(
        as.numeric(as.character(haven::zap_labels(.)))
      )
    ),
    
    # CA40 must be an unordered factor for multinom()
    CA40 = factor(CA40, ordered = FALSE)
  )

# ------------------------------------------------------------
# Table variables in the requested order
# ------------------------------------------------------------

vars_table_descr <- c(
  "Age_Child_AtSep",
  "Reference_child_Gender",
  "Age_Mother_AtSep",
  "Age_Father_AtSep",
  "Respondent_Parent_Gender",
  "Country_birth_2",
  "Education_AtSep_Mother",
  "EmploymentBeforeSD_Mother",
  "EmploymentBeforeSD_Father",
  "PreviousSD_WM_final",
  "parental_issues_WM",
  "Type_Union_final",
  "YearsSinceSep_final",
  "Conflict_Before_Sep_Final",
  "Implication_Parents_Past_WM",
  "Violence_Addictions_YN",
  "Infidelity_YN",
  "Parental_disagreement_YN",
  "Conjugal_conflict_YN",
  "Incompatibility_YN",
  "Decision_CA_2",
  "Language_Region"
)

# ------------------------------------------------------------
# Calculate the overall multinomial p-value
# ------------------------------------------------------------

get_multinom_p <- function(data, variable, outcome = "CA40") {
  
  dat_model <- data %>%
    dplyr::select(all_of(c(outcome, variable))) %>%
    tidyr::drop_na() %>%
    mutate(
      across(where(is.factor), droplevels)
    )
  
  # Minimum model checks
  if (
    nrow(dat_model) == 0 ||
    dplyr::n_distinct(dat_model[[outcome]]) < 2 ||
    dplyr::n_distinct(dat_model[[variable]]) < 2
  ) {
    return(NA_real_)
  }
  
  # Intercept-only model
  model_null <- tryCatch(
    nnet::multinom(
      reformulate("1", response = outcome),
      data = dat_model,
      trace = FALSE, maxit = 1000, MaxNWts = 10000
    ),
    error = function(e) NULL
  )
  
  # Model containing the predictor
  model_full <- tryCatch(
    nnet::multinom(
      reformulate(variable, response = outcome),
      data = dat_model,
      trace = FALSE, maxit = 1000, MaxNWts = 10000
    ),
    error = function(e) NULL
  )
  
  if (is.null(model_null) || is.null(model_full) ||
      model_null$convergence != 0 || model_full$convergence != 0) {
    return(NA_real_)
  }
  
  # Likelihood-ratio test
  ll_null <- logLik(model_null)
  ll_full <- logLik(model_full)
  
  lr_stat <- 2 * (
    as.numeric(ll_full) - as.numeric(ll_null)
  )
  
  df_test <- attr(ll_full, "df") - attr(ll_null, "df")
  
  if (
    is.na(lr_stat) ||
    is.na(df_test) ||
    df_test <= 0 ||
    lr_stat < 0
  ) {
    return(NA_real_)
  }
  
  stats::pchisq(
    lr_stat,
    df = df_test,
    lower.tail = FALSE
  )
}

# ------------------------------------------------------------
# Calculate p-values for all variables
# ------------------------------------------------------------

multinom_pvalues <- purrr::map_dfr(
  vars_table_descr,
  function(var) {
    
    tibble::tibble(
      variable = var,
      p_multinom = get_multinom_p(
        data = FamyCH_w1,
        variable = var,
        outcome = "CA40"
      )
    )
  }
)

# Display the calculated p-values
multinom_pvalues

# ------------------------------------------------------------
# Descriptive table
# ------------------------------------------------------------

table_descr <- FamyCH_w1 %>%
  filter(!is.na(CA40)) %>%
  dplyr::select(
    CA40,
    all_of(vars_table_descr)
  ) %>%
  tbl_summary(
    by = CA40,
    missing = "no",
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})"
    ),
    
    type = list(
      where(is.numeric) ~ "continuous",
      
      c(
        Type_Union_final,
        PreviousSD_WM_final,
        Violence_Addictions_YN,
        Infidelity_YN,
        Parental_disagreement_YN,
        Conjugal_conflict_YN,
        Incompatibility_YN,
        Respondent_Parent_Gender,
        Reference_child_Gender,
        Country_birth_2,
        parental_issues_WM,
        Language_Region,
        Implication_Parents_Past_WM,
        Decision_CA_2
      ) ~ "categorical"
    ),
    
    digits = list(
      all_continuous() ~ 1
    ),
    
    label = list(
      Age_Child_AtSep =
        "Child's age at separation",
      
      Reference_child_Gender =
        "Child's gender",
      
      Respondent_Parent_Gender =
        "Respondent parent gender",
      
      Age_Mother_AtSep =
        "Mother's age at separation",
      
      Age_Father_AtSep =
        "Father's age at separation",
      
      Education_AtSep_Mother =
        "Mother's education at separation",
      
      EmploymentBeforeSD_Mother =
        "Mother's employment before separation",
      
      EmploymentBeforeSD_Father =
        "Father's employment before separation",
      
      Country_birth_2 =
        "Parents' migration background",
      
      parental_issues_WM =
        "Parental adversities",
      
      Type_Union_final =
        "Type of union",
      
      YearsSinceSep_final =
        "Years since separation",
      
      Conflict_Before_Sep_Final =
        "Conflict before separation",
      
      Implication_Parents_Past_WM =
        "Division of childcare before separation",
      
      PreviousSD_WM_final =
        "Previous separations",
      
      Decision_CA_2 =
        "Custody agreement",
      
      Language_Region =
        "Language region",
      
      Violence_Addictions_YN =
        "Reason for separation: Violence or addictions",
      
      Infidelity_YN =
        "Reason for separation: Infidelity",
      
      Parental_disagreement_YN =
        "Reason for separation: Parental disagreement",
      
      Conjugal_conflict_YN =
        "Reason for separation: Conjugal conflict",
      
      Incompatibility_YN =
        "Reason for separation: Incompatibility"
    )
  )

# ------------------------------------------------------------
# CA40 contains only the three eligible separated-family categories.
# ------------------------------------------------------------
# Add the multinomial p-value column on the right
# ------------------------------------------------------------

table_descr <- table_descr %>%
  modify_table_body(
    ~ .x %>%
      left_join(
        multinom_pvalues,
        by = "variable"
      ) %>%
      mutate(
        # Display the p-value only on the variable's main row
        p_multinom = case_when(
          row_type == "label" & !is.na(p_multinom) ~
            gtsummary::style_pvalue(p_multinom, digits = 3),
          
          TRUE ~ ""
        )
      ) %>%
      relocate(
        p_multinom,
        .after = last_col()
      )
  ) %>%
  
  # Make the column visible and set its header
  modify_header(
    p_multinom = "**p-value**"
  )

# ------------------------------------------------------------
# Final formatting
# ------------------------------------------------------------

table_descr <- table_descr %>%
  add_n(statistic = "{N_nonmiss}", col_label = "**N**") %>%
  modify_header(all_stat_cols() ~ "**{level}**  \nN = {n}") %>%
  modify_caption(
    "Table 2. Descriptive Characteristics by Custody Arrangement"
  ) %>%
  bold_labels() %>%
  modify_footnote(
    p_multinom ~ paste0(
      "Global p-value from a bivariate multinomial logistic regression ",
      "comparing the model containing the predictor with the intercept-only model."
    )
  )

# ------------------------------------------------------------
# Convert to flextable
# Portrait layout, font size and row spacing
# Indent category labels
# ------------------------------------------------------------

# Group sample sizes appear only in the column headers.

# Identify category rows
category_rows <- which(
  table_descr$table_body$row_type %in% c("level")
)

# Convert to flextable
ft <- as_flex_table(table_descr)

# Number of columns in the flextable
n_cols_ft <- ncol(ft$body$dataset)

# General formatting
ft <- ft %>%
  fontsize(
    size = 8,
    part = "all"
  ) %>%
  padding(
    padding.top = 2.5,
    padding.bottom = 2.5,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  ) %>%
  line_spacing(
    space = 1.05,
    part = "all"
  ) %>%
  align(
    j = 1,
    align = "left",
    part = "all"
  )

# Center result columns
if (n_cols_ft >= 2) {
  ft <- ft %>%
    align(
      j = 2:n_cols_ft,
      align = "center",
      part = "all"
    )
}

# Indent category labels
if (length(category_rows) > 0) {
  ft <- ft %>%
    padding(
      i = category_rows,
      j = 1,
      padding.left = 12,
      part = "body"
    )
}

# Bold variable labels
variable_rows <- which(
  table_descr$table_body$row_type == "label"
)

if (length(variable_rows) > 0) {
  ft <- ft %>%
    bold(
      i = variable_rows,
      j = 1,
      bold = TRUE,
      part = "body"
    )
}

# Fit the table to page width
ft <- ft %>%
  autofit() %>%
  set_table_properties(
    layout = "autofit",
    width = 1,
    opts_word = list(
      split = TRUE
    )
  )

# Variable-column width
ft <- ft %>%
  width(
    j = 1,
    width = 2.65
  )

# Result-column widths
if (n_cols_ft >= 2) {
  ft <- ft %>%
    width(
      j = 2:n_cols_ft,
      width = 0.82
    )
}

ft_descriptive <- ft

ft_descriptive
# END OF DESCRIPTIVE-ONLY EXECUTION -----------------------------------------
# To run only the descriptive table, execute from the beginning through the
# ft_descriptive line above. Everything needed by that table has now run.

# ------------------------------------------------------------
# Sample counts for effect tables. A variable's label row carries total N and
# custody-specific Ns; a continuous effect retains its estimate plus its group N.
# Counts refer to model participants, not to the number of imputed cells.
# Preserve model counts as metadata; never append counts to estimate cells.
add_model_sample_counts <- function(long_table, sample_for_variable) {
  result <- long_table
  if (!"N" %in% names(result)) result$N <- NA_integer_
  result$N_outcome <- NA_integer_
  for (variable in unique(as.character(result$Variable))) {
    model_data <- sample_for_variable(variable)
    ix <- which(as.character(result$Variable) == variable)
    total <- nrow(model_data)
    previous_n <- result$N[ix]
    if (any(!is.na(previous_n) & previous_n != total))
      stop("Sample-count mismatch for ", variable)
    result$N[ix] <- total
    for (outcome in unique(as.character(result$Outcome[ix]))) {
      cells <- ix[as.character(result$Outcome[ix]) == outcome]
      result$N_outcome[cells] <- sum(
        as.character(model_data$CA40) == outcome, na.rm = TRUE
      )
    }
  }
  result
}

format_header_n <- function(x) {
  x <- sort(unique(x[!is.na(x)]))
  if (!length(x)) stop("Missing sample counts for a column header")
  if (length(x) == 1L) return(as.character(x))
  paste0(min(x), "–", max(x))
}

custody_header_labels <- function(long_table) {
  outcomes <- c("Mother LPC", "Unequal SPC", "Equal SPC")
  labels <- vapply(outcomes, function(outcome) {
    paste0(outcome, "\nN = ", format_header_n(
      long_table$N_outcome[as.character(long_table$Outcome) == outcome]
    ))
  }, character(1))
  stats::setNames(labels, outcomes)
}

# One canonical factor registry governs category order and references everywhere.
# Effect tables sort by RowOrder derived from these factor levels.

# Bivariate analysis ------------------------------------------------------
# ------------------------------------------------------------
# BIVARIATE MULTINOMIAL AME — CA40
# Shared complete-case sample across all CC models
# ------------------------------------------------------------
# Variables
labels_vec <- c(
  Age_Child_AtSep = "Child's age at separation",
  Reference_child_Gender = "Child's gender",
  Age_Mother_AtSep = "Mother's age at separation",
  Age_Father_AtSep = "Father's age at separation",
  Country_birth_2 = "Parents' migration background",
  Education_AtSep_Mother = "Mother's education at separation",
  EmploymentBeforeSD_Mother = "Mother's employment before separation",
  EmploymentBeforeSD_Father = "Father's employment before separation",
  PreviousSD_WM_final = "Previous separations",
  parental_issues_WM = "Parental adversities",
  Type_Union_final = "Type of union",
  YearsSinceSep_final = "Years since separation",
  Conflict_Before_Sep_Final = "Conflict before separation",
  Implication_Parents_Past_WM = "Division of childcare before separation",
  Violence_Addictions_YN = "Reason for separation: Violence or addictions",
  Infidelity_YN = "Reason for separation: Infidelity",
  Parental_disagreement_YN = "Reason for separation: Parental disagreement",
  Conjugal_conflict_YN = "Reason for separation: Conjugal conflict",
  Incompatibility_YN = "Reason for separation: Incompatibility",
  Decision_CA_2 = "Custody agreement",
  Language_Region = "Language region"
)

var_order <- names(labels_vec)

numeric_vars <- c(
  "Age_Child_AtSep",
  "Age_Mother_AtSep",
  "Age_Father_AtSep",
  "Conflict_Before_Sep_Final",
  "YearsSinceSep_final"
)

factor_vars <- setdiff(var_order, numeric_vars)

# Data preparation
FamyCH_w1 <- FamyCH_w1 %>%
  mutate(
    CA40 = relevel(factor(CA40, ordered = FALSE), ref = "Mother LPC"),
    across(
      all_of(numeric_vars),
      ~ suppressWarnings(as.numeric(as.character(zap_labels(.))))
    ),
    across(all_of(factor_vars), ~ factor(., ordered = FALSE)),
    EmploymentBeforeSD_Mother =
      relevel(EmploymentBeforeSD_Mother, ref = "Full-time"),
    EmploymentBeforeSD_Father =
      relevel(EmploymentBeforeSD_Father, ref = "Full-time")
  )

# One shared complete-case sample for ALL bivariate and adjusted CC models.
# Use the union of study variables, including the respondent-gender control.
cc_required_vars <- unique(c(article_variables, "Respondent_Parent_Gender"))
stopifnot(all(cc_required_vars %in% names(FamyCH_w1)))
cc_keep <- !is.na(FamyCH_w1$CA40) &
  complete.cases(FamyCH_w1[, cc_required_vars, drop = FALSE])
cc_household_ids <- as.character(FamyCH_w1$HOUSEHOLD_ID[cc_keep])
if (!length(cc_household_ids)) stop("The common complete-case sample is empty.")
FamyCH_AnalyticalSample <- FamyCH_w1 %>%
  filter(as.character(HOUSEHOLD_ID) %in% cc_household_ids) %>%
  mutate(across(where(is.factor), droplevels))
dat_common <- FamyCH_AnalyticalSample %>%
  select(CA40, all_of(var_order)) %>%
  apply_study_levels() %>% droplevels()
N_common <- nrow(dat_common)
stopifnot(N_common == length(cc_household_ids))
readr::write_csv(tibble::tibble(HOUSEHOLD_ID = cc_household_ids),
                 file.path(output_dir, "Common_complete_case_household_IDs.csv"))
cat("Common complete-case sample for bivariate and adjusted analyses: N =", N_common, "\n")
print(table(dat_common$CA40))

# Helper functions
format_p <- function(p) {
  vapply(p, function(x) {
    if (is.na(x)) return("")
    if (x < .001) return("p < 0.001")
    paste0("p = ", sprintf("%.3f", x))
  }, character(1))
}
format_ame <- function(estimate, conf_low, conf_high, p_value) {
  ifelse(is.na(estimate), "", paste0(
    sprintf("%.3f [%.3f, %.3f]", estimate, conf_low, conf_high),
    "\n", format_p(p_value)
  ))
}

detect_outcome <- function(x) {
  out <- intersect(
    c("group", "outcome", "response", "y.level",
      "level", "class", "eq"),
    names(x)
  )
  
  if (!length(out)) {
    stop("Outcome column not found in avg_slopes().")
  }
  
  out[1]
}

clean_contrast <- function(x) {
  
  x <- as.character(x)
  
  # Remove the text following the contrast separator
  x <- sub(" - .*", "", x)
  
  # Cleaning
  x <- gsub("^[[:punct:]_]+", "", x)
  x <- gsub("_", " ", x)
  
  trimws(x)
}

# Check categorical contrasts before assigning table labels. Each category must
# be compared with the fitted factor's actual first level for every outcome.
validate_ame_contrasts <- function(results, data, variable, outcome_column) {
  if (!is.factor(data[[variable]])) return(invisible(TRUE))
  lev <- levels(droplevels(data[[variable]]))
  expected <- paste(lev[-1], "-", lev[1])
  if (!"contrast" %in% names(results) ||
      !setequal(unique(as.character(results$contrast)), expected)) {
    stop("Unexpected AME contrasts for ", variable,
         ". Expected: ", paste(expected, collapse = " | "),
         ". Check factor references before reporting results.")
  }
  for (contrast in expected) {
    observed <- as.character(results[[outcome_column]][results$contrast == contrast])
    if (anyDuplicated(observed) || !setequal(observed, levels(data$CA40))) {
      stop("Missing or duplicated outcome-specific AMEs for ", variable, ": ", contrast)
    }
  }
  invisible(TRUE)
}

assert_estimable_results <- function(results, stage) {
  failed <- grepl("__(NA|ERROR|NOT_ESTIMABLE|MODEL_ERROR|AME_ERROR|INCONSISTENT_LEVELS)$",
                  results$RowID)
  if (any(failed)) {
    stop(stage, " did not produce all requested results: ",
         paste(unique(results$Term[failed]), collapse = "; "),
         ". Inspect the model messages above before interpreting the table.")
  }
  invisible(TRUE)
}

# Bivariate model and average marginal effects
run_biv_ame <- function(var) {
  dat_common <- dat_common[complete.cases(dat_common[, c("CA40", var)]), , drop = FALSE]
  dat_common <- droplevels(dat_common)
  
  lab <- labels_vec[[var]]
  x <- dat_common[[var]]
  outcomes <- levels(dat_common$CA40)
  
  if (n_distinct(x) < 2) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__NA"),
        Term = paste0(lab, " — Not estimable"),
        Outcome = outcomes,
        AME = ""
      )
    )
  }
  
  model <- tryCatch(
    multinom(
      reformulate(var, response = "CA40"),
      data = dat_common,
      trace = FALSE,
      Hess = TRUE, maxit = 1000, MaxNWts = 10000, model = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(model) || model$convergence != 0) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__ERROR"),
        Term = paste0(lab, " — Model error"),
        Outcome = outcomes,
        AME = ""
      )
    )
  }
  
  ame <- tryCatch(
    avg_slopes(model, variables = var, type = "probs"),
    error = function(e) NULL
  )
  
  if (is.null(ame)) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__AME_ERROR"),
        Term = paste0(lab, " — AME error"),
        Outcome = outcomes,
        AME = ""
      )
    )
  }
  
  outcol <- detect_outcome(ame)
  
  td <- ame %>%
    as.data.frame() %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Outcome = as.character(.data[[outcol]]),
      AME = format_ame(estimate, conf.low, conf.high, p.value)
    )
  
  validate_ame_contrasts(td, dat_common, var, outcol)
  
  # Continuous predictor
  if (var %in% numeric_vars) {
    return(
      td %>%
        transmute(
          Variable = var,
          RowType = "single",
          RowOrder = 1,
          RowID = paste0(var, "__S"),
          Term = lab,
          Outcome,
          AME
        ) %>%
        distinct()
    )
  }
  
  # Categorical predictor
  ref <- levels(droplevels(x))[1]
  
  header <- tibble(
    Variable = var,
    RowType = "header",
    RowOrder = 1,
    RowID = paste0(var, "__H"),
    Term = lab,
    Outcome = outcomes,
    AME = ""
  )
  
  # Reference row with empty AME cells
  reference <- tibble(
    Variable = var,
    RowType = "reference",
    RowOrder = 2,
    RowID = paste0(var, "__REF"),
    Term = paste0(ref, " (ref.)"),
    Outcome = outcomes,
    AME = ""
  )
  
  contrasts <- td %>%
    mutate(
      Term = clean_contrast(contrast),
      RowOrder = 2L + match(Term, levels(x)[-1]),
      RowID = paste0(var, "__", make.names(Term))
    ) %>%
    transmute(
      Variable = var,
      RowType = "level",
      RowOrder,
      RowID,
      Term,
      Outcome,
      AME
    ) %>%
    distinct()
  
  bind_rows(header, reference, contrasts)
}

# Run models and reshape results to wide format
ame_long <- map_dfr(var_order, run_biv_ame) %>%
  mutate(
    Variable = factor(Variable, levels = var_order),
    Outcome = factor(Outcome, levels = levels(dat_common$CA40)),
    RowType = factor(
      RowType,
      levels = c("header", "single", "reference", "level")
    )
  )

ame_long <- add_model_sample_counts(ame_long, function(variable) {
  required <- c("CA40", variable)
  dat_common[complete.cases(dat_common[, required, drop = FALSE]), , drop = FALSE]
})
bivariate_headers <- custody_header_labels(ame_long)
assert_estimable_results(ame_long, "Bivariate analysis")

ame_table <- ame_long %>%
  arrange(Variable, RowOrder, RowType, RowID, Outcome) %>%
  pivot_wider(
    id_cols = c(Variable, RowType, RowOrder, RowID, Term, N),
    names_from = Outcome,
    values_from = AME,
    values_fill = "",
    values_fn = first
  ) %>%
  arrange(Variable, RowOrder, RowType, RowID)

row_info <- ame_table %>%
  select(Variable, RowType, RowOrder, RowID, Term)

ame_display <- ame_table %>%
  mutate(N_display = if_else(RowType %in% c("header", "single"), as.character(N), "")) %>%
  select(Term, N = N_display, `Mother LPC`, `Unequal SPC`, `Equal SPC`)

# Flextable
var_rows <- which(row_info$RowType %in% c("header", "single"))
cat_rows <- which(row_info$RowType %in% c("reference", "level"))
ref_rows <- which(row_info$RowType == "reference")

apa_border <- fp_border(color = "black", width = 1)

ft_ame <- flextable(ame_display) %>%
  set_header_labels(Term = "Independent variables", N = "N") %>%
  align(j = 1, align = "left", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  padding(
    padding.top = 2.5,
    padding.bottom = 2.5,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  ) %>%
  line_spacing(space = 1.05, part = "all")

if (ncol(ame_display) >= 2) {
  ft_ame <- align(
    ft_ame,
    j = 2:ncol(ame_display),
    align = "center",
    part = "all"
  )
}

if (length(var_rows)) {
  ft_ame <- bold(
    ft_ame,
    i = var_rows,
    j = 1,
    part = "body"
  )
}

if (length(cat_rows)) {
  ft_ame <- padding(
    ft_ame,
    i = cat_rows,
    j = 1,
    padding.left = 12,
    part = "body"
  )
}

if (length(ref_rows)) {
  ft_ame <- italic(
    ft_ame,
    i = ref_rows,
    j = 1:ncol(ame_display),
    part = "body"
  )
}

ft_ame <- ft_ame %>%
  add_footer_lines(
    paste0(
      "Average marginal effects on the probability of each custody arrangement. ",
      "AME [95% CI]. P-values are displayed below estimates (p < 0.001 for smaller values). ",
      "The same complete-case sample is used for all bivariate and adjusted CC models."
    )
  ) %>%
  border_remove() %>%
  hline_top(border = apa_border, part = "all") %>%
  hline(border = apa_border, part = "header") %>%
  hline_bottom(border = apa_border, part = "all") %>%
  autofit() %>%
  set_table_properties(
    layout = "autofit",
    width = 1,
    opts_word = list(split = TRUE)
  ) %>%
  width(j = 1, width = 2.7)

if (ncol(ame_display) >= 2) {
  ft_ame <- width(
    ft_ame,
    j = 2:ncol(ame_display),
    width = 1.05
  )
}

ft_bivariate <- ft_ame %>%
  flextable::set_header_labels(values = bivariate_headers) %>%
  flextable::set_caption(
    "Table. Bivariate associations between characteristics and custody arrangements."
  )
ft_bivariate <- flextable::add_footer_lines(ft_bivariate,
                                            "N is the common complete-case sample size. Custody-column counts are identical across variables and across bivariate and adjusted CC models.")


ft_bivariate

# ------------------------------------------------------------
# Adjusted complete-case analysis -----------------------------------------
# ------------------------------------------------------------
# ADJUSTED MULTINOMIAL MODELS
# Outcome: CA40
# One focal predictor per model
# Predictor-specific covariates based on DAGs
# Common complete-case sample across models
# Results: AME [95% CI]
# Separated families only
# Exact p-value
# ------------------------------------------------------------
# Labels
# ------------------------------------------------------------

labels_vec <- c(
  Age_Child_AtSep = "Child's age at separation",
  Reference_child_Gender = "Child's gender",
  Age_Mother_AtSep = "Mother's age at separation",
  Age_Father_AtSep = "Father's age at separation",
  Country_birth_2 = "Parents' migration background",
  Education_AtSep_Mother = "Mother's education at separation",
  EmploymentBeforeSD_Mother = "Mother's employment before separation",
  EmploymentBeforeSD_Father = "Father's employment before separation",
  PreviousSD_WM_final = "Previous separations",
  parental_issues_WM = "Parental adversities",
  Type_Union_final = "Type of union",
  Conflict_Before_Sep_Final = "Conflict before separation",
  Implication_Parents_Past_WM = "Division of childcare before separation",
  Violence_Addictions_YN = "Reason for separation: Violence or addictions",
  Infidelity_YN = "Reason for separation: Infidelity",
  Parental_disagreement_YN = "Reason for separation: Parental disagreement",
  Conjugal_conflict_YN = "Reason for separation: Conjugal conflict",
  Incompatibility_YN = "Reason for separation: Incompatibility",
  Decision_CA_2 = "Custody agreement",
  Language_Region = "Language region"
)

# ------------------------------------------------------------
# Numeric variables
# ------------------------------------------------------------

numeric_vars <- c(
  "Age_Child_AtSep",
  "Age_Mother_AtSep",
  "Age_Father_AtSep",
  "Conflict_Before_Sep_Final"
)

# ------------------------------------------------------------
# Predictor order
# ------------------------------------------------------------

var_order <- c(
  "Age_Child_AtSep",
  "Education_AtSep_Mother",
  "EmploymentBeforeSD_Mother",
  "EmploymentBeforeSD_Father",
  "PreviousSD_WM_final",
  "parental_issues_WM",
  "Type_Union_final",
  "Conflict_Before_Sep_Final",
  "Implication_Parents_Past_WM",
  "Violence_Addictions_YN",
  "Infidelity_YN",
  "Parental_disagreement_YN",
  "Conjugal_conflict_YN",
  "Incompatibility_YN",
  "Decision_CA_2"
)

# ------------------------------------------------------------
# Predictor-specific covariates
# ------------------------------------------------------------

model_controls <- list(
  Age_Child_AtSep = c(
    "Age_Mother_AtSep", "Age_Father_AtSep",
    "Country_birth_2", "Language_Region"
  ),
  
  Education_AtSep_Mother = c(
    "Age_Mother_AtSep", "Age_Father_AtSep",
    "Country_birth_2", "Language_Region"
  ),
  
  EmploymentBeforeSD_Mother = c(
    "Age_Child_AtSep", "Age_Mother_AtSep", "Age_Father_AtSep",
    "Country_birth_2", "Education_AtSep_Mother",
    "Type_Union_final", "Language_Region"
  ),
  
  EmploymentBeforeSD_Father = c(
    "Age_Child_AtSep", "Age_Mother_AtSep", "Age_Father_AtSep",
    "Country_birth_2", "Education_AtSep_Mother",
    "Type_Union_final", "Language_Region"
  ),
  
  PreviousSD_WM_final = c(
    "Age_Mother_AtSep", "Age_Father_AtSep",
    "Country_birth_2", "Education_AtSep_Mother",
    "Language_Region"
  ),
  
  parental_issues_WM = c(
    "Age_Mother_AtSep", "Age_Father_AtSep",
    "Country_birth_2", "Education_AtSep_Mother",
    "Language_Region"
  ),
  
  Type_Union_final = c(
    "Age_Mother_AtSep", "Age_Father_AtSep",
    "PreviousSD_WM_final", "Country_birth_2",
    "Education_AtSep_Mother", "Language_Region"
  ),
  
  Conflict_Before_Sep_Final = c(
    "Age_Mother_AtSep", "Age_Father_AtSep",
    "Education_AtSep_Mother", "PreviousSD_WM_final",
    "Type_Union_final", "Country_birth_2",
    "Language_Region"
  ),
  
  Implication_Parents_Past_WM = c(
    "Age_Child_AtSep", "Reference_child_Gender",
    "Age_Mother_AtSep", "Age_Father_AtSep",
    "Country_birth_2", "Education_AtSep_Mother",
    "parental_issues_WM", "Type_Union_final",
    "Language_Region"
  ),
  
  Violence_Addictions_YN = c(
    "Age_Child_AtSep", "Age_Mother_AtSep",
    "Age_Father_AtSep", "Country_birth_2",
    "Education_AtSep_Mother", "PreviousSD_WM_final",
    "Type_Union_final", "Language_Region"
  ),
  
  Infidelity_YN = c(
    "Age_Child_AtSep", "Age_Mother_AtSep",
    "Age_Father_AtSep", "Country_birth_2",
    "Education_AtSep_Mother", "PreviousSD_WM_final",
    "Type_Union_final", "Language_Region"
  ),
  
  Parental_disagreement_YN = c(
    "Age_Child_AtSep", "Age_Mother_AtSep",
    "Age_Father_AtSep", "Country_birth_2",
    "Education_AtSep_Mother", "PreviousSD_WM_final",
    "Type_Union_final", "Language_Region"
  ),
  
  Conjugal_conflict_YN = c(
    "Age_Child_AtSep", "Age_Mother_AtSep",
    "Age_Father_AtSep", "Country_birth_2",
    "Education_AtSep_Mother", "PreviousSD_WM_final",
    "Type_Union_final", "Language_Region"
  ),
  
  Incompatibility_YN = c(
    "Age_Child_AtSep", "Age_Mother_AtSep",
    "Age_Father_AtSep", "Country_birth_2",
    "Education_AtSep_Mother", "PreviousSD_WM_final",
    "Type_Union_final", "Language_Region"
  ),
  
  Decision_CA_2 = c(
    "Age_Child_AtSep", "Age_Mother_AtSep",
    "Age_Father_AtSep", "Country_birth_2",
    "Education_AtSep_Mother", "Type_Union_final",
    "parental_issues_WM", "Conflict_Before_Sep_Final",
    "Implication_Parents_Past_WM",
    "Violence_Addictions_YN", "Infidelity_YN",
    "Parental_disagreement_YN", "Conjugal_conflict_YN",
    "Incompatibility_YN", "Language_Region"
  )
)

model_controls <- lapply(model_controls, function(controls) {
  unique(c(controls, "Respondent_Parent_Gender"))
})
stopifnot(identical(names(model_controls), var_order))

# ------------------------------------------------------------
# All model variables
# ------------------------------------------------------------

all_model_vars <- unique(c("CA40", var_order, unlist(model_controls)))

missing_vars <- setdiff(all_model_vars, names(FamyCH_w1))

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "The following variables are missing from FamyCH_w1: ",
      paste(missing_vars, collapse = ", ")
    )
  )
}

factor_vars <- setdiff(all_model_vars, c("CA40", numeric_vars))

# ------------------------------------------------------------
# Common analytical sample
# ------------------------------------------------------------

stopifnot(all(all_model_vars %in% cc_required_vars))
dat_models <- FamyCH_w1 %>%
  filter(as.character(HOUSEHOLD_ID) %in% cc_household_ids) %>%
  select(all_of(all_model_vars)) %>%
  mutate(
    CA40 = droplevels(factor(CA40, ordered = FALSE)),
    CA40 = relevel(CA40, ref = "Mother LPC"),
    
    across(
      all_of(numeric_vars),
      ~ suppressWarnings(
        as.numeric(as.character(haven::zap_labels(.)))
      )
    ),
    
    across(
      all_of(factor_vars),
      ~ droplevels(
        factor(haven::zap_labels(.), ordered = FALSE)
      )
    )
  ) %>%
  mutate(across(where(is.factor), droplevels))

dat_models <- droplevels(apply_study_levels(dat_models))

cc_model_rows <- purrr::map_dfr(var_order, function(variable) {
  required <- unique(c("CA40", variable, model_controls[[variable]]))
  ok <- as.character(FamyCH_w1$HOUSEHOLD_ID) %in% cc_household_ids
  stopifnot(all(complete.cases(FamyCH_w1[ok, required, drop = FALSE])))
  tibble::tibble(Model = variable, HOUSEHOLD_ID = FamyCH_w1$HOUSEHOLD_ID[ok])
})
readr::write_csv(cc_model_rows, file.path(output_dir, "Complete_case_household_IDs_by_model.csv"))
readr::write_csv(cc_model_rows %>% count(Model, name = "N"),
                 file.path(output_dir, "Complete_case_N_by_model.csv"))
N_common <- nrow(dat_models)
stopifnot(N_common == length(cc_household_ids), !anyNA(dat_models))

cat("Shared complete-case sample: N =", N_common, "\n")
print(table(dat_models$CA40, useNA = "ifany"))
print(summary(dat_models$Conflict_Before_Sep_Final))
print(class(dat_models$Conflict_Before_Sep_Final))

# ------------------------------------------------------------
# Reference levels for categorical predictors
# ------------------------------------------------------------

if ("Full-time" %in% levels(dat_models$EmploymentBeforeSD_Mother)) {
  dat_models$EmploymentBeforeSD_Mother <- relevel(
    dat_models$EmploymentBeforeSD_Mother,
    ref = "Full-time"
  )
}

if ("Full-time" %in% levels(dat_models$EmploymentBeforeSD_Father)) {
  dat_models$EmploymentBeforeSD_Father <- relevel(
    dat_models$EmploymentBeforeSD_Father,
    ref = "Full-time"
  )
}

if ("None" %in% levels(dat_models$PreviousSD_WM_final)) {
  dat_models$PreviousSD_WM_final <- relevel(
    dat_models$PreviousSD_WM_final,
    ref = "None"
  )
}

if ("None" %in% levels(dat_models$parental_issues_WM)) {
  dat_models$parental_issues_WM <- relevel(
    dat_models$parental_issues_WM,
    ref = "None"
  )
}

reason_vars <- c(
  "Violence_Addictions_YN",
  "Infidelity_YN",
  "Parental_disagreement_YN",
  "Conjugal_conflict_YN",
  "Incompatibility_YN"
)

for (v in reason_vars) {
  if ("No" %in% levels(dat_models[[v]])) {
    dat_models[[v]] <- relevel(dat_models[[v]], ref = "No")
  }
}

if ("Court decision" %in% levels(dat_models$Decision_CA_2)) {
  dat_models$Decision_CA_2 <- relevel(
    dat_models$Decision_CA_2,
    ref = "Court decision"
  )
}

# ------------------------------------------------------------
# Model formulas
# ------------------------------------------------------------

# Omit constant respondent gender only from the CC adjustment sets.
# Retain the source pool; eligibility is determined within each model.
if (dplyr::n_distinct(dat_models$Respondent_Parent_Gender, na.rm = TRUE) < 2L) {
  model_controls <- lapply(model_controls, setdiff, y = "Respondent_Parent_Gender")
}
cat("Complete-case factor orders and references:\n")
for (nm in c("PreviousSD_WM_final", "Implication_Parents_Past_WM", "Type_Union_final")) {
  cat(nm, ": ", paste(levels(dat_models[[nm]]), collapse = " | "), "\n", sep = "")
}

model_formulas <- imap(
  model_controls,
  ~ reformulate(
    termlabels = c(.y, .x),
    response = "CA40"
  )
)

print(model_formulas)

# ------------------------------------------------------------
# Formatting functions
# ------------------------------------------------------------

# P-values displayed to three decimal places.
# Values below 0.001 are displayed as p < 0.001,
# to avoid displaying p = 0.000.
format_p <- function(p) {
  vapply(
    p,
    function(x) {
      if (is.na(x)) {
        return("")
      }
      
      if (x < .001) {
        return("p < 0.001")
      }
      
      paste0("p = ", sprintf("%.3f", x))
    },
    character(1)
  )
}

# First line: AME [95% CI]
# Second line: p-value
format_ame <- function(estimate, conf_low, conf_high, p_value) {
  ifelse(
    is.na(estimate),
    "",
    paste0(
      sprintf("%.3f [%.3f, %.3f]", estimate, conf_low, conf_high),
      "\n",
      format_p(p_value)
    )
  )
}

# ------------------------------------------------------------
# Estimate one model and its AMEs
# ------------------------------------------------------------

run_adjusted_ame <- function(var) {
  
  controls <- model_controls[[var]]
  required <- unique(c("CA40", var, controls))
  dat_model <- droplevels(dat_models[complete.cases(dat_models[, required]), , drop = FALSE])
  # A constant control cannot be estimated in this model-specific sample.
  controls <- controls[vapply(controls, function(nm) dplyr::n_distinct(dat_model[[nm]]) > 1L, logical(1))]
  N_model <- nrow(dat_model)
  outcomes <- levels(dat_model$CA40)
  variable_label <- labels_vec[[var]]
  
  if (
    n_distinct(dat_model$CA40) < 2 ||
    n_distinct(dat_model[[var]]) < 2
  ) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__NOT_ESTIMABLE"),
        Term = paste0(variable_label, " — Not estimable"),
        Outcome = outcomes,
        AME = "",
        N = N_model
      )
    )
  }
  
  fml <- reformulate(
    termlabels = c(var, controls),
    response = "CA40"
  )
  
  model <- tryCatch(
    multinom(
      formula = fml,
      data = dat_model,
      trace = FALSE,
      Hess = TRUE, maxit = 1000, MaxNWts = 10000, model = TRUE
    ),
    error = function(e) {
      message("Model error for ", var, ": ", e$message)
      NULL
    }
  )
  
  if (is.null(model) || model$convergence != 0) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__MODEL_ERROR"),
        Term = paste0(variable_label, " — Model error"),
        Outcome = outcomes,
        AME = "",
        N = N_model
      )
    )
  }
  
  ame <- tryCatch(
    avg_slopes(
      model,
      variables = var,
      type = "probs"
    ),
    error = function(e) {
      message("AME error for ", var, ": ", e$message)
      NULL
    }
  )
  
  if (is.null(ame)) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__AME_ERROR"),
        Term = paste0(variable_label, " — AME error"),
        Outcome = outcomes,
        AME = "",
        N = N_model
      )
    )
  }
  
  outcome_col <- detect_outcome(ame)
  
  ame_df <- ame %>%
    as.data.frame() %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Outcome = as.character(.data[[outcome_col]]),
      AME = format_ame(
        estimate,
        conf.low,
        conf.high,
        p.value
      )
    )
  
  validate_ame_contrasts(ame_df, dat_model, var, outcome_col)
  
  # Continuous predictor
  if (var %in% numeric_vars) {
    return(
      ame_df %>%
        transmute(
          Variable = var,
          RowType = "single",
          RowOrder = 1,
          RowID = paste0(var, "__CONTINUOUS"),
          Term = variable_label,
          Outcome,
          AME,
          N = N_model
        ) %>%
        distinct()
    )
  }
  
  # Categorical predictor
  reference_level <- levels(dat_model[[var]])[1]
  
  header_row <- tibble(
    Variable = var,
    RowType = "header",
    RowOrder = 1,
    RowID = paste0(var, "__HEADER"),
    Term = variable_label,
    Outcome = outcomes,
    AME = "",
    N = N_model
  )
  
  reference_row <- tibble(
    Variable = var,
    RowType = "reference",
    RowOrder = 2,
    RowID = paste0(var, "__REFERENCE"),
    Term = paste0(reference_level, " (ref.)"),
    Outcome = outcomes,
    AME = "",
    N = N_model
  )
  
  contrast_rows <- ame_df %>%
    mutate(
      Term = if ("contrast" %in% names(ame_df)) {
        clean_contrast(contrast)
      } else {
        clean_contrast(term)
      },
      RowOrder = 2L + match(Term, levels(dat_model[[var]])[-1]),
      RowID = paste0(var, "__", make.names(Term))
    ) %>%
    transmute(
      Variable = var,
      RowType = "level",
      RowOrder,
      RowID,
      Term,
      Outcome,
      AME,
      N = N_model
    ) %>%
    distinct()
  
  bind_rows(header_row, reference_row, contrast_rows)
}

# ------------------------------------------------------------
# Estimate all models
# ------------------------------------------------------------

ame_adjusted_long <- map_dfr(var_order, run_adjusted_ame) %>%
  mutate(
    Variable = factor(Variable, levels = var_order),
    Outcome = factor(
      Outcome,
      levels = c("Mother LPC", "Unequal SPC", "Equal SPC")
    ),
    RowType = factor(
      RowType,
      levels = c("header", "single", "reference", "level")
    )
  )

# ------------------------------------------------------------
# Check common sample size
# ------------------------------------------------------------

model_N <- ame_adjusted_long %>%
  group_by(Variable) %>%
  summarise(N = first(N), .groups = "drop")

print(model_N)

if (n_distinct(model_N$N) != 1) {
  message("Model-specific sample sizes are reported in model_N.")
} else {
  cat(
    "All models use the same sample: N =",
    unique(model_N$N),
    "\n"
  )
}

# ------------------------------------------------------------
# Wide-format results
# ------------------------------------------------------------

ame_adjusted_long <- add_model_sample_counts(ame_adjusted_long, function(variable) {
  required <- unique(c("CA40", variable, model_controls[[variable]]))
  dat_models[complete.cases(dat_models[, required, drop = FALSE]), , drop = FALSE]
})
cc_headers <- custody_header_labels(ame_adjusted_long)
assert_estimable_results(ame_adjusted_long, "Adjusted analysis")

ame_adjusted_table <- ame_adjusted_long %>%
  arrange(Variable, RowOrder, RowType, RowID, Outcome) %>%
  pivot_wider(
    id_cols = c(
      Variable, RowType, RowOrder,
      RowID, Term, N
    ),
    names_from = Outcome,
    values_from = AME,
    values_fill = "",
    values_fn = first
  ) %>%
  arrange(Variable, RowOrder, RowType, RowID) %>%
  mutate(
    N_display = if_else(
      RowType %in% c("header", "single"),
      as.character(N),
      ""
    )
  )

row_info <- ame_adjusted_table %>%
  select(Variable, RowType, RowOrder, RowID, Term, N)

ame_adjusted_display <- ame_adjusted_table %>%
  select(
    Term,
    N = N_display,
    `Mother LPC`,
    `Unequal SPC`,
    `Equal SPC`
  )

# ------------------------------------------------------------
# Notes
# ------------------------------------------------------------

general_note <- paste0(
  "Note. Average marginal effects on the probability of each custody ",
  "arrangement are reported with 95% confidence intervals. ",
  "P-values are displayed below each estimate. ",
  "Intact families were excluded. ",
  "The same complete-case sample is used for all bivariate and adjusted CC models; N is shown for each variable."
)

control_note_short <- paste0(
  "Controls included, as applicable, child and parental characteristics, ",
  "socioeconomic factors, family history, pre-separation family functioning, ",
  "reasons for separation, and language region."
)

# ------------------------------------------------------------
# Flextable
# ------------------------------------------------------------

var_rows <- which(row_info$RowType %in% c("header", "single"))
category_rows <- which(row_info$RowType %in% c("reference", "level"))
reference_rows <- which(row_info$RowType == "reference")

apa_border <- fp_border(color = "black", width = 1)

ft_adjusted <- flextable(ame_adjusted_display) %>%
  set_header_labels(
    Term = "Independent variables",
    N = "N",
    `Mother LPC` = "Mother LPC",
    `Unequal SPC` = "Unequal SPC",
    `Equal SPC` = "Equal SPC"
  ) %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:5, align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  padding(
    padding.top = 2.5,
    padding.bottom = 2.5,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  ) %>%
  line_spacing(space = 1.05, part = "all")

if (length(var_rows) > 0) {
  ft_adjusted <- ft_adjusted %>%
    bold(i = var_rows, j = 1, part = "body")
}

if (length(category_rows) > 0) {
  ft_adjusted <- ft_adjusted %>%
    padding(
      i = category_rows,
      j = 1,
      padding.left = 12,
      part = "body"
    )
}

if (length(reference_rows) > 0) {
  ft_adjusted <- ft_adjusted %>%
    italic(i = reference_rows, j = 1:5, part = "body")
}

ft_adjusted <- ft_adjusted %>%
  add_footer_lines(values = c(general_note, control_note_short)) %>%
  fontsize(size = 7, part = "footer") %>%
  align(align = "left", part = "footer") %>%
  border_remove() %>%
  hline_top(border = apa_border, part = "header") %>%
  hline_bottom(border = apa_border, part = "header") %>%
  hline_bottom(border = apa_border, part = "body") %>%
  autofit() %>%
  set_table_properties(
    layout = "autofit",
    width = 1,
    opts_word = list(split = TRUE)
  ) %>%
  width(j = 1, width = 2.75) %>%
  width(j = 2, width = 0.50) %>%
  width(j = 3:5, width = 1.05)

ft_adjusted_cc <- ft_adjusted %>%
  flextable::set_header_labels(values = cc_headers) %>%
  flextable::set_caption(
    "Table A1. Adjusted Average Marginal Effects From Multinomial Logistic Regressions Predicting Custody Arrangements (Complete-Case Analysis)."
  )
ft_adjusted_cc <- flextable::add_footer_lines(ft_adjusted_cc,
                                              "N is the common complete-case sample size. Custody-column counts are identical across variables and across bivariate and adjusted CC models.")


ft_adjusted_cc

# ------------------------------------------------------------
# Multiple imputation ----------------------------------------------------
# ------------------------------------------------------------
# Recompute imputations in every full run to avoid reusing a stale sample.

# ------------------------------------------------------------
# MULTIPLE IMPUTATION WITH MICE
# 50 imputations, separated families only
# ------------------------------------------------------------

# ------------------------------------------------------------
# Variable-name configuration
# ------------------------------------------------------------

v <- list(
  id                    = "HOUSEHOLD_ID",
  custody               = "CA40",
  
  child_age             = "Age_Child_AtSep",
  child_gender          = "Reference_child_Gender",
  respondent_gender     = "Respondent_Parent_Gender",
  mother_age            = "Age_Mother_AtSep",
  father_age            = "Age_Father_AtSep",
  
  education_mother      = "Education_AtSep_Mother",
  employment_mother     = "EmploymentBeforeSD_Mother",
  employment_father     = "EmploymentBeforeSD_Father",
  
  migration             = "Country_birth_2",
  adversities           = "parental_issues_WM",
  union_type            = "Type_Union_final",
  years_since_sep       = "YearsSinceSep_final",
  
  conflict              = "Conflict_Before_Sep_Final",
  childcare             = "Implication_Parents_Past_WM",
  previous_sep          = "PreviousSD_WM_final",
  
  custody_agreement     = "Decision_CA_2",
  language_region       = "Language_Region",
  
  violence              = "Violence_Addictions_YN",
  infidelity            = "Infidelity_YN",
  parental_disagreement = "Parental_disagreement_YN",
  conjugal_conflict     = "Conjugal_conflict_YN",
  incompatibility       = "Incompatibility_YN"
)

# All variables included in imputation
vars_imp <- unique(
  unlist(v, use.names = FALSE)
)

# ------------------------------------------------------------
# Check that all required variables exist
# ------------------------------------------------------------

missing_variables <- setdiff(
  vars_imp,
  names(FamyCH_w1)
)

if (length(missing_variables) > 0) {
  stop(
    "Variables missing from FamyCH_w1: ",
    paste(missing_variables, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Select separated families only
# ------------------------------------------------------------

imputation_eligibility <- FamyCH_w1 %>% transmute(
  HOUSEHOLD_ID, Analysis_Eligible, CA40,
  Known_Invalid_History = Incoherence_Hist_OP %in% 1,
  Eligible_For_Imputation = Analysis_Eligible & !is.na(CA40))
print(imputation_eligibility %>% count(Analysis_Eligible, Known_Invalid_History,
                                       Eligible_For_Imputation), n = Inf)
dat_imp <- FamyCH_w1 %>%
  filter(Analysis_Eligible) %>%
  filter(
    !is.na(.data[[v$custody]]),
    as.character(.data[[v$custody]]) != "Intact"
  ) %>%
  select(all_of(vars_imp)) %>%
  droplevels()

# Align protection flags to the MI input by household ID.
mi_source_rows <- match(as.character(dat_imp[[v$id]]), as.character(FamyCH_w1$HOUSEHOLD_ID))
protected_cells <- matrix(FALSE, nrow(dat_imp), ncol(dat_imp), dimnames = list(NULL, names(dat_imp)))
protected_cells[, v$mother_age] <- FamyCH_w1$Protect_Mother_Age[mi_source_rows]
protected_cells[, v$father_age] <- FamyCH_w1$Protect_Father_Age[mi_source_rows]
# Individual role-specific employment cannot be assigned where that role is unknown.
protected_cells[, v$employment_mother] <- !(FamyCH_w1$Mother_Is_Respondent | FamyCH_w1$Mother_Is_OP)[mi_source_rows]
protected_cells[, v$employment_father] <- !(FamyCH_w1$Father_Is_Respondent | FamyCH_w1$Father_Is_OP)[mi_source_rows]
protected_cells[, v$education_mother] <- is.na(dat_imp[[v$education_mother]]) &
  !(FamyCH_w1$Mother_Is_Respondent | FamyCH_w1$Mother_Is_OP)[mi_source_rows]
src <- FamyCH_w1[mi_source_rows, ]
protected_cells[, v$adversities] <- is.na(dat_imp[[v$adversities]]) & (
  (src$parental_issues %in% "Only respondent" & !(src$Mother_Is_Respondent | src$Father_Is_Respondent)) |
    (src$parental_issues %in% "Only other parent" & !(src$Mother_Is_OP | src$Father_Is_OP)))
protected_cells[, v$childcare] <- is.na(dat_imp[[v$childcare]]) & (
  (src$Implication_Parents_Past %in% "Respondent more" & !(src$Mother_Is_Respondent | src$Father_Is_Respondent)) |
    (src$Implication_Parents_Past %in% "Other parent more" & !(src$Mother_Is_OP | src$Father_Is_OP)))
protected_cells[, v$previous_sep] <- is.na(dat_imp[[v$previous_sep]]) &
  !(src$Mother_Is_Respondent | src$Mother_Is_OP) & !(src$Father_Is_Respondent | src$Father_Is_OP)
protected_cells[, v$child_age] <- FamyCH_w1$Protect_Separation_Date[mi_source_rows]
protected_cells[, v$years_since_sep] <- FamyCH_w1$Protect_Separation_Date[mi_source_rows]
# Keep known final gender unaltered; do not infer gender to repair role exclusions.
for (nm in colnames(protected_cells)) {
  stopifnot(all(is.na(dat_imp[[nm]][protected_cells[, nm]])))
}
where_imp <- is.na(dat_imp) & !protected_cells
where_imp[, c(v$id, v$custody, v$respondent_gender)] <- FALSE

# Check the analytical sample
table(
  dat_imp[[v$custody]],
  useNA = "ifany"
)

nrow(dat_imp)

# ------------------------------------------------------------
# Define categorical variables
# ------------------------------------------------------------

categorical_variables <- c(
  v$custody,
  v$child_gender,
  v$respondent_gender,
  v$migration,
  v$previous_sep,
  v$employment_mother,
  v$employment_father,
  v$adversities,
  v$union_type,
  v$childcare,
  v$violence,
  v$infidelity,
  v$parental_disagreement,
  v$conjugal_conflict,
  v$incompatibility,
  v$custody_agreement,
  v$language_region
)

dat_imp <- dat_imp %>%
  mutate(
    across(
      all_of(categorical_variables),
      ~ droplevels(factor(.x))
    )
  )

dat_imp[[v$education_mother]] <- factor(
  as.character(dat_imp[[v$education_mother]]),
  levels = c("Compulsory school", "Post-compulsory education", "Tertiary education"),
  ordered = TRUE
)
# Preserve ordinal education coding
if (is.ordered(dat_imp[[v$education_mother]])) {
  
  dat_imp[[v$education_mother]] <- droplevels(
    dat_imp[[v$education_mother]]
  )
  
} else {
  
  dat_imp[[v$education_mother]] <- droplevels(
    factor(dat_imp[[v$education_mother]])
  )
}

# ------------------------------------------------------------
# Define continuous variables
# ------------------------------------------------------------

continuous_variables <- c(
  v$child_age,
  v$mother_age,
  v$father_age,
  v$years_since_sep,
  v$conflict
)

# Verify numeric types
non_numeric_variables <- continuous_variables[
  !vapply(
    dat_imp[continuous_variables],
    is.numeric,
    logical(1)
  )
]

if (length(non_numeric_variables) > 0) {
  stop(
    "These variables must be numeric to use PMM: ",
    paste(non_numeric_variables, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Set parental ages below 18 to missing; review this eligibility rule
# ------------------------------------------------------------

parental_age_variables <- c(
  v$mother_age,
  v$father_age
)

# Count values below the specified age threshold
n_parental_ages_below_18 <- vapply(
  dat_imp[parental_age_variables],
  function(x) {
    sum(!is.na(x) & x < 18)
  },
  numeric(1)
)

n_parental_ages_below_18

# Replace parental ages below 18 with NA
dat_imp <- dat_imp %>%
  mutate(
    across(
      all_of(parental_age_variables),
      ~ replace(
        .x,
        !is.na(.x) & .x < 18,
        NA_real_
      )
    )
  )

# ------------------------------------------------------------
# Check gender variables
# ------------------------------------------------------------

gender_variables <- c(
  v$child_gender,
  v$respondent_gender
)

# Gender distributions
lapply(
  dat_imp[gender_variables],
  table,
  useNA = "ifany"
)

# Number of observed levels
n_gender_levels <- vapply(
  dat_imp[gender_variables],
  function(x) {
    length(unique(x[!is.na(x)]))
  },
  integer(1)
)

n_gender_levels

# ------------------------------------------------------------
# Initialize methods and predictor matrix
# ------------------------------------------------------------

dat_imp <- as.data.frame(dat_imp)
meth <- mice::make.method(dat_imp)
pred <- make.predictorMatrix(dat_imp)

# Start with an empty predictor matrix
pred[,] <- 0L

# ------------------------------------------------------------
# Define predictor blocks
# ------------------------------------------------------------

# Initial variables
initial_block <- c(
  v$child_gender,
  v$respondent_gender,
  v$mother_age,
  v$father_age,
  v$child_age
)

# Variables measured before separation
pre_separation_block <- unique(
  c(
    initial_block,
    v$migration,
    v$previous_sep,
    v$employment_mother,
    v$employment_father,
    v$adversities,
    v$union_type,
    v$conflict,
    v$childcare
  )
)

# Reasons for separation
separation_reasons <- c(
  v$violence,
  v$infidelity,
  v$parental_disagreement,
  v$conjugal_conflict,
  v$incompatibility
)

# Variables measured before or at separation
separation_block <- unique(
  c(
    pre_separation_block,
    v$education_mother,
    separation_reasons,
    v$custody_agreement
  )
)

# ------------------------------------------------------------
# Define predictors for each imputation target
# ------------------------------------------------------------

predictor_sets <- list()

# Child's gender
predictor_sets[[v$child_gender]] <- unique(
  c(
    setdiff(
      initial_block,
      v$child_gender
    ),
    v$migration
  )
)

# Respondent parent's gender
predictor_sets[[v$respondent_gender]] <- unique(
  c(
    setdiff(
      initial_block,
      v$respondent_gender
    ),
    v$migration
  )
)

# Parents' migration background
predictor_sets[[v$migration]] <- initial_block

# Previous separations
predictor_sets[[v$previous_sep]] <- unique(
  c(
    initial_block,
    v$migration
  )
)

# Father's employment before separation
predictor_sets[[v$employment_father]] <- setdiff(
  pre_separation_block,
  v$employment_father
)

# Mother's employment before separation
predictor_sets[[v$employment_mother]] <- setdiff(
  pre_separation_block,
  v$employment_mother
)

# Parental adversities
predictor_sets[[v$adversities]] <- setdiff(
  pre_separation_block,
  v$adversities
)

# Type of union
predictor_sets[[v$union_type]] <- setdiff(
  pre_separation_block,
  v$union_type
)

# Conflict before separation
predictor_sets[[v$conflict]] <- setdiff(
  pre_separation_block,
  v$conflict
)

# Division of childcare before separation
predictor_sets[[v$childcare]] <- setdiff(
  pre_separation_block,
  v$childcare
)

# ------------------------------------------------------------
# Variables measured at separation
# ------------------------------------------------------------

separation_targets <- c(
  v$child_age,
  v$mother_age,
  v$father_age,
  v$education_mother,
  separation_reasons,
  v$custody_agreement
)

# Predict each target using the other variables in its block
for (target in separation_targets) {
  
  predictor_sets[[target]] <- setdiff(
    separation_block,
    target
  )
}

# ------------------------------------------------------------
# Variables measured after separation
# ------------------------------------------------------------

# Years since separation
predictor_sets[[v$years_since_sep]] <- unique(
  c(
    separation_block,
    v$language_region
  )
)

# Language region
predictor_sets[[v$language_region]] <- unique(
  c(
    separation_block,
    v$years_since_sep
  )
)

# ------------------------------------------------------------
# Populate the predictor matrix
# ------------------------------------------------------------

# CA40 predicts all imputed variables,
# but CA40 itself is not imputed.

for (target in names(predictor_sets)) {
  
  predictors_target <- unique(
    c(
      predictor_sets[[target]],
      v$custody
    )
  )
  
  # Exclude the target itself
  predictors_target <- setdiff(
    predictors_target,
    target
  )
  
  # Keep only available variables
  predictors_target <- intersect(
    predictors_target,
    colnames(pred)
  )
  
  pred[target, predictors_target] <- 1L
}

# HOUSEHOLD_ID is neither imputed nor used as a predictor
pred[v$id, ] <- 0L
pred[, v$id] <- 0L

# CA40 is not imputed
pred[v$custody, ] <- 0L

# No variable can predict itself
diag(pred) <- 0L

# ------------------------------------------------------------
# Remove constant predictors
# ------------------------------------------------------------

n_observed_levels <- vapply(
  dat_imp,
  function(x) {
    length(unique(x[!is.na(x)]))
  },
  integer(1)
)

constant_predictors <- names(
  n_observed_levels[n_observed_levels < 2]
)

if (length(constant_predictors) > 0) {
  
  message(
    "Constant variables removed from the predictor set: ",
    paste(constant_predictors, collapse = ", ")
  )
  
  pred[, constant_predictors] <- 0L
}

# ------------------------------------------------------------
# Identify variables that cannot be imputed
# ------------------------------------------------------------

incomplete_constant_variables <- names(dat_imp)[
  vapply(
    dat_imp,
    anyNA,
    logical(1)
  ) &
    n_observed_levels < 2
]

if (length(incomplete_constant_variables) > 0) {
  stop(
    "These variables cannot be imputed: they have missing values ",
    "but fewer than two distinct observed values: ",
    paste(
      incomplete_constant_variables,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------
# Define variables excluded from imputation
# ------------------------------------------------------------

# Household ID and outcome
meth[v$id] <- ""
meth[v$custody] <- ""

# Final parental gender is observed for every eligible household; never impute it.
# Observed final parental gender must not be overwritten by imputation.
meth[v$respondent_gender] <- ""

# Disable imputation for complete variables
complete_variables <- names(dat_imp)[
  !vapply(
    dat_imp,
    anyNA,
    logical(1)
  )
]

meth[complete_variables] <- ""

# ------------------------------------------------------------
# Set imputation methods for gender variables
# ------------------------------------------------------------

# Child's gender
if (anyNA(dat_imp[[v$child_gender]])) {
  
  if (n_gender_levels[v$child_gender] == 2) {
    meth[v$child_gender] <- "logreg"
  } else {
    meth[v$child_gender] <- "polyreg"
  }
  
} else {
  
  meth[v$child_gender] <- ""
}

# Respondent parent's final gender is never imputed.
meth[v$respondent_gender] <- ""

# ------------------------------------------------------------
# Use PMM for continuous variables
# ------------------------------------------------------------

for (target in continuous_variables) {
  
  if (anyNA(dat_imp[[target]])) {
    meth[target] <- "pmm"
  } else {
    meth[target] <- ""
  }
}

meth[continuous_variables]

# ------------------------------------------------------------
# Finalize categorical imputation methods
# ------------------------------------------------------------

# Other methods are selected by make.method():
# ------------------------------------------------------------
# logreg: binary factor
# polyreg: nominal factor with more than two levels
# polr: ordered factor with more than two levels
# pmm: continuous variable

meth

# Remove equations for variables excluded from imputation
pred[names(meth)[meth == ""], ] <- 0L

# Reapply restrictions on household ID and CA40
pred[v$id, ] <- 0L
pred[, v$id] <- 0L
pred[v$custody, ] <- 0L

diag(pred) <- 0L

# ------------------------------------------------------------
# Explicit bounds executed by the custom sampler; no unused post expressions.
imputation_bounds <- list(
  Age_Mother_AtSep = c(18, 120), Age_Father_AtSep = c(18, 120),
  Age_Child_AtSep = c(0, 120), YearsSinceSep_final = c(0, Inf)
)

# ------------------------------------------------------------
# Define the imputation visit sequence
# ------------------------------------------------------------

requested_visit_order <- unique(
  c(
    v$child_gender,
    v$respondent_gender,
    v$migration,
    v$previous_sep,
    v$employment_father,
    v$employment_mother,
    v$adversities,
    v$union_type,
    v$conflict,
    v$childcare,
    v$child_age,
    v$mother_age,
    v$father_age,
    v$education_mother,
    separation_reasons,
    v$custody_agreement,
    v$years_since_sep,
    v$language_region
  )
)

# Keep only variables actually imputed
visit <- requested_visit_order[
  meth[requested_visit_order] != ""
]

visit

# ------------------------------------------------------------
# Pre-imputation checks
# ------------------------------------------------------------

# Imputation method and missing count by variable
method_checks <- data.frame(
  Variable = names(meth),
  Method = unname(meth),
  Missing = vapply(
    dat_imp,
    function(x) sum(is.na(x)),
    integer(1)
  ),
  row.names = NULL
)

method_checks

# Gender-specific checks
gender_checks <- data.frame(
  Variable = gender_variables,
  Method = unname(meth[gender_variables]),
  Missing = vapply(
    dat_imp[gender_variables],
    function(x) sum(is.na(x)),
    integer(1)
  ),
  Observed_levels = unname(n_gender_levels),
  row.names = NULL
)

gender_checks

# Predictors of respondent parent's gender
colnames(pred)[
  pred[v$respondent_gender, ] == 1L
]

# Continuous-variable checks
continuous_checks <- data.frame(
  Variable = continuous_variables,
  Method = unname(meth[continuous_variables]),
  Missing = vapply(
    dat_imp[continuous_variables],
    function(x) sum(is.na(x)),
    integer(1)
  ),
  row.names = NULL
)

continuous_checks

# List predictors for each imputed variable
predictor_list <- lapply(
  visit,
  function(target) {
    colnames(pred)[pred[target, ] == 1L]
  }
)

names(predictor_list) <- visit

predictor_list

# Ensure every imputed variable has predictors
targets_without_predictors <- visit[
  rowSums(pred[visit, , drop = FALSE]) == 0
]

if (length(targets_without_predictors) > 0) {
  stop(
    "These imputation targets have no predictors: ",
    paste(targets_without_predictors, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Run 50 imputations
# ------------------------------------------------------------

# FCS using mice's univariate algorithms, grouped by available predictors.
# Protected cells stay NA at every iteration. For each recipient pattern, only
# predictors available in that pattern enter its equation; valid values in that
# same column still predict other recipients. Observed outcomes alone are donors.
# This is a custom FCS schedule, not an unmodified call to mice().
run_available_fcs <- function(data, methods, predictors, where, protected,
                              visit, m = 50L, maxit = 20L, seed = 2026L, bounds = list()) {
  set.seed(seed)
  data <- as.data.frame(data)
  observed <- !is.na(data)
  n <- nrow(data)
  stopifnot(identical(dim(where), dim(protected)), identical(colnames(where), names(data)),
            !anyNA(where), !anyNA(protected), !any(where & protected))
  for (target in intersect(names(bounds), names(data))) {
    val <- data[[target]][observed[, target]]
    if (any(!is.finite(val) | val < bounds[[target]][1] | val > bounds[[target]][2]))
      stop("Observed values outside bounds for ", target, "; correct coding before MI.")
  }
  completed_sets <- vector("list", m)
  trace_mean <- trace_var <- array(NA_real_, c(ncol(data), maxit, m),
                                   dimnames = list(names(data), as.character(seq_len(maxit)), as.character(seq_len(m))))
  pattern_log <- list()
  warning_log <- list()
  check_completed <- function(current) {
    if (any(is.na(as.matrix(current)) & where)) stop("Ordinary imputation cells remain NA.")
    if (any(!is.na(as.matrix(current)) & protected)) stop("A protected cell was filled.")
    for (nm in names(data)) {
      if (!identical(current[[nm]][observed[, nm]], data[[nm]][observed[, nm]]))
        stop("Observed values changed for ", nm)
    }
    invisible(TRUE)
  }
  for (chain in seq_len(m)) {
    current <- data
    for (target in visit) {
      recipients <- which(where[, target])
      donors <- which(observed[, target] & !protected[, target])
      if (!length(recipients)) next
      if (!length(donors)) stop("No observed donors for ", target)
      current[[target]][recipients] <- data[[target]][donors[sample.int(length(donors), length(recipients), replace = TRUE)]]
    }
    for (iteration in seq_len(maxit)) {
      for (target in visit) {
        recipients <- which(where[, target])
        if (!length(recipients)) next
        candidates <- setdiff(names(data)[predictors[target, ] != 0], target)
        available <- !is.na(current[recipients, candidates, drop = FALSE])
        patterns <- if (length(candidates)) apply(available, 1L, paste0, collapse = "") else rep("none", length(recipients))
        for (pattern in unique(patterns)) {
          rr <- recipients[patterns == pattern]
          selected <- candidates[!is.na(unlist(current[rr[1L], candidates, drop = FALSE]))]
          donor_complete <- if (length(selected))
            complete.cases(current[, selected, drop = FALSE]) else rep(TRUE, n)
          donors <- which(observed[, target] & !protected[, target] & donor_complete)
          if (!length(donors)) stop("No eligible donors for ", target, " / pattern ", pattern)
          # A predictor with no observed variation in this donor subset is unestimable.
          selected <- selected[vapply(selected, function(nm)
            dplyr::n_distinct(current[[nm]][donors]) > 1L, logical(1))]
          rows <- c(donors, rr)
          y <- current[[target]][rows]
          ry <- seq_along(rows) <= length(donors)
          wy <- !ry
          if (!length(selected) || dplyr::n_distinct(y[ry]) == 1L) {
            draw <- y[ry][sample.int(sum(ry), sum(wy), replace = TRUE)]
          } else {
            design <- stats::model.matrix(~ ., data = current[rows, selected, drop = FALSE])
            design <- design[, colnames(design) != "(Intercept)", drop = FALSE]
            stopifnot(!anyNA(design))
            imputer <- getExportedValue("mice", paste0("mice.impute.", methods[target]))
            args <- list(y = y, ry = ry, x = design, wy = wy)
            if (methods[target] == "pmm") args$donors <- min(5L, length(donors))
            if (methods[target] %in% c("polyreg", "polr")) args$nnet.MaxNWts <- 10000L
            draw <- tryCatch(withCallingHandlers(do.call(imputer, args),
                                                 warning = function(w) {
                                                   warning_log[[length(warning_log) + 1L]] <<- tibble::tibble(
                                                     Chain = chain, Iteration = iteration, Target = target,
                                                     Message = conditionMessage(w))
                                                   invokeRestart("muffleWarning")
                                                 }), error = function(e)
                                                   stop("FCS failure for ", target, " at chain ", chain,
                                                        ", iteration ", iteration, ": ", conditionMessage(e)))
          }
          if (length(draw) != length(rr) || anyNA(draw)) stop("Invalid draws for ", target)
          if (is.factor(current[[target]])) {
            draw <- as.character(draw)
            if (any(!draw %in% levels(current[[target]]))) stop("Unknown imputed category for ", target)
          } else if (any(!is.finite(draw))) stop("Non-finite imputation for ", target)
          if (target %in% names(bounds) &&
              any(draw < bounds[[target]][1] | draw > bounds[[target]][2]))
            stop("Imputed value outside bounds for ", target)
          current[[target]][rr] <- draw
          {
            pattern_log[[length(pattern_log) + 1L]] <- tibble::tibble(
              Chain = chain, Iteration = iteration, Method = unname(methods[target]),
              Target = target, Recipients = length(rr), Donors = length(donors),
              Predictors = paste(selected, collapse = " | "),
              Marginal_draw = !length(selected) || dplyr::n_distinct(y[ry]) == 1L)
          }
        }
      }
      for (target in visit) {
        z <- current[[target]][where[, target]]
        if (is.factor(z)) z <- as.integer(z)
        trace_mean[target, iteration, chain] <- mean(z)
        trace_var[target, iteration, chain] <- stats::var(z)
      }
      check_completed(current)
    }
    completed_sets[[chain]] <- current
    message("FCS chain ", chain, " / ", m, " finished")
  }
  long <- dplyr::bind_rows(lapply(0:m, function(i) {
    z <- if (i == 0L) data else completed_sets[[i]]
    z$.imp <- i
    z$.id <- as.character(seq_len(n))
    z
  }))
  ans <- mice::as.mids(long, where = where)
  ans$method <- methods
  ans$predictorMatrix <- predictors
  ans$visitSequence <- visit
  ans$iteration <- maxit
  ans$chainMean <- trace_mean
  ans$chainVar <- trace_var
  ans$lastSeedValue <- .Random.seed
  ans$available_predictor_log <- dplyr::bind_rows(pattern_log)
  ans$custom_fcs <- TRUE
  ans$custom_warning_log <- if (length(warning_log)) dplyr::bind_rows(warning_log) else
    tibble::tibble(Chain = integer(), Iteration = integer(), Target = character(), Message = character())
  ans$bounds <- bounds
  ans
}

# Small runtime smoke test: protected values remain NA; observed data do not change.
test_available_fcs <- function() {
  set.seed(12)
  z <- data.frame(x = rnorm(120), age = runif(120, 25, 60),
                  group = factor(sample(c("A", "B"), 120, TRUE)),
                  nominal = factor(sample(c("N1", "N2", "N3"), 120, TRUE)),
                  ordinal = ordered(sample(c("L", "M", "H"), 120, TRUE), levels = c("L", "M", "H")))
  z$age[c(1, 5, 9)] <- NA_real_
  z$group[c(1, 7)] <- NA
  z$nominal[c(1, 8)] <- NA
  z$ordinal[c(1, 10)] <- NA
  protect <- matrix(FALSE, nrow(z), ncol(z), dimnames = list(NULL, names(z)))
  protect[1, "age"] <- TRUE
  wh <- is.na(z) & !protect
  mt <- c(x = "", age = "pmm", group = "logreg", nominal = "polyreg", ordinal = "polr")
  pr <- matrix(1L, ncol(z), ncol(z), dimnames = list(names(z), names(z)))
  diag(pr) <- 0L
  pr["x", ] <- 0L
  test <- suppressMessages(run_available_fcs(z, mt, pr, wh, protect,
                                             c("age", "group", "nominal", "ordinal"), m = 2L, maxit = 2L, seed = 9L,
                                             bounds = list(age = c(18, 120))))
  for (out in mice::complete(test, "all")) {
    stopifnot(is.na(out$age[1]), !anyNA(as.matrix(out)[wh]))
    for (nm in names(z)) stopifnot(identical(out[[nm]][!is.na(z[[nm]])], z[[nm]][!is.na(z[[nm]])]))
  }
  one <- data.frame(id_test = 1:3, x = c(NA_real_, NA_real_, 42))
  pr_one <- matrix(0L, 2, 2, dimnames = list(names(one), names(one)))
  protected_one <- matrix(FALSE, 3, 2, dimnames = list(NULL, names(one)))
  test_one <- suppressMessages(run_available_fcs(one, c(id_test = "", x = "pmm"), pr_one,
                                                 is.na(one), protected_one, "x", m = 2L, maxit = 2L))
  for (out in mice::complete(test_one, action = "all")) {
    stopifnot(all(out$x == 42), identical(out$id_test, one$id_test))
  }
  invisible(TRUE)
}

test_available_fcs()

where_imp[, names(meth)[meth == ""]] <- FALSE
visit <- visit[colSums(where_imp[, visit, drop = FALSE]) > 0]
imp_sep_50 <- run_available_fcs(dat_imp, meth, pred, where_imp, protected_cells,
                                visit, m = 50L, maxit = 20L, seed = 2026L, bounds = imputation_bounds)
readr::write_csv(imp_sep_50$available_predictor_log,
                 file.path(output_dir, "MI_available_predictor_patterns.csv"))

readr::write_csv(imp_sep_50$custom_warning_log, file.path(output_dir, "MI_warnings.csv"))

# All requested cells filled, all exclusions preserved, all observed values unchanged.
for (completed in mice::complete(imp_sep_50, action = "all")) {
  stopifnot(!any(is.na(as.matrix(completed)) & where_imp))
  for (nm in names(dat_imp)) {
    obs <- !is.na(dat_imp[[nm]])
    stopifnot(identical(completed[[nm]][obs], dat_imp[[nm]][obs]))
  }
  for (nm in colnames(protected_cells)) {
    stopifnot(all(is.na(completed[[nm]][protected_cells[, nm]])))
  }
}
saveRDS(imp_sep_50, file.path(output_dir, "imputations_mice.rds"))

# ------------------------------------------------------------
# POST-IMPUTATION CHECKS
# ------------------------------------------------------------
# General checks
# ------------------------------------------------------------

# Number of imputations
imp_sep_50$m

# Methods actually used
imp_sep_50$method

# Predictor matrix actually used
imp_sep_50$predictorMatrix

# Logged imputation events
imp_sep_50$loggedEvents

# Convergence diagnostics
grDevices::pdf(file.path(output_dir, "Imputation_traceplots.pdf"))
tryCatch(print(plot(imp_sep_50)), finally = grDevices::dev.off())

# ------------------------------------------------------------
# Check respondent-gender imputation
# ------------------------------------------------------------

# Imputed values across all completed datasets
imp_sep_50$imp[[v$respondent_gender]]

# Distribution in the first completed dataset
table(
  complete(
    imp_sep_50,
    action = 1
  )[[v$respondent_gender]],
  useNA = "ifany"
)

# ------------------------------------------------------------
# Check imputed parental ages
# ------------------------------------------------------------

minimum_imputed_age <- sapply(
  parental_age_variables,
  function(target) {
    
    imputed_values <- unlist(
      imp_sep_50$imp[[target]],
      use.names = FALSE
    )
    
    if (length(imputed_values) == 0) {
      return(NA_real_)
    }
    
    min(
      imputed_values,
      na.rm = TRUE
    )
  }
)

minimum_imputed_age

stopifnot(
  all(
    is.na(minimum_imputed_age) |
      minimum_imputed_age >= 18
  )
)

# ------------------------------------------------------------
# Create one completed dataset for inspection
# ------------------------------------------------------------

FamyCH_w1_imp1 <- complete(
  imp_sep_50,
  action = 1
)

# Check for remaining missing values in imputed variables
colSums(
  is.na(
    FamyCH_w1_imp1[visit]
  )
)

# ------------------------------------------------------------
# Adjusted imputed analysis -----------------------------------------------
# ------------------------------------------------------------
# ADJUSTED MULTINOMIAL MODELS WITH MULTIPLE IMPUTATION
# Outcome: CA40
# One focal predictor per model
# Predictor-specific covariates based on DAGs
# Common analytical sample across imputations
# Results: AME [95% CI], pooled using Rubin's rules
# Separated families only
# ------------------------------------------------------------
# Labels
# ------------------------------------------------------------

labels_vec <- c(
  Age_Child_AtSep =
    "Child's age at separation",
  Reference_child_Gender =
    "Child's gender",
  Age_Mother_AtSep =
    "Mother's age at separation",
  Age_Father_AtSep =
    "Father's age at separation",
  Country_birth_2 =
    "Parents' migration background",
  Education_AtSep_Mother =
    "Mother's education at separation",
  EmploymentBeforeSD_Mother =
    "Mother's employment before separation",
  EmploymentBeforeSD_Father =
    "Father's employment before separation",
  PreviousSD_WM_final =
    "Previous separations",
  parental_issues_WM =
    "Parental adversities",
  Type_Union_final =
    "Type of union",
  Conflict_Before_Sep_Final =
    "Conflict before separation",
  Implication_Parents_Past_WM =
    "Division of childcare before separation",
  Violence_Addictions_YN =
    "Reason for separation: Violence or addictions",
  Infidelity_YN =
    "Reason for separation: Infidelity",
  Parental_disagreement_YN =
    "Reason for separation: Parental disagreement",
  Conjugal_conflict_YN =
    "Reason for separation: Conjugal conflict",
  Incompatibility_YN =
    "Reason for separation: Incompatibility",
  Decision_CA_2 =
    "Custody agreement",
  Language_Region =
    "Language region"
)

# ------------------------------------------------------------
# Numeric variables
# ------------------------------------------------------------

numeric_vars <- c(
  "Age_Child_AtSep",
  "Age_Mother_AtSep",
  "Age_Father_AtSep",
  "Conflict_Before_Sep_Final"
)

# ------------------------------------------------------------
# Predictor order in the table
# ------------------------------------------------------------

var_order <- c(
  "Age_Child_AtSep",
  "Education_AtSep_Mother",
  "EmploymentBeforeSD_Mother",
  "EmploymentBeforeSD_Father",
  "PreviousSD_WM_final",
  "parental_issues_WM",
  "Type_Union_final",
  "Conflict_Before_Sep_Final",
  "Implication_Parents_Past_WM",
  "Violence_Addictions_YN",
  "Infidelity_YN",
  "Parental_disagreement_YN",
  "Conjugal_conflict_YN",
  "Incompatibility_YN",
  "Decision_CA_2"
)

# ------------------------------------------------------------
# Predictor-specific covariates
# ------------------------------------------------------------

model_controls <- list(
  Age_Child_AtSep = c(
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Language_Region"
  ),
  Education_AtSep_Mother = c(
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Language_Region"
  ),
  EmploymentBeforeSD_Mother = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "Type_Union_final",
    "Language_Region"
  ),
  EmploymentBeforeSD_Father = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "Type_Union_final",
    "Language_Region"
  ),
  PreviousSD_WM_final = c(
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "Language_Region"
  ),
  parental_issues_WM = c(
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "Language_Region"
  ),
  Type_Union_final = c(
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "PreviousSD_WM_final",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "Language_Region"
  ),
  Conflict_Before_Sep_Final = c(
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Education_AtSep_Mother",
    "PreviousSD_WM_final",
    "Type_Union_final",
    "Country_birth_2",
    "Language_Region"
  ),
  Implication_Parents_Past_WM = c(
    "Age_Child_AtSep",
    "Reference_child_Gender",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "parental_issues_WM",
    "Type_Union_final",
    "Language_Region"
  ),
  Violence_Addictions_YN = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "PreviousSD_WM_final",
    "Type_Union_final",
    "Language_Region"
  ),
  Infidelity_YN = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "PreviousSD_WM_final",
    "Type_Union_final",
    "Language_Region"
  ),
  Parental_disagreement_YN = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "PreviousSD_WM_final",
    "Type_Union_final",
    "Language_Region"
  ),
  Conjugal_conflict_YN = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "PreviousSD_WM_final",
    "Type_Union_final",
    "Language_Region"
  ),
  Incompatibility_YN = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "PreviousSD_WM_final",
    "Type_Union_final",
    "Language_Region"
  ),
  Decision_CA_2 = c(
    "Age_Child_AtSep",
    "Age_Mother_AtSep",
    "Age_Father_AtSep",
    "Country_birth_2",
    "Education_AtSep_Mother",
    "Type_Union_final",
    "parental_issues_WM",
    "Conflict_Before_Sep_Final",
    "Implication_Parents_Past_WM",
    "Violence_Addictions_YN",
    "Infidelity_YN",
    "Parental_disagreement_YN",
    "Conjugal_conflict_YN",
    "Incompatibility_YN",
    "Language_Region"
  )
)

# Add respondent parent's gender to every model.
# It is removed below if it has no variation.

model_controls <- lapply(
  model_controls,
  function(controls) {
    unique(c(controls, "Respondent_Parent_Gender"))
  }
)

stopifnot(
  identical(names(model_controls), var_order)
)

# ------------------------------------------------------------
# Check and extract imputations
# ------------------------------------------------------------

if (!inherits(imp_sep_50, "mids")) {
  stop(
    paste0(
      "imp_sep_50 must be a mids object. Detected class: ",
      paste(class(imp_sep_50), collapse = ", ")
    )
  )
}

M <- imp_sep_50$m

bases_imp_raw <- mice::complete(
  imp_sep_50,
  action = "all"
)

long_imp_raw <- mice::complete(
  imp_sep_50,
  action = "long",
  include = FALSE
)

stopifnot(
  length(bases_imp_raw) == M
)

# All candidate variables before
# any removal of respondent parent's gender.

all_candidate_vars <- unique(
  c(
    "CA40",
    var_order,
    unlist(model_controls)
  )
)

missing_vars <- setdiff(
  all_candidate_vars,
  names(long_imp_raw)
)

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "Variables missing from imp_sep_50: ",
      paste(missing_vars, collapse = ", ")
    )
  )
}

factor_candidate_vars <- setdiff(
  all_candidate_vars,
  c("CA40", numeric_vars)
)

# ------------------------------------------------------------
# Type-conversion functions
# ------------------------------------------------------------

to_character_safe <- function(x) {
  if (
    inherits(x, "haven_labelled") ||
    inherits(x, "labelled_spss")
  ) {
    return(
      as.character(
        haven::as_factor(
          x,
          levels = "labels"
        )
      )
    )
  }
  
  as.character(x)
}

to_numeric_safe <- function(x) {
  if (
    inherits(x, "haven_labelled") ||
    inherits(x, "labelled_spss")
  ) {
    x <- haven::zap_labels(x)
  }
  
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  suppressWarnings(
    as.numeric(x)
  )
}

get_levels_safe <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  }
  
  if (
    inherits(x, "haven_labelled") ||
    inherits(x, "labelled_spss")
  ) {
    return(
      levels(
        haven::as_factor(
          x,
          levels = "labels"
        )
      )
    )
  }
  
  x <- as.character(x)
  unique(x[!is.na(x)])
}

# ------------------------------------------------------------
# Common factor levels
# ------------------------------------------------------------

factor_levels_mi <- setNames(
  lapply(
    factor_candidate_vars,
    function(variable) {
      get_levels_safe(
        long_imp_raw[[variable]]
      )
    }
  ),
  factor_candidate_vars
)

# Requested categorical level orders; only variables
# included in factor_candidate_vars are used here.
requested_level_orders <- common_factor_levels

variables_to_order <- intersect(names(requested_level_orders), factor_candidate_vars)
for (variable in variables_to_order) {
  current_levels <- factor_levels_mi[[variable]]
  requested_levels <- requested_level_orders[[variable]]
  if (!setequal(current_levels, requested_levels)) {
    stop(
      "Different levels for ", variable, ". Levels in the imputations: ",
      paste(current_levels, collapse = " | "),
      ". Requested order: ", paste(requested_levels, collapse = " | ")
    )
  }
  factor_levels_mi[[variable]] <- requested_levels
}
print(factor_levels_mi[variables_to_order])

ca40_order <- c(
  "Mother LPC",
  "Unequal SPC",
  "Equal SPC",
  "Intact"
)

ca40_observed <- unique(
  to_character_safe(
    long_imp_raw$CA40
  )
)

ca40_observed <- ca40_observed[
  !is.na(ca40_observed)
]

ca40_levels_initial <- c(
  intersect(ca40_order, ca40_observed),
  setdiff(ca40_observed, ca40_order)
)

if (!"Mother LPC" %in% ca40_levels_initial) {
  stop(
    "The reference category Mother LPC is absent from CA40."
  )
}

# ------------------------------------------------------------
# Prepare each imputed dataset
# ------------------------------------------------------------

prepare_mi_data <- function(data) {
  data %>%
    mutate(
      .row_id_mi = row_number(),
      
      CA40 = factor(
        to_character_safe(CA40),
        levels = ca40_levels_initial,
        ordered = FALSE
      ),
      
      CA40 = stats::relevel(
        CA40,
        ref = "Mother LPC"
      ),
      
      across(
        all_of(numeric_vars),
        to_numeric_safe
      ),
      
      across(
        all_of(factor_candidate_vars),
        ~ factor(
          to_character_safe(.x),
          levels = factor_levels_mi[[cur_column()]],
          ordered = FALSE
        )
      )
    )
}

bases_prepared_all <- lapply(
  bases_imp_raw,
  prepare_mi_data
)

N_initial <- vapply(
  bases_prepared_all,
  nrow,
  integer(1)
)

if (length(unique(N_initial)) != 1) {
  stop(
    "The imputed datasets do not have the same number of rows."
  )
}

cat(
  "Number of imputations:",
  M,
  "\n"
)

cat(
  "Initial observations per imputation:",
  unique(N_initial),
  "\n"
)

# ------------------------------------------------------------
# Check respondent parent's gender
# ------------------------------------------------------------

respondent_gender_varies <- all(
  vapply(
    bases_prepared_all,
    function(data) {
      data_sep <- data[
        !is.na(data$CA40) &
          as.character(data$CA40) != "Intact",
        ,
        drop = FALSE
      ]
      
      dplyr::n_distinct(
        data_sep$Respondent_Parent_Gender,
        na.rm = TRUE
      ) >= 2
    },
    logical(1)
  )
)

if (!respondent_gender_varies) {
  model_controls <- lapply(
    model_controls,
    setdiff,
    y = "Respondent_Parent_Gender"
  )
  
  message(
    paste0(
      "Respondent_Parent_Gender does not vary in every imputation ",
      "and was removed from the adjustment sets."
    )
  )
}

# Model variables retained after this check

all_model_vars <- unique(
  c(
    "CA40",
    var_order,
    unlist(model_controls)
  )
)

factor_vars <- setdiff(
  all_model_vars,
  c("CA40", numeric_vars)
)

# ------------------------------------------------------------
# Retain all eligible families; complete-case eligibility is model-specific.
bases_common <- lapply(bases_prepared_all, function(d) d[, all_model_vars, drop = FALSE])
N_common <- nrow(bases_common[[1]]) # source pool size, not a model's N
N_by_imp <- vapply(bases_common, nrow, integer(1))

model_bases_mi <- function(variable) {
  required <- unique(c("CA40", variable, model_controls[[variable]]))
  # Align rows across imputations for this formula only, never across all formulas.
  ok <- Reduce(`&`, lapply(bases_common, function(d) complete.cases(d[, required, drop = FALSE])))
  if (!any(ok)) stop("No eligible observations for model ", variable)
  lapply(bases_common, function(d) droplevels(d[ok, , drop = FALSE]))
}

# ------------------------------------------------------------
# Harmonize factor levels in the common sample
# ------------------------------------------------------------

factor_levels_common <- setNames(
  lapply(
    factor_vars,
    function(variable) {
      observed <- unique(
        unlist(
          lapply(
            bases_common,
            function(data) {
              as.character(
                data[[variable]]
              )
            }
          ),
          use.names = FALSE
        )
      )
      
      observed <- observed[
        !is.na(observed)
      ]
      
      original_order <- factor_levels_mi[[variable]]
      
      c(
        intersect(original_order, observed),
        setdiff(observed, original_order)
      )
    }
  ),
  factor_vars
)

bases_common <- lapply(
  bases_common,
  function(data) {
    for (variable in factor_vars) {
      data[[variable]] <- factor(
        as.character(data[[variable]]),
        levels = factor_levels_common[[variable]],
        ordered = FALSE
      )
    }
    
    data
  }
)

# Requested reference levels

reference_levels <- c(
  Respondent_Parent_Gender = "Woman",
  EmploymentBeforeSD_Mother = "Full-time",
  EmploymentBeforeSD_Father = "Full-time",
  PreviousSD_WM_final = "None",
  Implication_Parents_Past_WM = "Mother more",
  Type_Union_final = "Cohabiting",
  parental_issues_WM = "None",
  Violence_Addictions_YN = "No",
  Infidelity_YN = "No",
  Parental_disagreement_YN = "No",
  Conjugal_conflict_YN = "No",
  Incompatibility_YN = "No",
  Decision_CA_2 = "Court decision"
)

apply_reference_levels <- function(data) {
  for (variable in intersect(names(reference_levels), names(data))) {
    ref <- unname(
      reference_levels[[variable]]
    )
    
    if (
      is.factor(data[[variable]]) &&
      ref %in% levels(data[[variable]])
    ) {
      data[[variable]] <- stats::relevel(
        data[[variable]],
        ref = ref
      )
    }
  }
  
  data
}

bases_common <- lapply(
  bases_common,
  apply_reference_levels
)

# Check final reference levels and category order.
for (variable in c("EmploymentBeforeSD_Mother",
                   "EmploymentBeforeSD_Father", "PreviousSD_WM_final")) {
  cat(variable, ": ",
      paste(levels(bases_common[[1]][[variable]]), collapse = " | "), "\n")
}

# ------------------------------------------------------------
# Harmonize CA40 levels
# ------------------------------------------------------------

observed_outcomes_by_imp <- lapply(
  bases_common,
  function(data) {
    unique(
      as.character(
        data$CA40[
          !is.na(data$CA40)
        ]
      )
    )
  }
)

same_outcomes <- all(
  vapply(
    observed_outcomes_by_imp,
    function(x) {
      setequal(
        x,
        observed_outcomes_by_imp[[1]]
      )
    },
    logical(1)
  )
)

if (!same_outcomes) {
  stop(
    paste0(
      "Observed CA40 categories are not identical ",
      "across all imputations."
    )
  )
}

expected_outcomes <- c(
  "Mother LPC",
  "Unequal SPC",
  "Equal SPC"
)

outcomes <- c(
  intersect(
    expected_outcomes,
    observed_outcomes_by_imp[[1]]
  ),
  setdiff(
    observed_outcomes_by_imp[[1]],
    expected_outcomes
  )
)

if (!setequal(outcomes, expected_outcomes)) {
  stop(
    paste0(
      "After excluding intact families, CA40 must contain: ",
      paste(expected_outcomes, collapse = ", "),
      ". Observed categories: ",
      paste(outcomes, collapse = ", "),
      "."
    )
  )
}

outcomes <- expected_outcomes

bases_common <- lapply(
  bases_common,
  function(data) {
    data$CA40 <- factor(
      as.character(data$CA40),
      levels = outcomes,
      ordered = FALSE
    )
    
    data$CA40 <- stats::relevel(
      data$CA40,
      ref = "Mother LPC"
    )
    
    data
  }
)

cat(
  "\nCA40 distribution in the first imputation:\n"
)

print(
  table(
    bases_common[[1]]$CA40,
    useNA = "ifany"
  )
)

cat(
  "\nRespondent gender in the first imputation:\n"
)

if ("Respondent_Parent_Gender" %in% names(bases_common[[1]])) {
  print(
    table(
      bases_common[[1]]$Respondent_Parent_Gender,
      useNA = "ifany"
    )
  )
} else {
  cat(
    "Variable removed from the models because it has no variation.\n"
  )
}

# First imputation of the analytical sample,
# for descriptive checks only.

FamyCH_AnalyticalSample_imp1 <- bases_common[[1]]

# ------------------------------------------------------------
# Model formulas
# ------------------------------------------------------------

model_formulas <- purrr::imap(
  model_controls,
  ~ stats::reformulate(
    termlabels = c(.y, .x),
    response = "CA40"
  )
)

print(
  model_formulas
)

# ------------------------------------------------------------
# Formatting functions
# ------------------------------------------------------------

format_p <- function(p) {
  vapply(
    p,
    function(x) {
      if (is.na(x)) return("")
      if (x < .001) return("p < 0.001")
      paste0("p = ", sprintf("%.3f", x))
    },
    character(1)
  )
}

format_ame <- function(estimate, conf_low, conf_high, p_value) {
  ifelse(
    is.na(estimate),
    "",
    paste0(
      sprintf("%.3f [%.3f, %.3f]", estimate, conf_low, conf_high),
      "\n",
      format_p(p_value)
    )
  )
}

detect_outcome_column <- function(x) {
  possible_names <- c(
    "group",
    "outcome",
    "response",
    "y.level",
    "level",
    "class",
    "eq"
  )
  
  detected <- intersect(
    possible_names,
    names(x)
  )
  
  if (length(detected) == 0) {
    stop(
      paste0(
        "No outcome column corresponding to CA40 was detected. ",
        "Available columns: ",
        paste(names(x), collapse = ", ")
      )
    )
  }
  
  detected[1]
}

clean_contrast <- function(x) {
  x <- as.character(x)
  x <- sub(" - .*", "", x)
  x <- gsub("^[[:punct:]_]+", "", x)
  x <- gsub("_", " ", x)
  trimws(x)
}

# ------------------------------------------------------------
# Estimate an adjusted model across M imputations
# and pool its AMEs
# ------------------------------------------------------------

effective_controls_mi <- function(variable, bases) {
  controls <- model_controls[[variable]]
  keep <- vapply(controls, function(nm) all(vapply(bases, function(d)
    dplyr::n_distinct(d[[nm]], na.rm = TRUE) > 1L, logical(1))), logical(1))
  if (any(!keep)) message("Constant controls omitted for ", variable, ": ", paste(controls[!keep], collapse = ", "))
  controls[keep]
}

run_adjusted_ame_mi <- function(var) {
  bases_common <- model_bases_mi(var)
  controls <- effective_controls_mi(var, bases_common)
  variable_label <- labels_vec[[var]]
  N_model <- nrow(bases_common[[1]])
  
  cat(
    "Calculating pooled adjusted AMEs for:",
    var,
    "\n"
  )
  
  # Check variation in the focal predictor and covariates
  # within every imputation.
  
  model_terms <- unique(
    c(var, controls)
  )
  
  term_varies <- setNames(
    vapply(
      model_terms,
      function(term) {
        all(
          vapply(
            bases_common,
            function(data) {
              dplyr::n_distinct(
                data[[term]],
                na.rm = TRUE
              ) >= 2
            },
            logical(1)
          )
        )
      },
      logical(1)
    ),
    model_terms
  )
  
  if (!all(term_varies)) {
    nonvarying_terms <- names(
      term_varies
    )[
      !term_varies
    ]
    
    message(
      "Model not estimable for ",
      var,
      ". Variables with no variation: ",
      paste(nonvarying_terms, collapse = ", ")
    )
    
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__NOT_ESTIMABLE"),
        Term = paste0(variable_label, " — Not estimable"),
        Outcome = outcomes,
        AME = "",
        N = N_model
      )
    )
  }
  
  # Check that each model factor has the same observed levels
  # across all imputations.
  
  factor_terms <- model_terms[
    model_terms %in% factor_vars
  ]
  
  inconsistent_factors <- factor_terms[
    vapply(
      factor_terms,
      function(term) {
        observed_levels <- lapply(
          bases_common,
          function(data) {
            levels(
              droplevels(
                data[[term]]
              )
            )
          }
        )
        
        !all(
          vapply(
            observed_levels,
            function(x) {
              identical(
                x,
                observed_levels[[1]]
              )
            },
            logical(1)
          )
        )
      },
      logical(1)
    )
  ]
  
  if (length(inconsistent_factors) > 0) {
    message(
      "Model not estimable for ",
      var,
      ". Inconsistent levels across imputations for: ",
      paste(inconsistent_factors, collapse = ", ")
    )
    
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__INCONSISTENT_LEVELS"),
        Term = paste0(variable_label, " — Inconsistent categories"),
        Outcome = outcomes,
        AME = "",
        N = N_model
      )
    )
  }
  
  # Formula: focal predictor plus its specific covariates
  
  fml <- stats::reformulate(
    termlabels = c(var, controls),
    response = "CA40"
  )
  
  # Estimate the model within each imputation
  
  models <- lapply(
    bases_common,
    function(data) {
      tryCatch(
        nnet::multinom(
          formula = fml,
          data = data,
          trace = FALSE,
          Hess = TRUE,
          model = TRUE,
          maxit = 1000,
          MaxNWts = 10000
        ),
        error = function(e) {
          message(
            "Model error for ",
            var,
            " : ",
            e$message
          )
          
          NULL
        }
      )
    }
  )
  
  model_failed <- any(
    vapply(
      models,
      function(x) is.null(x) || x$convergence != 0,
      logical(1)
    )
  )
  
  if (model_failed) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__MODEL_ERROR"),
        Term = paste0(variable_label, " — Model error"),
        Outcome = outcomes,
        AME = "",
        N = N_model
      )
    )
  }
  
  # Convert the model list to a mira object.
  
  model_mi <- mice::as.mira(
    models
  )
  
  # avg_slopes() estimates AMEs in each imputation
  # and pools them using Rubin's rules.
  # do.call() passes the predictor name explicitly.
  
  ame_pooled <- tryCatch(
    do.call(
      marginaleffects::avg_slopes,
      args = list(
        model = model_mi,
        variables = as.character(var),
        type = "probs",
        conf_level = 0.95
      )
    ),
    error = function(e) {
      message(
        "Pooled AME calculation error for ",
        var,
        " : ",
        e$message
      )
      
      NULL
    }
  )
  
  if (is.null(ame_pooled)) {
    return(
      tibble(
        Variable = var,
        RowType = "single",
        RowOrder = 1,
        RowID = paste0(var, "__AME_ERROR"),
        Term = paste0(variable_label, " — AME pooling error"),
        Outcome = outcomes,
        AME = "",
        N = N_model
      )
    )
  }
  
  # Prepare the pooled AMEs
  
  ame_df <- as.data.frame(
    ame_pooled
  )
  
  required_result_columns <- c(
    "estimate",
    "conf.low",
    "conf.high",
    "p.value"
  )
  
  missing_result_columns <- setdiff(
    required_result_columns,
    names(ame_df)
  )
  
  if (length(missing_result_columns) > 0) {
    stop(
      paste0(
        "Missing marginaleffects result columns for ",
        var,
        " : ",
        paste(missing_result_columns, collapse = ", ")
      )
    )
  }
  
  outcome_col <- detect_outcome_column(
    ame_df
  )
  
  ame_df <- ame_df %>%
    filter(
      term != "(Intercept)"
    ) %>%
    mutate(
      Outcome = as.character(
        .data[[outcome_col]]
      ),
      AME = format_ame(
        estimate,
        conf.low,
        conf.high,
        p.value
      )
    )
  
  validate_ame_contrasts(ame_df, bases_common[[1]], var, outcome_col)
  
  # Continuous predictor
  
  if (var %in% numeric_vars) {
    return(
      ame_df %>%
        transmute(
          Variable = var,
          RowType = "single",
          RowOrder = 1,
          RowID = paste0(var, "__CONTINUOUS"),
          Term = variable_label,
          Outcome,
          AME,
          N = N_model
        ) %>%
        distinct()
    )
  }
  
  # Categorical predictor
  
  reference_level <- levels(
    droplevels(
      bases_common[[1]][[var]]
    )
  )[1]
  
  nonreference_levels <- setdiff(
    levels(
      droplevels(
        bases_common[[1]][[var]]
      )
    ),
    reference_level
  )
  
  header_row <- tibble(
    Variable = var,
    RowType = "header",
    RowOrder = 1,
    RowID = paste0(var, "__HEADER"),
    Term = variable_label,
    Outcome = outcomes,
    AME = "",
    N = N_model
  )
  
  reference_row <- tibble(
    Variable = var,
    RowType = "reference",
    RowOrder = 2,
    RowID = paste0(var, "__REFERENCE"),
    Term = paste0(reference_level, " (ref.)"),
    Outcome = outcomes,
    AME = "",
    N = N_model
  )
  
  if ("contrast" %in% names(ame_df)) {
    ame_df <- ame_df %>%
      mutate(
        Term = clean_contrast(
          contrast
        )
      )
  } else {
    ame_df <- ame_df %>%
      mutate(
        Term = clean_contrast(
          term
        )
      )
  }
  
  observed_contrasts <- unique(
    ame_df$Term
  )
  
  contrast_levels <- c(
    intersect(
      nonreference_levels,
      observed_contrasts
    ),
    setdiff(
      observed_contrasts,
      nonreference_levels
    )
  )
  
  contrast_rows <- ame_df %>%
    mutate(
      contrast_order = match(
        Term,
        contrast_levels
      ),
      RowOrder = 2 + contrast_order,
      RowID = paste0(
        var,
        "__",
        make.names(Term)
      )
    ) %>%
    transmute(
      Variable = var,
      RowType = "level",
      RowOrder,
      RowID,
      Term,
      Outcome,
      AME,
      N = N_model
    ) %>%
    distinct()
  
  bind_rows(
    header_row,
    reference_row,
    contrast_rows
  )
}

# ------------------------------------------------------------
# Estimate all models
# ------------------------------------------------------------

ame_adjusted_long <- purrr::map_dfr(
  var_order,
  run_adjusted_ame_mi
) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = var_order
    ),
    Outcome = factor(
      Outcome,
      levels = outcomes
    ),
    RowType = factor(
      RowType,
      levels = c(
        "header",
        "single",
        "reference",
        "level"
      )
    )
  ) %>%
  tibble::as_tibble()

# Display all results:

print(
  ame_adjusted_long,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------
# Check the common sample size
# ------------------------------------------------------------

model_N <- ame_adjusted_long %>%
  group_by(
    Variable
  ) %>%
  summarise(
    N = first(N),
    .groups = "drop"
  )

print(
  model_N
)

if (dplyr::n_distinct(model_N$N) != 1) {
  message("Sample sizes vary by model because only required variables determine eligibility.")
} else {
  cat(
    "All models use the same sample: N =",
    unique(model_N$N),
    "in each of the",
    M,
    "imputations.\n"
  )
}

# ------------------------------------------------------------
# Reshape to wide format
# ------------------------------------------------------------

ame_adjusted_long <- add_model_sample_counts(ame_adjusted_long, function(variable) {
  model_bases_mi(variable)[[1]]
})
mi_headers <- custody_header_labels(ame_adjusted_long)
assert_estimable_results(ame_adjusted_long, "Adjusted analysis")

ame_adjusted_table <- ame_adjusted_long %>%
  arrange(
    Variable,
    RowOrder,
    RowType,
    RowID,
    Outcome
  ) %>%
  pivot_wider(
    id_cols = c(
      Variable,
      RowType,
      RowOrder,
      RowID,
      Term,
      N
    ),
    names_from = Outcome,
    values_from = AME,
    values_fill = "",
    values_fn = first
  ) %>%
  arrange(
    Variable,
    RowOrder,
    RowType,
    RowID
  ) %>%
  mutate(
    N_display = if_else(
      RowType %in% c("header", "single"),
      as.character(N),
      ""
    )
  )

row_info <- ame_adjusted_table %>%
  select(
    Variable,
    RowType,
    RowOrder,
    RowID,
    Term,
    N
  )

ame_adjusted_display <- ame_adjusted_table %>%
  select(
    Term,
    N = N_display,
    all_of(outcomes)
  ) %>%
  tibble::as_tibble()

print(
  ame_adjusted_display,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------
# Notes
# ------------------------------------------------------------

general_note <- paste0(
  "Note. Average marginal effects on the probability of each custody ",
  "arrangement are reported with 95% confidence intervals. ",
  "P-values are displayed below each estimate; values below 0.001 are reported as p < 0.001. ",
  "Intact families were excluded. Models were estimated in ",
  M,
  " imputed datasets, and AMEs were combined using Rubin's rules. ",
  "Samples were selected separately for each model and aligned across imputations within that model. N is reported by model."
)

gender_note <- if (respondent_gender_varies) {
  "All models were controlled for the respondent's gender. If there was no variation in a model's analytical sample, this covariate was omitted "
} else {
  paste0(
    "Respondent gender was not included because it did not vary ",
    "in the analytical sample. "
  )
}

control_note_short <- paste0(
  gender_note,
  "Each model was adjusted for the covariates specified in its ",
  "DAG-based adjustment set."
)

# ------------------------------------------------------------
# Flextable
# ------------------------------------------------------------

var_rows <- which(
  row_info$RowType %in% c(
    "header",
    "single"
  )
)

category_rows <- which(
  row_info$RowType %in% c(
    "reference",
    "level"
  )
)

reference_rows <- which(
  row_info$RowType == "reference"
)

apa_border <- officer::fp_border(
  color = "black",
  width = 1
)

ft_adjusted <- flextable::flextable(
  ame_adjusted_display
) %>%
  set_header_labels(
    Term = "Independent variables",
    N = "N",
    `Mother LPC` = "Mother LPC",
    `Unequal SPC` = "Unequal SPC",
    `Equal SPC` = "Equal SPC"
  ) %>%
  font(
    fontname = "Times New Roman",
    part = "all"
  ) %>%
  align(
    j = 1,
    align = "left",
    part = "all"
  ) %>%
  align(
    j = 2:ncol(ame_adjusted_display),
    align = "center",
    part = "all"
  ) %>%
  valign(
    valign = "center",
    part = "all"
  ) %>%
  fontsize(
    size = 8,
    part = "all"
  ) %>%
  padding(
    padding.top = 2.5,
    padding.bottom = 2.5,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  ) %>%
  line_spacing(
    space = 1.05,
    part = "all"
  )

if (length(var_rows) > 0) {
  ft_adjusted <- ft_adjusted %>%
    bold(
      i = var_rows,
      j = 1,
      part = "body"
    )
}

if (length(category_rows) > 0) {
  ft_adjusted <- ft_adjusted %>%
    padding(
      i = category_rows,
      j = 1,
      padding.left = 12,
      part = "body"
    )
}

if (length(reference_rows) > 0) {
  ft_adjusted <- ft_adjusted %>%
    italic(
      i = reference_rows,
      j = 1:ncol(ame_adjusted_display),
      part = "body"
    )
}

ft_adjusted <- ft_adjusted %>%
  add_footer_lines(
    values = c(
      general_note,
      control_note_short
    )
  ) %>%
  fontsize(
    size = 7,
    part = "footer"
  ) %>%
  align(
    align = "left",
    part = "footer"
  ) %>%
  border_remove() %>%
  hline_top(
    border = apa_border,
    part = "header"
  ) %>%
  hline_bottom(
    border = apa_border,
    part = "header"
  ) %>%
  hline_bottom(
    border = apa_border,
    part = "body"
  ) %>%
  autofit() %>%
  set_table_properties(
    layout = "autofit",
    width = 1,
    opts_word = list(
      split = TRUE
    )
  ) %>%
  width(
    j = 1,
    width = 2.75
  ) %>%
  width(
    j = 2,
    width = 0.50
  ) %>%
  width(
    j = 3:ncol(ame_adjusted_display),
    width = 1.05
  )

ft_adjusted_mi <- ft_adjusted %>%
  flextable::set_header_labels(values = mi_headers) %>%
  flextable::set_caption(
    "Table 3. Adjusted Average Marginal Effects From Multinomial Logistic Regressions Predicting Custody Arrangements."
  )
ft_adjusted_mi <- flextable::add_footer_lines(ft_adjusted_mi,
                                              "N is the total model sample for each variable. Custody-group sample sizes appear only in the column headers. A range gives the minimum and maximum across models when their sample sizes differ.")


ft_adjusted_mi

# ------------------------------------------------------------
# Wald comparisons: Equal SPC minus Unequal SPC -----------------------------
# ------------------------------------------------------------

stopifnot(
  length(bases_common) == M,
  identical(names(model_controls), var_order)
)

wald_one_imputation <- function(data, variable, imp_number) {
  controls <- effective_controls_mi(variable, model_bases_mi(variable))
  fml <- stats::reformulate(
    termlabels = c(variable, controls),
    response = "CA40"
  )
  
  model <- nnet::multinom(
    formula = fml,
    data = data,
    trace = FALSE,
    Hess = TRUE,
    model = TRUE,
    maxit = 1000,
    MaxNWts = 10000
  )
  
  if (model$convergence != 0) {
    stop("The Wald-test model did not converge for ", variable,
         " in imputation ", imp_number)
  }
  
  ame <- marginaleffects::avg_slopes(
    model,
    variables = as.character(variable),
    type = "probs"
  )
  
  tab <- as.data.frame(ame)
  outcome_col <- detect_outcome_column(tab)
  
  if (!"contrast" %in% names(tab)) {
    tab$contrast <- ""
  }
  
  tab$contrast[is.na(tab$contrast)] <- ""
  tab$contrast <- as.character(tab$contrast)
  tab[[outcome_col]] <- as.character(tab[[outcome_col]])
  
  # A categorical predictor may have several contrasts
  # against its reference level.
  keys <- unique(
    tab[, c("term", "contrast"), drop = FALSE]
  )
  keys <- keys[
    keys$term != "(Intercept)",
    ,
    drop = FALSE
  ]
  
  purrr::map_dfr(seq_len(nrow(keys)), function(k) {
    
    idx <- which(
      tab$term == keys$term[k] &
        tab$contrast == keys$contrast[k]
    )
    
    i_equal <- idx[
      tab[[outcome_col]][idx] == "Equal SPC"
    ]
    i_unequal <- idx[
      tab[[outcome_col]][idx] == "Unequal SPC"
    ]
    
    if (
      length(i_equal) != 1L ||
      length(i_unequal) != 1L
    ) {
      stop(
        "Missing or duplicated Equal SPC or Unequal SPC AME for ",
        variable,
        "; contrast: ", keys$contrast[k],
        "; imputation: ", imp_number
      )
    }
    
    # The test accounts for covariance between the two AMEs.
    test <- marginaleffects::hypotheses(
      ame,
      hypothesis = sprintf(
        "b%d - b%d = 0",
        i_equal,
        i_unequal
      )
    )
    
    test <- as.data.frame(test)
    
    tibble::tibble(
      .imp = imp_number,
      Variable = variable,
      Term = as.character(keys$term[k]),
      Contrast = as.character(keys$contrast[k]),
      estimate = test$estimate[1],
      std.error = test$std.error[1],
      N_model = nrow(data)
    )
  })
}

# Estimate each contrast separately in all imputations
wald_by_imputation <- purrr::map_dfr(
  var_order,
  function(variable) {
    
    message("Wald test for: ", variable)
    
    purrr::imap_dfr(
      model_bases_mi(variable),
      function(data, imp_number) {
        
        tryCatch(
          wald_one_imputation(
            data,
            variable,
            as.integer(imp_number)
          ),
          error = function(e) {
            stop(
              "Failure for ", variable,
              " in imputation ", imp_number,
              " : ", conditionMessage(e),
              call. = FALSE)})})})

# Pool one contrast using Rubin's rules
pool_one_contrast <- function(x) {
  
  if (
    nrow(x) != M ||
    anyDuplicated(x$.imp) ||
    anyNA(x$estimate) ||
    anyNA(x$std.error) ||
    any(!is.finite(x$estimate)) ||
    any(!is.finite(x$std.error))
  ) {
    stop(
      "A contrast is missing or invalid in ",
      "at least one imputation."
    )
  }
  
  pooled <- mice::pool.scalar(
    Q = x$estimate,
    U = x$std.error^2,
    n = unique(x$N_model),
    k = 1
  )
  
  se <- sqrt(pooled$t)
  df <- pooled$df
  t_wald <- pooled$qbar / se
  critical <- stats::qt(0.975, df = df)
  
  tibble::tibble(
    difference_AME = pooled$qbar,
    std.error = se,
    conf.low = pooled$qbar - critical * se,
    conf.high = pooled$qbar + critical * se,
    t_Wald = t_wald,
    df = df,
    p.value = 2 * stats::pt(
      -abs(t_wald),
      df = df
    ),
    m = nrow(x)
  )
}

wald_pooled <- wald_by_imputation %>%
  dplyr::group_by(
    Variable,
    Term,
    Contrast
  ) %>%
  dplyr::group_modify(
    ~ pool_one_contrast(.x)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    comparison = "Equal SPC - Unequal SPC"
  ) %>%
  dplyr::select(
    Variable,
    Term,
    Contrast,
    comparison,
    difference_AME,
    std.error,
    conf.low,
    conf.high,
    t_Wald,
    df,
    p.value,
    m
  )

print(wald_pooled, n = Inf)

# ------------------------------------------------------------
# Wald table using the AME table structure
# ------------------------------------------------------------

# Match each Wald test to the corresponding row
# in the original AME table.
wald_for_table <- wald_pooled %>%
  dplyr::mutate(
    Variable = as.character(Variable),
    
    Category = dplyr::if_else(
      !is.na(Contrast) & Contrast != "",
      Contrast,
      Term
    ),
    
    RowID = dplyr::if_else(
      Variable %in% numeric_vars,
      paste0(Variable, "__CONTINUOUS"),
      paste0(
        Variable,
        "__",
        make.names(clean_contrast(Category))
      )
    )
  ) %>%
  dplyr::select(
    Variable,
    RowID,
    difference_AME,
    conf.low,
    conf.high,
    p.value
  )

# Check for duplicate test-to-row matches
if (anyDuplicated(wald_for_table[c("Variable", "RowID")])) {
  stop(
    "Multiple Wald tests match the same table row."
  )
}

# Reuse the original adjusted table's rows and order
wald_table <- ame_adjusted_table %>%
  dplyr::mutate(
    Variable = as.character(Variable),
    RowType = as.character(RowType)
  ) %>%
  dplyr::select(
    Variable, RowType, RowOrder, RowID, Term, N_display
  ) %>%
  dplyr::left_join(
    wald_for_table,
    by = c("Variable", "RowID")
  ) %>%
  dplyr::mutate(
    Difference = format_ame(
      estimate = difference_AME,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value
    )
  ) %>%
  dplyr::arrange(
    match(Variable, var_order), RowOrder, RowType, RowID
  )

# Verify that every calculated test appears in the table
unmatched <- dplyr::anti_join(
  wald_for_table,
  wald_table,
  by = c("Variable", "RowID")
)

if (nrow(unmatched) > 0) {
  stop(
    "Some Wald tests could not be matched to a row ",
    "in the original table. Inspect unmatched."
  )
}

wald_display <- wald_table %>%
  dplyr::transmute(
    Term,
    N = N_display,
    `Equal SPC − Unequal SPC` = Difference
  )

# Counts for the compared groups are shown once, in the difference header.
wald_group_counts <- lapply(unique(as.character(wald_table$Variable)), function(variable) {
  model_data <- model_bases_mi(variable)[[1]]
  c(Equal = sum(as.character(model_data$CA40) == "Equal SPC", na.rm = TRUE),
    Unequal = sum(as.character(model_data$CA40) == "Unequal SPC", na.rm = TRUE))
})
wald_group_counts <- do.call(rbind, wald_group_counts)
wald_header <- paste0(
  "Equal SPC − Unequal SPC\nAME difference [95% CI]\n",
  "Equal SPC: N = ", format_header_n(wald_group_counts[, "Equal"]), "\n",
  "Unequal SPC: N = ", format_header_n(wald_group_counts[, "Unequal"])
)
print(wald_display, n = Inf)

# Use the same formatting as the original table
var_rows <- which(
  wald_table$RowType %in% c("header", "single")
)

category_rows <- which(
  wald_table$RowType %in% c("reference", "level")
)

reference_rows <- which(
  wald_table$RowType == "reference"
)

ft_wald <- flextable::flextable(wald_display) %>%
  flextable::set_header_labels(
    Term = "Independent variables",
    N = "N",
    `Equal SPC − Unequal SPC` =
      "AME difference [95% CI]"
  ) %>%
  flextable::font(
    fontname = "Times New Roman",
    part = "all"
  ) %>%
  flextable::fontsize(
    size = 8,
    part = "all"
  ) %>%
  flextable::align(
    j = 1,
    align = "left",
    part = "all"
  ) %>%
  flextable::align(
    j = 2:3,
    align = "center",
    part = "all"
  ) %>%
  flextable::padding(
    padding.top = 2.5,
    padding.bottom = 2.5,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  ) %>%
  flextable::bold(
    i = var_rows,
    j = 1,
    bold = TRUE,
    part = "body"
  ) %>%
  flextable::italic(
    i = reference_rows,
    j = 1,
    italic = TRUE,
    part = "body"
  ) %>%
  flextable::add_footer_lines(
    values = paste0(
      "Note. Difference = AME for Equal SPC minus AME ",
      "for Unequal SPC. 95% confidence intervals are shown ",
      "in brackets. Wald tests were pooled across ",
      M,
      " imputations. P-values are displayed below each estimate; ",
      "values below 0.001 are reported as p < 0.001."
    )
  ) %>%
  flextable::autofit()

ft_wald <- ft_wald %>%
  flextable::set_header_labels(
    values = stats::setNames(wald_header, "Equal SPC − Unequal SPC")
  ) %>%
  flextable::set_caption("Table. Wald tests between Equal and Unequal SPC.") %>%
  flextable::add_footer_lines(
    "Group sample sizes appear only in the header. A range gives the minimum and maximum across models."
  )
ft_wald

# ============================================================
# Bivariate Wald tests: Equal SPC − Unequal SPC
# Multiply imputed data — no control variables
# ============================================================
# Each model includes one predictor only.
# Protected missing values are retained in the completed datasets.
# All models use the same individuals across predictors and imputations.

# ------------------------------------------------------------
# 1. Define predictors and labels
# ------------------------------------------------------------

labels_biv_wald <- c(
  Age_Child_AtSep = "Child's age at separation",
  Reference_child_Gender = "Child's gender",
  Respondent_Parent_Gender = "Respondent parent gender",
  Age_Mother_AtSep = "Mother's age at separation",
  Age_Father_AtSep = "Father's age at separation",
  Country_birth_2 = "Parents' migration background",
  Education_AtSep_Mother = "Mother's education at separation",
  EmploymentBeforeSD_Mother = "Mother's employment before separation",
  EmploymentBeforeSD_Father = "Father's employment before separation",
  PreviousSD_WM_final = "Previous separations",
  parental_issues_WM = "Parental adversities",
  Type_Union_final = "Type of union",
  YearsSinceSep_final = "Years since separation",
  Conflict_Before_Sep_Final = "Conflict before separation",
  Implication_Parents_Past_WM = "Division of childcare before separation",
  Violence_Addictions_YN = "Reason for separation: Violence or addictions",
  Infidelity_YN = "Reason for separation: Infidelity",
  Parental_disagreement_YN = "Reason for separation: Parental disagreement",
  Conjugal_conflict_YN = "Reason for separation: Conjugal conflict",
  Incompatibility_YN = "Reason for separation: Incompatibility",
  Decision_CA_2 = "Custody agreement",
  Language_Region = "Language region"
)

vars_biv_wald <- names(labels_biv_wald)

# ------------------------------------------------------------
# 2. Retrieve the completed datasets
# ------------------------------------------------------------

required_biv_wald <- c("CA40", vars_biv_wald)

missing_columns_biv_wald <- setdiff(
  required_biv_wald,
  names(imp_sep_50$data)
)

if (length(missing_columns_biv_wald) > 0L) {
  stop(
    "Variables absent from the imputation object: ",
    paste(missing_columns_biv_wald, collapse = ", ")
  )
}

bases_biv_wald <- lapply(
  mice::complete(imp_sep_50, action = "all"),
  function(data) {
    
    data <- as.data.frame(data)
    
    # Treat categorical predictors as unordered factors.
    data <- data %>%
      mutate(
        across(
          where(is.factor),
          ~ factor(
            as.character(.x),
            levels = levels(.x),
            ordered = FALSE
          )
        )
      )
    
    # Restore the category order and reference levels
    # specified in the main script.
    data <- apply_study_levels(data)
    
    # Use Woman as the reference for respondent gender.
    gender_values <- as.character(data$Respondent_Parent_Gender)
    gender_levels <- unique(c(
      "Woman", "Man", gender_values[!is.na(gender_values)]
    ))
    data$Respondent_Parent_Gender <- factor(
      gender_values,
      levels = gender_levels,
      ordered = FALSE
    )
    
    data[, required_biv_wald, drop = FALSE]
  }
)

M_biv_wald <- length(bases_biv_wald)

if (M_biv_wald < 2L) {
  stop("At least two imputed datasets are required.")
}

sample_sizes_biv_wald <- vapply(
  bases_biv_wald,
  nrow,
  integer(1)
)

if (length(unique(sample_sizes_biv_wald)) != 1L) {
  stop("Completed datasets contain different numbers of observations.")
}

# Confirm that custody is unchanged across imputations.
custody_biv_first <- as.character(
  bases_biv_wald[[1]]$CA40
)

if (!all(vapply(
  bases_biv_wald,
  function(data) {
    identical(
      as.character(data$CA40),
      custody_biv_first
    )
  },
  logical(1)
))) {
  stop("Custody values or row order differ across imputations.")
}

# ------------------------------------------------------------
# 3. Select one common post-imputation sample
# ------------------------------------------------------------

# Retain the same rows for every model in every imputation.
common_keep_biv_wald <- Reduce(
  `&`,
  lapply(bases_biv_wald, function(data) {
    complete.cases(data[, c("CA40", vars_biv_wald), drop = FALSE])
  })
)
if (!any(common_keep_biv_wald)) {
  stop("The common post-imputation sample is empty.")
}
N_common_biv_wald <- sum(common_keep_biv_wald)
message("Common sample for all bivariate Wald models: N = ",
        N_common_biv_wald, " in each of ", M_biv_wald, " imputations.")

get_biv_wald_bases <- function(variable) {
  
  required <- c("CA40", variable)
  
  # Use the same row mask for every focal predictor.
  keep <- common_keep_biv_wald
  
  if (!any(keep)) {
    stop("No eligible observations for: ", variable)
  }
  
  model_bases <- lapply(
    bases_biv_wald,
    function(data) {
      droplevels(
        data[keep, required, drop = FALSE]
      )
    }
  )
  
  required_custody <- c(
    "Mother LPC",
    "Unequal SPC",
    "Equal SPC"
  )
  
  for (i in seq_along(model_bases)) {
    
    data <- model_bases[[i]]
    x <- data[[variable]]
    
    if (!setequal(
      unique(as.character(data$CA40)),
      required_custody
    )) {
      stop(
        "Missing or unexpected custody category for ",
        variable, " in imputation ", i
      )
    }
    
    if (dplyr::n_distinct(x) < 2L) {
      stop(
        "Predictor has no variation: ",
        variable, " in imputation ", i
      )
    }
    
    if (is.numeric(x) && any(!is.finite(x))) {
      stop(
        "Non-finite predictor values for ",
        variable, " in imputation ", i
      )
    }
  }
  
  # Categorical contrasts must be identical across imputations.
  if (is.factor(model_bases[[1]][[variable]])) {
    
    observed_levels <- lapply(
      model_bases,
      function(data) levels(data[[variable]])
    )
    
    if (!all(vapply(
      observed_levels,
      function(x) identical(x, observed_levels[[1]]),
      logical(1)
    ))) {
      stop(
        "Observed factor levels differ across imputations for: ",
        variable
      )
    }
  }
  
  model_bases
}

# Store the model-specific completed datasets.
biv_wald_model_bases <- setNames(
  lapply(vars_biv_wald, get_biv_wald_bases),
  vars_biv_wald
)

# Sample sizes used in the analyses.
biv_wald_sample_counts <- purrr::map_dfr(
  vars_biv_wald,
  function(variable) {
    
    data <- biv_wald_model_bases[[variable]][[1]]
    
    tibble(
      Variable = variable,
      N = nrow(data),
      N_equal = sum(
        as.character(data$CA40) == "Equal SPC"
      ),
      N_unequal = sum(
        as.character(data$CA40) == "Unequal SPC"
      )
    )
  }
)

stopifnot(
  all(biv_wald_sample_counts$N == N_common_biv_wald),
  dplyr::n_distinct(biv_wald_sample_counts$N_equal) == 1L,
  dplyr::n_distinct(biv_wald_sample_counts$N_unequal) == 1L
)
print(biv_wald_sample_counts, n = Inf)

# ------------------------------------------------------------
# 4. Fit one bivariate model within one imputation
# ------------------------------------------------------------

wald_biv_one_imputation <- function(
    data,
    variable,
    imp_number
) {
  
  # The focal predictor is the only explanatory variable.
  formula_biv <- stats::reformulate(
    termlabels = variable,
    response = "CA40"
  )
  
  model <- nnet::multinom(
    formula = formula_biv,
    data = data,
    na.action = stats::na.fail,
    trace = FALSE,
    Hess = TRUE,
    model = TRUE,
    maxit = 1000,
    MaxNWts = 10000
  )
  
  if (model$convergence != 0) {
    stop(
      "Model did not converge for ",
      variable, " in imputation ", imp_number
    )
  }
  
  if (nrow(model$model) != nrow(data)) {
    stop("Unexpected model sample size for: ", variable)
  }
  
  ame <- marginaleffects::avg_slopes(
    model,
    variables = variable,
    type = "probs"
  )
  
  tab <- as.data.frame(ame)
  
  outcome_col <- detect_outcome_column(tab)
  
  if (!"contrast" %in% names(tab)) {
    tab$contrast <- ""
  }
  
  tab$contrast <- as.character(tab$contrast)
  tab$contrast[is.na(tab$contrast)] <- ""
  
  tab[[outcome_col]] <- as.character(
    tab[[outcome_col]]
  )
  
  keys <- unique(
    tab[, c("term", "contrast"), drop = FALSE]
  )
  
  purrr::map_dfr(seq_len(nrow(keys)), function(k) {
    
    idx <- which(
      tab$term == keys$term[k] &
        tab$contrast == keys$contrast[k]
    )
    
    i_equal <- idx[
      tab[[outcome_col]][idx] == "Equal SPC"
    ]
    
    i_unequal <- idx[
      tab[[outcome_col]][idx] == "Unequal SPC"
    ]
    
    if (length(i_equal) != 1L || length(i_unequal) != 1L) {
      stop(
        "Missing or duplicated AME for ", variable,
        "; contrast: ", keys$contrast[k],
        "; imputation: ", imp_number
      )
    }
    
    # Test the difference while accounting for covariance
    # between the two estimated AMEs.
    test <- marginaleffects::hypotheses(
      ame,
      hypothesis = sprintf(
        "b%d - b%d = 0",
        i_equal,
        i_unequal
      )
    )
    
    test <- as.data.frame(test)
    
    if (
      !is.finite(test$estimate[1]) ||
      !is.finite(test$std.error[1])
    ) {
      stop(
        "Invalid Wald result for ",
        variable, " in imputation ", imp_number
      )
    }
    
    tibble(
      .imp = imp_number,
      Variable = variable,
      Term = as.character(keys$term[k]),
      Contrast = keys$contrast[k],
      estimate = test$estimate[1],
      std.error = test$std.error[1],
      N_model = nrow(data)
    )
  })
}

# ------------------------------------------------------------
# 5. Calculate differences across all imputations
# ------------------------------------------------------------

wald_biv_by_imputation <- purrr::map_dfr(
  vars_biv_wald,
  function(variable) {
    
    message(
      "Bivariate MI Wald test without controls: ",
      variable
    )
    
    purrr::imap_dfr(
      biv_wald_model_bases[[variable]],
      function(data, imp_number) {
        
        wald_biv_one_imputation(
          data = data,
          variable = variable,
          imp_number = as.integer(imp_number)
        )
      }
    )
  }
)

# ------------------------------------------------------------
# 6. Pool the differences using Rubin's rules
# ------------------------------------------------------------

# Use the same scalar-pooling convention as the adjusted
# Wald analysis in the main script.
pool_biv_wald_contrast <- function(x) {
  
  if (
    nrow(x) != M_biv_wald ||
    anyDuplicated(x$.imp) ||
    any(!is.finite(x$estimate)) ||
    any(!is.finite(x$std.error)) ||
    any(x$std.error < 0) ||
    dplyr::n_distinct(x$N_model) != 1L
  ) {
    stop("Missing, inconsistent, or invalid imputation-level results.")
  }
  
  pooled <- mice::pool.scalar(
    Q = x$estimate,
    U = x$std.error^2,
    n = unique(x$N_model),
    k = 1
  )
  
  se <- sqrt(pooled$t)
  df <- pooled$df
  
  if (!is.finite(se) || se <= 0 || is.na(df) || df <= 0) {
    stop("Invalid pooled uncertainty estimate.")
  }
  
  statistic <- pooled$qbar / se
  critical <- stats::qt(0.975, df = df)
  
  tibble(
    difference_AME = pooled$qbar,
    std.error = se,
    conf.low = pooled$qbar - critical * se,
    conf.high = pooled$qbar + critical * se,
    t_Wald = statistic,
    df = df,
    p.value = 2 * stats::pt(
      -abs(statistic),
      df = df
    ),
    m = nrow(x)
  )
}

wald_biv_pooled <- wald_biv_by_imputation %>%
  group_by(Variable, Term, Contrast) %>%
  group_modify(
    ~ pool_biv_wald_contrast(.x)
  ) %>%
  ungroup()

wald_biv_results <- wald_biv_pooled %>%
  left_join(
    biv_wald_sample_counts,
    by = "Variable"
  ) %>%
  mutate(
    Variable_label = unname(
      labels_biv_wald[Variable]
    )
  ) %>%
  arrange(
    match(Variable, vars_biv_wald),
    Contrast
  )

print(wald_biv_results, n = Inf, width = Inf)

# ------------------------------------------------------------
# 7. Build the layout from the bivariate results
# ------------------------------------------------------------
# Continuous predictors in the bivariate analyses
numeric_vars <- c(
  "Age_Child_AtSep",
  "Age_Mother_AtSep",
  "Age_Father_AtSep",
  "YearsSinceSep_final",
  "Conflict_Before_Sep_Final"
)

# Verify their storage type in all completed datasets.
for (variable in numeric_vars) {
  
  is_numeric <- vapply(
    bases_biv_wald,
    function(data) is.numeric(data[[variable]]),
    logical(1)
  )
  
  if (!all(is_numeric)) {
    stop(
      "Expected a numeric predictor in every imputation: ",
      variable
    )
  }
}

# Match estimated contrasts to category labels.
wald_biv_for_table <- wald_biv_results %>%
  mutate(
    Variable = as.character(Variable),
    
    Category = if_else(
      !is.na(Contrast) & Contrast != "",
      Contrast,
      Term
    ),
    
    RowID = if_else(
      Variable %in% numeric_vars,
      paste0(Variable, "__CONTINUOUS"),
      paste0(
        Variable,
        "__",
        make.names(clean_contrast(Category))
      )
    )
  ) %>%
  select(
    Variable,
    RowID,
    difference_AME,
    conf.low,
    conf.high,
    p.value
  )

if (anyDuplicated(
  wald_biv_for_table[c("Variable", "RowID")]
)) {
  stop("Multiple bivariate tests match the same table row.")
}

# Build rows independently of the adjusted Wald table.
wald_biv_structure <- purrr::map_dfr(
  vars_biv_wald,
  function(variable) {
    
    variable_label <- unname(labels_biv_wald[variable])
    
    if (variable %in% numeric_vars) {
      
      return(
        tibble(
          Variable = variable,
          RowType = "single",
          RowOrder = 1L,
          RowID = paste0(variable, "__CONTINUOUS"),
          Term = variable_label
        )
      )
    }
    
    x <- biv_wald_model_bases[[variable]][[1]][[variable]]
    
    if (!is.factor(x)) {
      stop(
        "Categorical predictor is not a factor: ",
        variable
      )
    }
    
    category_levels <- levels(x)
    
    if (length(category_levels) < 2L) {
      stop("Fewer than two categories for: ", variable)
    }
    
    # The first factor level is the reference category.
    header_row <- tibble(
      Variable = variable,
      RowType = "header",
      RowOrder = 1L,
      RowID = paste0(variable, "__HEADER"),
      Term = variable_label
    )
    
    category_rows <- tibble(
      Variable = variable,
      RowType = c(
        "reference",
        rep("level", length(category_levels) - 1L)
      ),
      RowOrder = seq_along(category_levels) + 1L,
      RowID = paste0(
        variable,
        "__",
        make.names(category_levels)
      ),
      Term = c(
        paste0(category_levels[1], " (ref.)"),
        category_levels[-1]
      )
    )
    
    bind_rows(header_row, category_rows)
  }
)

if (anyDuplicated(
  wald_biv_structure[c("Variable", "RowID")]
)) {
  stop("Duplicate category identifiers in the bivariate layout.")
}

# Check that every estimated test matches a table row.
unmatched_biv_tests <- anti_join(
  wald_biv_for_table,
  wald_biv_structure,
  by = c("Variable", "RowID")
)

if (nrow(unmatched_biv_tests) > 0L) {
  print(unmatched_biv_tests)
  stop(
    "Some estimated contrasts could not be matched to category labels. ",
    "Inspect unmatched_biv_tests."
  )
}

# Check that every non-reference category has a test.
missing_biv_tests <- wald_biv_structure %>%
  filter(RowType %in% c("single", "level")) %>%
  anti_join(
    wald_biv_for_table,
    by = c("Variable", "RowID")
  )

if (nrow(missing_biv_tests) > 0L) {
  print(missing_biv_tests)
  stop(
    "Some categories have no corresponding bivariate test. ",
    "Inspect missing_biv_tests."
  )
}

# Join estimates and model-specific sample sizes.
wald_biv_table <- wald_biv_structure %>%
  left_join(
    wald_biv_for_table,
    by = c("Variable", "RowID")
  ) %>%
  left_join(
    biv_wald_sample_counts,
    by = "Variable"
  ) %>%
  arrange(
    match(Variable, vars_biv_wald),
    RowOrder
  ) %>%
  mutate(
    Difference = format_ame(
      estimate = difference_AME,
      conf_low = conf.low,
      conf_high = conf.high,
      p_value = p.value
    ),
    
    Difference = if_else(
      RowType %in% c("header", "reference"),
      "",
      Difference
    ),
    
    N_display = if_else(
      RowType %in% c("header", "single"),
      as.character(N),
      ""
    )
  )

wald_biv_display <- wald_biv_table %>%
  transmute(
    Term,
    N = N_display,
    `Equal SPC − Unequal SPC` = Difference
  )

# ------------------------------------------------------------
# 8. Create the table
# ------------------------------------------------------------

format_biv_header_n <- function(x) {
  
  x <- sort(unique(x))
  
  if (length(x) == 1L) {
    return(as.character(x))
  }
  
  paste0(min(x), "–", max(x))
}

# Custody-specific Ns appear only in the column header.
wald_biv_header <- paste0(
  "Equal SPC − Unequal SPC\n",
  "AME difference [95% CI]\n",
  "Equal SPC: N = ",
  format_biv_header_n(biv_wald_sample_counts$N_equal),
  "\nUnequal SPC: N = ",
  format_biv_header_n(biv_wald_sample_counts$N_unequal)
)

var_rows_biv <- which(
  wald_biv_table$RowType %in% c("header", "single")
)

reference_rows_biv <- which(
  wald_biv_table$RowType == "reference"
)

ft_wald_bivariate <- flextable::flextable(
  wald_biv_display
) %>%
  flextable::set_header_labels(
    Term = "Independent variables",
    N = "N"
  ) %>%
  flextable::set_header_labels(
    values = stats::setNames(
      wald_biv_header,
      "Equal SPC − Unequal SPC"
    )
  ) %>%
  flextable::font(
    fontname = "Times New Roman",
    part = "all"
  ) %>%
  flextable::fontsize(
    size = 8,
    part = "all"
  ) %>%
  flextable::align(
    j = 1,
    align = "left",
    part = "all"
  ) %>%
  flextable::align(
    j = 2:3,
    align = "center",
    part = "all"
  ) %>%
  flextable::padding(
    padding.top = 2.5,
    padding.bottom = 2.5,
    padding.left = 2,
    padding.right = 2,
    part = "all"
  )

if (length(var_rows_biv) > 0L) {
  ft_wald_bivariate <- flextable::bold(
    ft_wald_bivariate,
    i = var_rows_biv,
    j = 1,
    bold = TRUE,
    part = "body"
  )
}

if (length(reference_rows_biv) > 0L) {
  ft_wald_bivariate <- flextable::italic(
    ft_wald_bivariate,
    i = reference_rows_biv,
    j = 1,
    italic = TRUE,
    part = "body"
  )
}

ft_wald_bivariate <- ft_wald_bivariate %>%
  flextable::set_caption(
    "Table. Bivariate Wald tests between Equal and Unequal SPC (Common Sample)."
  ) %>%
  flextable::add_footer_lines(
    values = paste0(
      "Note. Each multinomial model includes one predictor and no controls. ",
      "Difference = AME for Equal SPC minus AME for Unequal SPC. ",
      "95% confidence intervals are shown in brackets. ",
      "Differences were pooled across ", M_biv_wald,
      " imputations using Rubin's rules, accounting for covariance ",
      "between the two AMEs within each imputation. ",
      "P-values are displayed below each estimate; values below 0.001 ",
      "are reported as p < 0.001."
    )
  ) %>%
  flextable::add_footer_lines(
    values = paste0(
      "Analyses use the completed datasets from the final imputation object. ",
      "The same individuals are retained for every predictor in every imputation. ",
      "The common sample requires available custody and all included predictors after imputation. ",
      "Protected missing values remain missing; individuals with remaining missing values ",
      "on any included predictor are excluded from all models. ",
      "N is identical across predictors. Custody-specific Ns appear only in the column header."
    )
  ) %>%
  flextable::autofit()

ft_wald_bivariate

# Bivariate multinomial AMEs from multiply imputed data
# Run after creating/loading imp_sep_50 and the main script's
# apply_study_levels() function. No controls are included.

labels_biv_mi <- c(
  Age_Child_AtSep = "Child's age at separation",
  Reference_child_Gender = "Child's gender",
  Respondent_Parent_Gender = "Respondent parent gender",
  Age_Mother_AtSep = "Mother's age at separation",
  Age_Father_AtSep = "Father's age at separation",
  Country_birth_2 = "Parents' migration background",
  Education_AtSep_Mother = "Mother's education at separation",
  EmploymentBeforeSD_Mother = "Mother's employment before separation",
  EmploymentBeforeSD_Father = "Father's employment before separation",
  PreviousSD_WM_final = "Previous separations",
  parental_issues_WM = "Parental adversities",
  Type_Union_final = "Type of union",
  YearsSinceSep_final = "Years since separation",
  Conflict_Before_Sep_Final = "Conflict before separation",
  Implication_Parents_Past_WM = "Division of childcare before separation",
  Violence_Addictions_YN = "Reason for separation: Violence or addictions",
  Infidelity_YN = "Reason for separation: Infidelity",
  Parental_disagreement_YN = "Reason for separation: Parental disagreement",
  Conjugal_conflict_YN = "Reason for separation: Conjugal conflict",
  Incompatibility_YN = "Reason for separation: Incompatibility",
  Decision_CA_2 = "Custody agreement",
  Language_Region = "Language region"
)

vars_biv_mi <- names(labels_biv_mi)

numeric_biv_mi <- c(
  "Age_Child_AtSep",
  "Age_Mother_AtSep",
  "Age_Father_AtSep",
  "Conflict_Before_Sep_Final",
  "YearsSinceSep_final"
)


outcomes_biv_mi <- c("Mother LPC", "Unequal SPC", "Equal SPC")
required <- c("CA40", vars_biv_mi)
if (!all(required %in% names(imp_sep_50$data))) {
  stop("Missing imputation variables: ", paste(setdiff(required, names(imp_sep_50$data)), collapse = ", "))
}
bases_biv_mi <- lapply(mice::complete(imp_sep_50, "all"), function(d) {
  d <- apply_study_levels(as.data.frame(d), ordered_education = FALSE)
  if (!is.factor(d$Respondent_Parent_Gender) ||
      !identical(levels(d$Respondent_Parent_Gender)[1], "Woman")) {
    stop("Respondent gender must be a factor with Woman as its reference.")
  }
  for (v in numeric_biv_mi) {
    if (!is.numeric(d[[v]])) stop("Expected numeric predictor: ", v)
  }
  d[, required, drop = FALSE]
})
M_biv_mi <- length(bases_biv_mi)
if (M_biv_mi < 2L) stop("At least two imputations are required.")
if (length(unique(vapply(bases_biv_mi, nrow, integer(1)))) != 1L)
  stop("Different numbers of observations across imputations.")
if (!all(vapply(bases_biv_mi, function(d)
  identical(as.character(d$CA40), as.character(bases_biv_mi[[1]]$CA40)), logical(1))))
  stop("Custody or row order differs across imputations.")

# One shared sample across all predictors and all imputations.
common_keep_biv_mi <- Reduce(`&`, lapply(bases_biv_mi, function(d) {
  complete.cases(d[, c("CA40", vars_biv_mi), drop = FALSE])
}))
if (!any(common_keep_biv_mi)) stop("The common post-imputation sample is empty.")
N_common_biv_mi <- sum(common_keep_biv_mi)
message("Common post-imputation sample: N = ", N_common_biv_mi,
        " in each of ", M_biv_mi, " imputations.")
models_biv_mi <- setNames(lapply(vars_biv_mi, function(v) {
  cols <- c("CA40", v)
  ok <- common_keep_biv_mi
  if (!any(ok)) stop("No eligible observations for ", v)
  ds <- lapply(bases_biv_mi, function(d) droplevels(d[ok, cols, drop = FALSE]))
  for (d in ds) {
    if (!setequal(as.character(d$CA40), outcomes_biv_mi)) stop("Missing custody category for ", v)
    if (dplyr::n_distinct(d[[v]]) < 2L) stop("No variation for ", v)
    if (is.numeric(d[[v]]) && any(!is.finite(d[[v]]))) stop("Non-finite values for ", v)
  }
  if (is.factor(ds[[1]][[v]]) && !all(vapply(ds, function(d)
    identical(levels(d[[v]]), levels(ds[[1]][[v]])), logical(1))))
    stop("Inconsistent factor levels for ", v)
  ds
}), vars_biv_mi)

sample_counts_biv_mi <- map_dfr(vars_biv_mi, function(v) {
  d <- models_biv_mi[[v]][[1]]
  tibble(Variable = v, N = nrow(d),
         N_Mother = sum(d$CA40 == "Mother LPC"),
         N_Unequal = sum(d$CA40 == "Unequal SPC"),
         N_Equal = sum(d$CA40 == "Equal SPC"))
})
stopifnot(all(sample_counts_biv_mi$N == N_common_biv_mi),
          n_distinct(sample_counts_biv_mi$N_Mother) == 1L,
          n_distinct(sample_counts_biv_mi$N_Unequal) == 1L,
          n_distinct(sample_counts_biv_mi$N_Equal) == 1L)
print(sample_counts_biv_mi, n = Inf)

# Estimate AMEs separately in each imputation.
ame_biv_mi_by_imputation <- map_dfr(vars_biv_mi, function(v) {
  message("Bivariate MI AMEs: ", v)
  imap_dfr(models_biv_mi[[v]], function(d, i) {
    fit <- nnet::multinom(reformulate(v, response = "CA40"), data = d,
                          na.action = na.fail, trace = FALSE, Hess = TRUE, model = TRUE,
                          maxit = 1000, MaxNWts = 10000)
    if (fit$convergence != 0) stop("Non-convergence for ", v, ", imputation ", i)
    if (nrow(fit$model) != nrow(d)) stop("Unexpected row exclusion for ", v)
    a <- as.data.frame(marginaleffects::avg_slopes(fit, variables = v, type = "probs"))
    candidates <- intersect(c("group", "outcome", "response", "y.level", "level", "class", "eq"), names(a))
    oc <- candidates[vapply(candidates, function(nm)
      setequal(as.character(a[[nm]]), outcomes_biv_mi), logical(1))]
    if (!length(oc)) stop("Cannot identify custody outcome column for ", v)
    a$Outcome <- as.character(a[[oc[1]]])
    if (!"contrast" %in% names(a)) a$contrast <- ""
    a$contrast <- as.character(a$contrast)
    a$contrast[is.na(a$contrast)] <- ""
    if (v %in% numeric_biv_mi) {
      a$Category <- ""
      if (nrow(a) != 3L || anyDuplicated(a$Outcome)) stop("Unexpected continuous AME rows for ", v)
    } else {
      lev <- levels(d[[v]])
      expected <- paste(lev[-1], "-", lev[1])
      if (!setequal(a$contrast, expected)) stop("Unexpected categorical contrasts for ", v)
      a$Category <- lev[-1][match(a$contrast, expected)]
      for (ct in expected) {
        z <- a$Outcome[a$contrast == ct]
        if (anyDuplicated(z) || !setequal(z, outcomes_biv_mi)) stop("Missing or duplicated AMEs for ", v)
      }
    }
    if (any(!is.finite(a$estimate)) || any(!is.finite(a$std.error))) stop("Invalid AMEs for ", v)
    tibble(.imp = as.integer(i), Variable = v, Outcome = a$Outcome,
           Category = a$Category, estimate = a$estimate, std.error = a$std.error,
           N = nrow(d))
  })
})

# Pool each outcome-specific AME using Rubin's rules.
# The scalar-pooling convention matches the previous Wald script.
pooled_biv_mi <- ame_biv_mi_by_imputation %>%
  group_by(Variable, Outcome, Category) %>%
  group_modify(~ {
    x <- .x
    if (nrow(x) != M_biv_mi || anyDuplicated(x$.imp) || n_distinct(x$N) != 1L)
      stop("Incomplete or inconsistent AME estimates across imputations.")
    p <- mice::pool.scalar(Q = x$estimate, U = x$std.error^2, n = unique(x$N), k = 1)
    se <- sqrt(p$t)
    if (!is.finite(se) || se <= 0 || is.na(p$df) || p$df <= 0) stop("Invalid pooled uncertainty.")
    critical <- qt(.975, df = p$df)
    tibble(estimate = p$qbar, std.error = se, df = p$df,
           conf.low = p$qbar - critical * se, conf.high = p$qbar + critical * se,
           p.value = 2 * pt(-abs(p$qbar / se), df = p$df), m = nrow(x))
  }) %>% ungroup()

# Build variable headers and reference rows in the requested order.
structure_biv_mi <- map_dfr(vars_biv_mi, function(v) {
  if (v %in% numeric_biv_mi) return(tibble(Variable = v, Category = "",
                                           RowType = "single", RowOrder = 1L, Term = unname(labels_biv_mi[v])))
  lev <- levels(models_biv_mi[[v]][[1]][[v]])
  bind_rows(
    tibble(Variable = v, Category = NA_character_, RowType = "header",
           RowOrder = 1L, Term = unname(labels_biv_mi[v])),
    tibble(Variable = v, Category = lev,
           RowType = c("reference", rep("level", length(lev) - 1L)),
           RowOrder = seq_along(lev) + 1L,
           Term = c(paste0(lev[1], " (ref.)"), lev[-1])))
})

p_text_biv_mi <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .001, "p < 0.001", sprintf("p = %.3f", p)))
}
formatted_biv_mi <- pooled_biv_mi %>% mutate(
  AME = paste0(sprintf("%.3f [%.3f, %.3f]", estimate, conf.low, conf.high),
               "\n", p_text_biv_mi(p.value))) %>%
  select(Variable, Category, Outcome, AME) %>%
  pivot_wider(names_from = Outcome, values_from = AME)

# Check that every estimated category appears in the display structure.
if (nrow(anti_join(pooled_biv_mi, structure_biv_mi, by = c("Variable", "Category"))))
  stop("An estimated category is missing from the table layout.")
ame_biv_mi_table <- structure_biv_mi %>%
  left_join(formatted_biv_mi, by = c("Variable", "Category")) %>%
  left_join(sample_counts_biv_mi, by = "Variable") %>%
  arrange(match(Variable, vars_biv_mi), RowOrder)
for (oc in outcomes_biv_mi) {
  need <- ame_biv_mi_table$RowType %in% c("single", "level")
  if (any(is.na(ame_biv_mi_table[[oc]][need]))) stop("Missing displayed AME for ", oc)
  ame_biv_mi_table[[oc]][!need] <- ""
}
ame_biv_mi_display <- ame_biv_mi_table %>% mutate(
  N_display = if_else(RowType %in% c("header", "single"), as.character(N), "")) %>%
  select(Term, N = N_display, all_of(outcomes_biv_mi))

# Group Ns appear once in the headers and are identical across models.
header_n_biv_mi <- function(x) {
  x <- sort(unique(x))
  if (length(x) == 1L) as.character(x) else paste0(min(x), "–", max(x))
}
headers_biv_mi <- setNames(paste0(outcomes_biv_mi, "\nN = ",
                                  vapply(sample_counts_biv_mi[c("N_Mother", "N_Unequal", "N_Equal")],
                                         header_n_biv_mi, character(1))), outcomes_biv_mi)
var_rows <- which(ame_biv_mi_table$RowType %in% c("header", "single"))
cat_rows <- which(ame_biv_mi_table$RowType %in% c("reference", "level"))
ref_rows <- which(ame_biv_mi_table$RowType == "reference")
apa_border <- officer::fp_border(color = "black", width = 1)

ft_bivariate_mi <- flextable(ame_biv_mi_display) %>%
  set_header_labels(Term = "Independent variables", N = "N") %>%
  set_header_labels(values = headers_biv_mi) %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:5, align = "center", part = "all") %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding.top = 2.5, padding.bottom = 2.5,
          padding.left = 2, padding.right = 2, part = "all") %>%
  line_spacing(space = 1.05, part = "all")
if (length(var_rows)) ft_bivariate_mi <- bold(ft_bivariate_mi, i = var_rows, j = 1, part = "body")
if (length(cat_rows)) ft_bivariate_mi <- padding(ft_bivariate_mi, i = cat_rows, j = 1, padding.left = 12, part = "body")
if (length(ref_rows)) ft_bivariate_mi <- italic(ft_bivariate_mi, i = ref_rows, j = 1:5, part = "body")
ft_bivariate_mi <- ft_bivariate_mi %>%
  set_caption("Table. Bivariate associations between characteristics and custody arrangements.") %>%
  add_footer_lines(paste0(
    "Note. Each multinomial model includes one predictor and no controls. ",
    "Average marginal effects [95% CI] were pooled across ", M_biv_mi,
    " imputations using Rubin's rules. P-values are displayed below estimates. ",
    "All models use the same individuals across all imputations. ",
    "The common sample requires available custody and all included predictors after imputation. ",
    "Protected missing values remain missing; individuals with remaining missing values ",
    "on any included predictor are excluded from all models. ",
    "N is identical across predictors. Custody-specific Ns appear only in headers.")) %>%
  border_remove() %>% hline_top(border = apa_border, part = "all") %>%
  hline(border = apa_border, part = "header") %>%
  hline_bottom(border = apa_border, part = "all") %>% autofit() %>%
  set_table_properties(layout = "autofit", width = 1, opts_word = list(split = TRUE)) %>%
  width(j = 1, width = 2.7) %>% width(j = 2:5, width = 1.05)

ft_bivariate_mi

# Record the software environment used for this run.
writeLines(capture.output(sessionInfo()), file.path(output_dir, "sessionInfo.txt"))
