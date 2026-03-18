# ============================================================
# Pathogen Exposure - Crude Proportions
# ACCIDDA Survey Serology Project
# ============================================================

library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
dynata_02282026 <- read.csv("C:/Users/camlam/OneDrive - Johns Hopkins/ACCIDDA_SurveySerology/Data/data clean/wave1234_clean_weights_dynata.csv") %>%
  filter(state_residence != "MI") %>%
  mutate(state_residence = factor(state_residence))

# ============================================================
# HELPER FUNCTION: count + percent cell
# ============================================================
fmt_cell <- function(n, total) {
  pct <- round(n / total * 100, 2)
  paste0(n, " (", pct, "%)")
}

# ============================================================
# TABLE 1: BINARY VARIABLES
# Response options: 0=No, 1=Yes, 2=Not sure, 98=Prefer not to say
# ============================================================

binary_variables <- c(
  "vax_hx_covid",
  "vax_hx_flu_past_2_yrs",
  "vax_flu_this_year",
  "vax_hx_rsv",
  "vax_hx_mmr",
  "covid_hx",
  "covid_test",
  "covid_test_positive",
  "flu_test",
  "flu_test_positive",
  "rsv_test",
  "rsv_test_positive",
  "past_2_wks_covid_symptoms",
  "past_2_wks_covid_exposed"
)

binary_levels <- c("No", "Yes", "Not sure", "Prefer not to say")

binary_table <- do.call(rbind, lapply(binary_variables, function(var) {
  x <- factor(dynata_02282026[[var]], levels = binary_levels)
  counts <- table(x)
  total <- sum(counts)
  row <- setNames(
    sapply(binary_levels, function(lv) {
      n <- ifelse(lv %in% names(counts), counts[lv], 0)
      fmt_cell(n, total)
    }),
    binary_levels
  )
  c(Variable = var, row, Total = total)
}))

binary_table <- as.data.frame(binary_table, stringsAsFactors = FALSE)
print(binary_table)

# ============================================================
# TABLE 2: COVID VACCINE DOSES (vax_hx_covid_doses)
# Among those who answered Yes to vax_hx_covid
# ============================================================

doses_data   <- dynata_02282026 %>% filter(vax_hx_covid == "Yes")
doses_levels <- c("1", "2", "3", "4", "5 or more", "Not sure", "Prefer not to say")

doses_counts <- table(factor(doses_data$vax_hx_covid_doses, levels = doses_levels))
doses_total  <- sum(doses_counts)

doses_table <- data.frame(
  Variable = "vax_hx_covid_doses",
  Response = doses_levels,
  N_Pct    = sapply(doses_levels, function(lv) {
    n <- ifelse(lv %in% names(doses_counts), doses_counts[lv], 0)
    fmt_cell(n, doses_total)
  }),
  stringsAsFactors = FALSE
)
rownames(doses_table) <- NULL
print(doses_table)

# ============================================================
# TABLE 3: HOUSEHOLD POSITIVE (categorical)
# hh_covid_positive_2_wks, hh_flu_positive_2_wks, hh_rsv_positive_2_wks
# ============================================================

hh_variables <- c("hh_covid_positive_2_wks", "hh_flu_positive_2_wks", "hh_rsv_positive_2_wks")
hh_levels    <- c("No", "Not applicable/I live alone", "Yes, one child", "Yes, one adult",
                  "Yes, more than one person", "Not sure", "Prefer not to respond")

hh_table <- do.call(rbind, lapply(hh_variables, function(var) {
  x      <- factor(dynata_02282026[[var]], levels = hh_levels)
  counts <- table(x)
  total  <- sum(counts)
  data.frame(
    Variable = var,
    Response = hh_levels,
    N_Pct    = sapply(hh_levels, function(lv) {
      n <- ifelse(lv %in% names(counts), counts[lv], 0)
      fmt_cell(n, total)
    }),
    stringsAsFactors = FALSE
  )
}))
rownames(hh_table) <- NULL
print(hh_table)


# ============================================================
# TABLE 4: DATE VARIABLES (collapsed into seasonal categories)
# Variables: vax_hx_covid_last_time, covid_hx_recent,
#            covid_test_positive_recent, flu_test_positive_recent,
#            rsv_test_positive_recent
# Format in data: MM/YYYY string; Not sure=5, Prefer not to say=98
# ============================================================

# Recode pre-labeled season strings to MM/YY-MM/YY format
recode_season <- function(x) {
  dplyr::case_when(
    is.na(x)                                             ~ "NA (not applicable/skipped)",
    x == "Before December 2023"                          ~ "Before 12/23",
    x == "This winter (December 2023-February 2024)"     ~ "12/23-02/24",
    x == "This spring (March-May 2024)"                  ~ "03/24-05/24",
    x == "This summer (June-Aug 2024)"                   ~ "06/24-08/24",
    x == "This fall (Sep 2024-now)"                      ~ "09/24-11/24",
    x == "This winter (December 2024-February 2025)"     ~ "12/24-02/25",
    x == "This spring (March-May 2025)"                  ~ "03/25-05/25",
    x == "This summer (June-Aug 2025)"                   ~ "06/25-08/25",
    x == "This fall (Sep 2025-now)"                      ~ "09/25-11/25",
    x == "Not sure"                                      ~ "Not sure",
    x == "Prefer not to say"                             ~ "Prefer not to say",
    TRUE                                                 ~ "Other/Unknown"
  )
}

season_order <- c(
  "Before 12/23",
  "12/23-02/24",
  "03/24-05/24",
  "06/24-08/24",
  "09/24-11/24",
  "12/24-02/25",
  "03/25-05/25",
  "06/25-08/25",
  "09/25-11/25",
  "Not sure",
  "Prefer not to say",
  "NA (not applicable/skipped)",
  "Other/Unknown"
)

date_variables <- c(
  "vax_hx_covid_last_time",
  "covid_hx_recent",
  "covid_test_positive_recent",
  "flu_test_positive_recent",
  "rsv_test_positive_recent"
)

date_table <- do.call(rbind, lapply(date_variables, function(var) {
  x      <- factor(recode_season(dynata_02282026[[var]]), levels = season_order)
  counts <- table(x)
  total  <- sum(counts)
  data.frame(
    Variable = var,
    Response = season_order,
    N_Pct    = sapply(season_order, function(s) {
      n <- ifelse(s %in% names(counts), counts[s], 0)
      fmt_cell(n, total)
    }),
    stringsAsFactors = FALSE
  )
}))
rownames(date_table) <- NULL
print(date_table)


# ============================================================
# EXPORT ALL TABLES TO CSV
# ============================================================

output_dir <- "TABLES"
if (!dir.exists(output_dir)) dir.create(output_dir)

write.csv(binary_table, file.path(output_dir, "Table1_Binary_CrudeProps.csv"),      row.names = FALSE)
write.csv(doses_table,  file.path(output_dir, "Table2_CovidDoses_CrudeProps.csv"),   row.names = FALSE)
write.csv(hh_table,     file.path(output_dir, "Table3_HH_Positive_CrudeProps.csv"),  row.names = FALSE)
write.csv(date_table,   file.path(output_dir, "Table4_DateVars_CrudeProps.csv"),     row.names = FALSE)

cat("\nAll tables saved to TABLES/ directory.\n")



# ============================================================
# TABLE 5: DATE VARIABLES - WAVE 4 RAW (MM/YYYY format)
# Source: dynata_w4.csv
# Variables: covid_hx_recent, covid_need_test_recent,
#            covid_test_positive_recent, flu_test_positive_recent,
#            rsv_test_positive_recent, vax_hx_covid_last_time
# ============================================================

w4_raw <- read.csv("C:/Users/camlam/OneDrive - Johns Hopkins/ACCIDDA_SurveySerology/Data/data raw/dynata_w4.csv")

# Rename raw columns to clean names
w4_dates <- w4_raw %>%
  rename(
    covid_hx_recent_w4             = IH1ar8oe,
    covid_need_test_recent_w4      = IH2ar8oe,
    covid_test_positive_recent_w4  = IH3br8oe,
    flu_test_positive_recent_w4    = IH4br8oe,
    rsv_test_positive_recent_w4    = IH5br8oe,
    vax_hx_covid_last_time_w4      = PM1br8oe
  ) %>%
  select(
    covid_hx_recent_w4,
    covid_need_test_recent_w4,
    covid_test_positive_recent_w4,
    flu_test_positive_recent_w4,
    rsv_test_positive_recent_w4,
    vax_hx_covid_last_time_w4
  )

# Season assignment for MM/YYYY strings
assign_season_w4 <- function(date_str) {
  if (is.na(date_str) || date_str == "")  return("NA (not applicable/skipped)")
  
  parsed <- tryCatch(as.Date(paste0("01/", date_str), format = "%d/%m/%Y"), error = function(e) NA)
  if (is.na(parsed)) return("Other/Unknown")
  
  yr <- as.integer(format(parsed, "%Y"))
  mo <- as.integer(format(parsed, "%m"))
  
  if (yr < 2023 || (yr == 2023 && mo < 12))  return("Before 12/23")
  if (yr == 2023 && mo == 12)                 return("12/23-02/24")
  if (yr == 2024 && mo %in% 1:2)              return("12/23-02/24")
  if (yr == 2024 && mo %in% 3:5)              return("03/24-05/24")
  if (yr == 2024 && mo %in% 6:8)              return("06/24-08/24")
  if (yr == 2024 && mo %in% 9:11)             return("09/24-11/24")
  if (yr == 2024 && mo == 12)                 return("12/24-02/25")
  if (yr == 2025 && mo %in% 1:2)              return("12/24-02/25")
  if (yr == 2025 && mo %in% 3:5)              return("03/25-05/25")
  if (yr == 2025 && mo %in% 6:8)              return("06/25-08/25")
  if (yr == 2025 && mo %in% 9:11)             return("09/25-11/25")
  if (yr == 2025 && mo == 12)                 return("12/25-02/26")
  if (yr == 2026 && mo %in% 1:2)              return("12/25-02/26")
  if (yr == 2026 && mo >= 3)                  return("03/26+")
  return("Other/Unknown")
}

season_order_w4 <- c(
  "Before 12/23",
  "12/23-02/24",
  "03/24-05/24",
  "06/24-08/24",
  "09/24-11/24",
  "12/24-02/25",
  "03/25-05/25",
  "06/25-08/25",
  "09/25-11/25",
  "12/25-02/26",
  "03/26+",
  "NA (not applicable/skipped)",
  "Other/Unknown"
)

date_variables_w4 <- c(
  "covid_hx_recent_w4",
  "covid_need_test_recent_w4",
  "covid_test_positive_recent_w4",
  "flu_test_positive_recent_w4",
  "rsv_test_positive_recent_w4",
  "vax_hx_covid_last_time_w4"
)

date_table_w4 <- do.call(rbind, lapply(date_variables_w4, function(var) {
  x      <- as.character(w4_dates[[var]])
  seasons <- sapply(x, assign_season_w4)
  seasons <- factor(seasons, levels = season_order_w4)
  counts  <- table(seasons)
  total   <- length(x)
  data.frame(
    Variable = var,
    Response = season_order_w4,
    N_Pct    = sapply(season_order_w4, function(s) {
      n <- ifelse(s %in% names(counts), counts[s], 0)
      fmt_cell(n, total)
    }),
    stringsAsFactors = FALSE
  )
}))
rownames(date_table_w4) <- NULL
print(date_table_w4)

write.csv(date_table_w4, file.path(output_dir, "Table5_DateVars_W4_CrudeProps.csv"), row.names = FALSE)
cat("\nTable 5 saved.\n")

# ============================================================
# TABLE 6: DATE VARIABLES - COMBINED W1/W2 + W3 + W4
# W1/W2: pre-labeled season strings (already in Table 4)
# W3: Mon-YY format in r8oe columns; 5=Not sure, 98=Prefer not to say
# W4: MM/YYYY format in r8oe columns; 5=Not sure, 98=Prefer not to say
# ============================================================

# --- Load W3 and W4 raw ---
w3_raw <- read.csv("C:/Users/camlam/OneDrive - Johns Hopkins/ACCIDDA_SurveySerology/Data/data raw/dynata_w3.csv")
# w4_raw already loaded above

# --- Shared season order ---
season_order_combined <- c(
  "Before 12/23",
  "12/23-02/24",
  "03/24-05/24",
  "06/24-08/24",
  "09/24-11/24",
  "12/24-02/25",
  "03/25-05/25",
  "06/25-08/25",
  "09/25-11/25",
  "12/25-02/26",
  "03/26+",
  "Not sure",
  "Prefer not to say",
  "NA (not applicable/skipped)",
  "Other/Unknown"
)

# --- Parser for W3 Mon-YY format (e.g. "Oct-24") ---
assign_season_w3 <- function(date_str) {
  if (is.na(date_str) || date_str == "")  return("NA (not applicable/skipped)")
  parsed <- tryCatch(as.Date(paste0("01-", date_str), format = "%d-%b-%y"), error = function(e) NA)
  if (is.na(parsed)) return("Other/Unknown")
  yr <- as.integer(format(parsed, "%Y"))
  mo <- as.integer(format(parsed, "%m"))
  if (yr < 2023 || (yr == 2023 && mo < 12))  return("Before 12/23")
  if (yr == 2023 && mo == 12)                 return("12/23-02/24")
  if (yr == 2024 && mo %in% 1:2)              return("12/23-02/24")
  if (yr == 2024 && mo %in% 3:5)              return("03/24-05/24")
  if (yr == 2024 && mo %in% 6:8)              return("06/24-08/24")
  if (yr == 2024 && mo %in% 9:11)             return("09/24-11/24")
  if (yr == 2024 && mo == 12)                 return("12/24-02/25")
  if (yr == 2025 && mo %in% 1:2)              return("12/24-02/25")
  if (yr == 2025 && mo %in% 3:5)              return("03/25-05/25")
  if (yr == 2025 && mo %in% 6:8)              return("06/25-08/25")
  if (yr == 2025 && mo %in% 9:11)             return("09/25-11/25")
  if (yr == 2025 && mo == 12)                 return("12/25-02/26")
  if (yr == 2026 && mo %in% 1:2)              return("12/25-02/26")
  if (yr == 2026 && mo >= 3)                  return("03/26+")
  return("Other/Unknown")
}

# --- Helper: get season vector from a wave using code + r8oe columns ---
get_seasons_coded <- function(code_col, text_col, parse_fn) {
  sapply(seq_along(code_col), function(i) {
    code <- code_col[i]
    txt  <- text_col[i]
    if (is.na(code))   return("NA (not applicable/skipped)")
    if (code == 5)     return("Not sure")
    if (code == 98)    return("Prefer not to say")
    if (code == 8)     return(parse_fn(txt))
    return("Other/Unknown")
  })
}

# --- Variable mapping: clean name -> (code col, text col) ---
# W3 and W4 share same column names
var_map <- list(
  vax_hx_covid_last_time     = list(code = "PM1b",  text = "PM1br8oe"),
  covid_hx_recent            = list(code = "IH1a",  text = "IH1ar8oe"),
  covid_need_test_recent     = list(code = "IH2a",  text = "IH2ar8oe"),
  covid_test_positive_recent = list(code = "IH3b",  text = "IH3br8oe"),
  flu_test_positive_recent   = list(code = "IH4b",  text = "IH4br8oe"),
  rsv_test_positive_recent   = list(code = "IH5b",  text = "IH5br8oe")
)

# Variables that exist in W1/W2 (Table 4) — exclude covid_need_test_recent
vars_in_t4 <- c(
  "vax_hx_covid_last_time",
  "covid_hx_recent",
  "covid_test_positive_recent",
  "flu_test_positive_recent",
  "rsv_test_positive_recent"
)

# --- Build combined table ---
date_table_combined <- do.call(rbind, lapply(names(var_map), function(var) {
  
  # W3 seasons
  w3_seasons <- get_seasons_coded(
    w3_raw[[var_map[[var]]$code]],
    w3_raw[[var_map[[var]]$text]],
    assign_season_w3
  )
  
  # W4 seasons
  w4_seasons <- get_seasons_coded(
    w4_raw[[var_map[[var]]$code]],
    w4_raw[[var_map[[var]]$text]],
    assign_season_w4
  )
  
  # W1/W2 counts from Table 4 (only for overlapping variables)
  if (var %in% vars_in_t4) {
    t4_sub    <- date_table[date_table$Variable == var, ]
    t4_counts <- setNames(
      as.integer(gsub(" \\(.*", "", t4_sub$N_Pct)),
      t4_sub$Response
    )
  } else {
    t4_counts <- setNames(rep(0L, length(season_order_combined)), season_order_combined)
  }
  
  # Combine W3 + W4 counts
  w34_seasons <- factor(c(w3_seasons, w4_seasons), levels = season_order_combined)
  w34_counts  <- table(w34_seasons)
  
  # Sum all waves
  all_counts <- sapply(season_order_combined, function(s) {
    t4 <- ifelse(s %in% names(t4_counts), t4_counts[s], 0L)
    w34 <- ifelse(s %in% names(w34_counts), as.integer(w34_counts[s]), 0L)
    t4 + w34
  })
  
  total <- sum(all_counts)
  
  data.frame(
    Variable = var,
    Response = season_order_combined,
    N_Pct    = sapply(season_order_combined, function(s) fmt_cell(all_counts[s], total)),
    stringsAsFactors = FALSE
  )
}))

rownames(date_table_combined) <- NULL
print(date_table_combined)

write.csv(date_table_combined, file.path(output_dir, "Table6_DateVars_Combined_CrudeProps.csv"), row.names = FALSE)
cat("\nTable 6 saved.\n")



