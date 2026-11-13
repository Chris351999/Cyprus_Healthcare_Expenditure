# =============================================================================
# 02_clean.R
# Clean the 7 CYSTAT tables and fill the 2021 gaps.
# Input : data/raw/2160*.csv
# Output: data/processed/cystat_clean.RData
# =============================================================================

library(tidyverse)


# ---- STAGE 1. Read the files ------------------------------------------------
# WHY: load everything that 01_download.R saved.
files <- list.files("data/raw", pattern = "^2160.*csv$", full.names = TRUE)
raw   <- map(files, read_csv, show_col_types = FALSE)
names(raw) <- str_remove(basename(files), ".csv")


# ---- STAGE 2. Look at the data ----------------------------------------------
# WHY: before changing anything, see what it looks like.
map(raw, head, 3)
# WE SEE: long column names in capitals, in a different order in each table.
#         The money is always the last column.


# ---- STAGE 3. Short column names --------------------------------------------
# WHY: short names are easier to type. We find each column by a word in its name.
rename_columns <- function(df) {
  names(df)[ncol(df)] <- "value_meur"
  names(df)[str_detect(names(df), "YEAR")]      <- "year"
  names(df)[str_detect(names(df), "FUNCTION")]  <- "care"
  names(df)[str_detect(names(df), "FINANCING")] <- "financing"
  names(df)[str_detect(names(df), "PROVIDER")]  <- "provider"
  df
}
raw <- map(raw, rename_columns)
map(raw, names)
# WE SEE: year = "2023 (p,r)" is text, not a number.


# ---- STAGE 4. Year as a number, letters in their own column -----------------
# WHY: we need 2023 as a number for charts. The letters (b, p, r) are CYSTAT
#      warnings, so we keep them in the column year_flag.
fix_year <- function(df) {
  df$year_flag  <- str_remove_all(df$year, "[0-9() ]")   # "2023 (p,r)" -> "p,r"
  df$year       <- parse_number(df$year)                 # "2023 (p,r)" -> 2023
  df$value_meur <- as.numeric(df$value_meur)
  df
}
raw <- map(raw, fix_year)
distinct(raw[["2160011E"]], year, year_flag)
# WE SEE: b = 2015 (break in method) | r = 2022, 2023 | p = 2023, 2024
#         The 2021 break is a note on single cells, which the API does not return.


# ---- STAGE 5. Split "code - label" into two columns -------------------------
# WHY: "HF.3 - Household out-of-pocket payment" -> code "HF.3" is short for filtering.
split_codes <- function(df) {
  if ("care" %in% names(df))
    df <- separate(df, care, into = c("care_code", "care_label"), sep = " - ", extra = "merge")
  if ("financing" %in% names(df))
    df <- separate(df, financing, into = c("financing_code", "financing_label"), sep = " - ", extra = "merge")
  if ("provider" %in% names(df))
    df <- separate(df, provider, into = c("provider_code", "provider_label"), sep = " - ", extra = "merge")
  df
}
raw <- map(raw, split_codes)


# ---- STAGE 6. Give each table a clear name ----------------------------------
# WHY: "financing" says more than "2160011E".
total              <- raw[["2160010E"]]   # total spending
financing          <- raw[["2160011E"]]   # who pays
care               <- raw[["2160012E"]]   # type of care
provider           <- raw[["2160013E"]]   # who provides
care_financing     <- raw[["2160015E"]]   # type of care x who pays
provider_financing <- raw[["2160016E"]]   # provider x who pays
care_provider      <- raw[["2160017E"]]   # type of care x provider

distinct(financing, financing_code, financing_label)
distinct(care, care_code, care_label)


# ---- STAGE 7. Find missing values -------------------------------------------
# WHY: a missing value = a hole in the chart.
financing %>% filter(is.na(value_meur)) %>% count(year, financing_code)
care      %>% filter(is.na(value_meur)) %>% count(year, care_code)
# WE SEE:
#  - HF.4 (money from abroad) empty from 2016: not published, tiny -> we leave it.
#  - 2021: out-of-pocket (HF.3) is missing.
#  - 2021: inpatient and day care are missing.


# ---- STAGE 8. Test rule 1: total = sum of all payers ------------------------
# WHY: if the rule works in normal years, we can use it to find 2021.
other_schemes <- c("HF.1.1", "HF.1.2", "HF.2.1", "HF.2.2", "HF.2.3")

financing %>%
  group_by(year) %>%
  summarise(gap = round(value_meur[financing_code == "All HF"] -
                          sum(value_meur[financing_code %in% c(other_schemes, "HF.3", "HF.4")], na.rm = TRUE), 2))
# WE SEE: gap = 0 in every year except 2021 (= 369.9, the missing out-of-pocket).
#         -> rule works: out-of-pocket = total - other payers.


# ---- STAGE 9. Fill out-of-pocket 2021 in "who pays" -------------------------
total_2021  <- financing %>% filter(year == 2021, financing_code == "All HF") %>% pull(value_meur)
others_2021 <- financing %>% filter(year == 2021, financing_code %in% other_schemes) %>% pull(value_meur) %>% sum()
oop_2021    <- total_2021 - others_2021
oop_2021     # expect 369.898 (same as the CYSTAT Excel)

financing <- financing %>%
  mutate(imputed    = year == 2021 & financing_code == "HF.3",   # TRUE = we filled it
         value_meur = if_else(imputed, oop_2021, value_meur))


# ---- STAGE 10. Same rule in "type of care x who pays" -----------------------
# WHY: same rule, but now once for EACH type of care.
totals <- care_financing %>%
  filter(year == 2021, financing_code == "All HF") %>%
  select(care_code, total = value_meur)

others <- care_financing %>%
  filter(year == 2021, financing_code %in% other_schemes) %>%
  group_by(care_code) %>%
  summarise(others = sum(value_meur))

oop_by_care <- left_join(totals, others, by = "care_code") %>%
  mutate(oop = total - others)
oop_by_care   # look: one OOP value per type of care, none negative

care_financing <- care_financing %>%
  left_join(select(oop_by_care, care_code, oop), by = "care_code") %>%
  mutate(imputed    = year == 2021 & financing_code == "HF.3" & !is.na(oop),
         value_meur = if_else(imputed, oop, value_meur)) %>%
  select(-oop)


# ---- STAGE 11. Same rule in "provider x who pays" ---------------------------
# WHY: same rule, now once for EACH provider.
totals <- provider_financing %>%
  filter(year == 2021, financing_code == "All HF") %>%
  select(provider_code, total = value_meur)

others <- provider_financing %>%
  filter(year == 2021, financing_code %in% other_schemes) %>%
  group_by(provider_code) %>%
  summarise(others = sum(value_meur))

oop_by_provider <- left_join(totals, others, by = "provider_code") %>%
  mutate(oop = total - others)
oop_by_provider   # look: one OOP value per provider, none negative

provider_financing <- provider_financing %>%
  left_join(select(oop_by_provider, provider_code, oop), by = "provider_code") %>%
  mutate(imputed    = year == 2021 & financing_code == "HF.3" & !is.na(oop),
         value_meur = if_else(imputed, oop, value_meur)) %>%
  select(-oop)

# tables where we filled nothing
total$imputed         <- FALSE
care$imputed          <- FALSE
provider$imputed      <- FALSE
care_provider$imputed <- FALSE


# ---- STAGE 12. Test rule 2: curative = inpatient + day + outpatient + home ---
care %>%
  group_by(year) %>%
  summarise(gap = round(value_meur[care_code == "HC.1-HC.2"] -
                          value_meur[care_code == "HC.1.1-HC.2.1"] - value_meur[care_code == "HC.1.2-HC.2.2"] -
                          value_meur[care_code == "HC.1.3-HC.2.3"] - value_meur[care_code == "HC.1.4-HC.2.4"], 2))
# WE SEE: gap = 0 every year, 2021 = NA.
#         In 2021 TWO values are missing: we know their sum, not the split.
#         -> we do not guess. We join them: "Hospital care" = inpatient + day.


# ---- STAGE 13. Add "Hospital care" = curative - outpatient - home ------------
# WHY: put the 3 numbers side by side (like Excel columns) and subtract.
add_hospital <- function(df) {
  hospital <- df %>%
    filter(care_code %in% c("HC.1-HC.2", "HC.1.3-HC.2.3", "HC.1.4-HC.2.4")) %>%
    select(-care_label, -imputed) %>%
    pivot_wider(names_from = care_code, values_from = value_meur) %>%
    mutate(value_meur = `HC.1-HC.2` - `HC.1.3-HC.2.3` - `HC.1.4-HC.2.4`,
           care_code  = "HC.1.1+HC.1.2",
           care_label = "Hospital care (inpatient and day)",
           imputed    = FALSE) %>%
    select(-`HC.1-HC.2`, -`HC.1.3-HC.2.3`, -`HC.1.4-HC.2.4`)
  bind_rows(df, hospital)
}

care           <- add_hospital(care)
care_financing <- add_hospital(care_financing)
care_provider  <- add_hospital(care_provider)


# ---- STAGE 14. Check --------------------------------------------------------
# WHY: never trust a fix without looking at it.
financing %>% filter(financing_code == "HF.3", year %in% 2019:2023) %>% select(year, value_meur, imputed)
care      %>% filter(care_code == "HC.1.1+HC.1.2", year %in% 2019:2023) %>% select(year, value_meur)
c(sum(financing$imputed), sum(care_financing$imputed), sum(provider_financing$imputed))
# EXPECT: OOP 2021 = 369.9, between 2020 (413) and 2022 (404)
#         Hospital care 2021 = 945 (jump after GeSY phase 2, June 2020)
#         Filled values: 1, 15, 10


# ---- STAGE 15. Save ---------------------------------------------------------
save(total, financing, care, provider, care_financing, provider_financing, care_provider,
     file = "data/processed/cystat_clean.RData")
message("02_clean.R done")