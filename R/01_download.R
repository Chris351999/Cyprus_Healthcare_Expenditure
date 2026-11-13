# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 01_download.R
# PURPOSE: Create the project folders and download all data
#          (CYSTAT health accounts + Eurostat) as CSV files
# OUTPUT: data/raw/*.csv
# ============================================================


# 1. Load packages --------------------------------------------

library(tidyverse)
library(pxweb)
library(eurostat)


# 2. Create the project folders -------------------------------

dir.create("data/raw",        recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("app/data",        recursive = TRUE, showWarnings = FALSE)


# 3. CYSTAT: the 7 health expenditure tables ------------------

cystat_url <- "https://cystatdb.cystat.gov.cy/api/v1/en/8.CYSTAT-DB/Health/Health Care Expenditure/"

cystat_tables <- c(
  "2160010E",  # total, current and capital expenditure
  "2160011E",  # by financing scheme
  "2160012E",  # by type of care
  "2160013E",  # by provider
  "2160015E",  # type of care x financing scheme
  "2160016E",  # provider x financing scheme
  "2160017E"   # type of care x provider
)

download_cystat <- function(table_code) {
  url   <- URLencode(paste0(cystat_url, table_code, ".px"))
  meta  <- pxweb_get(url)
  codes <- sapply(meta$variables, function(v) v$code)
  query <- setNames(rep(list("*"), length(codes)), codes)
  data  <- pxweb_get(url, query = pxweb_query(query))
  df    <- as.data.frame(data, column.name.type = "text", variable.value.type = "text")
  write_csv(df, paste0("data/raw/", table_code, ".csv"))
  message(table_code, ": ", nrow(df), " rows")
  Sys.sleep(1)
}

walk(cystat_tables, download_cystat)


# 4. Eurostat: GDP, population, prices, EU comparison ---------

eu_codes <- eu_countries$code                     # the 27 EU countries
compare  <- c("CY", "EL", "MT", "PT", "EU27_2020") # Cyprus + comparators

download_eurostat <- function(dataset, filters, file_name) {
  df <- get_eurostat(dataset, filters = filters, time_format = "num")
  names(df)[names(df) %in% c("time", "TIME_PERIOD")] <- "year"
  write_csv(df, paste0("data/raw/", file_name, ".csv"))
  message(file_name, ": ", nrow(df), " rows")
}

download_eurostat("nama_10_gdp",
                  list(geo = "CY", na_item = "B1GQ", unit = "CP_MEUR"),
                  "eurostat_gdp")

download_eurostat("demo_gind",
                  list(geo = "CY", indic_de = "JAN"),
                  "eurostat_population")

download_eurostat("prc_hicp_aind",
                  list(geo = "CY", coicop = "CP00", unit = "INX_A_AVG"),
                  "eurostat_prices")

download_eurostat("hlth_sha11_hf",
                  list(geo = c(eu_codes, "EU27_2020"),
                       icha11_hf = c("TOT_HF", "HF1", "HF3"),
                       unit = c("PC_GDP", "PPS_HAB", "PC_CHE")),
                  "eurostat_spending")

download_eurostat("demo_mlexpec",
                  list(geo = eu_codes, sex = "T", age = "Y_LT1", unit = "YR"),
                  "eurostat_life_expectancy")

download_eurostat("hlth_cd_apr",
                  list(geo = eu_codes, sex = "T", icd10 = "TOTAL",
                       unit = "RT", mortalit = c("TRT", "PRVT")),
                  "eurostat_mortality")

download_eurostat("hlth_silc_08",
                  list(geo = compare, sex = "T", age = "Y_GE16", unit = "PC"),
                  "eurostat_unmet_needs")


# 5. Check: list the downloaded files -------------------------

list.files("data/raw")
