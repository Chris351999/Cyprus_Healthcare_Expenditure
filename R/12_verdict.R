# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 12_verdict.R
# PURPOSE: Was GeSY good for the population of Cyprus?
#          (1) difference-in-differences vs comparator countries
#          (2) a scorecard for the dashboard
# INPUT:  data/processed/eu_comparison.csv   (from 08_eu_comparison.R)
#         data/processed/unmet_needs.csv     (from 10_unmet_needs.R)
#         R/03_labels.R
# OUTPUT: data/processed/did_results.csv
#         data/processed/verdict.csv
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
source("R/03_labels.R")   # pre_years (2016-2018), post_years (2022-2024)

eu    <- read_csv("data/processed/eu_comparison.csv", show_col_types = FALSE)
unmet <- read_csv("data/processed/unmet_needs.csv",   show_col_types = FALSE)


# 2. Difference-in-differences: the idea ----------------------
#    Change in Cyprus (after - before)
#  - Change in Greece, Malta, Portugal (after - before)
#  = change that is NOT explained by things that hit all
#    countries (COVID, inflation, economic cycle)
#
#    Before = 2016-2018 (no GeSY), After = 2022-2024 (full GeSY)

did <- function(df, indicator_name) {
  means <- df %>%
    filter(country %in% c("Cyprus", "Greece", "Malta", "Portugal"), !is.na(value)) %>%
    mutate(period = case_when(year %in% pre_years  ~ "before",
                              year %in% post_years ~ "after"),
           group  = if_else(country == "Cyprus", "cyprus", "comparators")) %>%
    filter(!is.na(period)) %>%
    group_by(group, period) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    pivot_wider(names_from = c(group, period), values_from = value)
  
  means %>%
    mutate(indicator          = indicator_name,
           cyprus_change      = cyprus_after      - cyprus_before,
           comparators_change = comparators_after - comparators_before,
           did_estimate       = cyprus_change - comparators_change) %>%
    select(indicator, cyprus_before, cyprus_after, cyprus_change,
           comparators_before, comparators_after, comparators_change, did_estimate)
}


# 3. Apply it to three indicators -----------------------------

did_results <- bind_rows(
  eu    %>% filter(indicator == "Out-of-pocket share (%)") %>%
    did("Out-of-pocket share of health spending (%)"),
  eu    %>% filter(indicator == "Public share (%)") %>%
    did("Public share of health spending (%)"),
  unmet %>% filter(reason == "Too expensive", income == "Poorest 20%") %>%
    did("Poorest 20% who skipped care because of cost (%)")
)

did_results %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print(width = Inf)


# 4. Scorecard for the dashboard ------------------------------
#    (numbers from our analysis + independent sources)

verdict <- tribble(
  ~dimension,             ~finding,                                                                   ~rating,   ~source,
  "Financial protection", "Out-of-pocket share fell from 44% (2018) to 17% (2024)",                   "Success", "CYSTAT",
  "Financial protection", "Hospital care paid out-of-pocket fell from 19% to 3% (2018-2024)",         "Success", "CYSTAT",
  "Financial protection", "Medicines paid out-of-pocket fell from 76% to 26% (2018-2024)",            "Success", "CYSTAT",
  "Equity of access",     "Poorest 20% skipping care due to cost fell from 3.0% (2018) to 0.3% (2025)", "Success", "Eurostat EU-SILC",
  "Coverage gaps",        "Out-of-pocket spending on long-term care up 51% (2021-2024)",              "Concern", "CYSTAT",
  "Coverage gaps",        "Outpatient care still 31% out-of-pocket (EUR 202m in 2024)",               "Concern", "CYSTAT",
  "Efficiency",           "Hospital care up 148% vs outpatient care up 53% (2018-2024)",              "Concern", "CYSTAT",
  "Efficiency",           "WHO warns of low-value specialist care and weak primary care",             "Concern", "WHO/Observatory 2026",
  "Cost to society",      "Real spending per person up 46% (2018-2024); 7.7% of GDP vs EU 10%",       "Mixed",   "CYSTAT, Eurostat",
  "Responsiveness",       "92% of beneficiaries say GeSY improved their quality of life",            "Success", "HIO survey 2024",
  "Responsiveness",       "Waiting times for several specialists grew (e.g. dermatology 2->6 months)", "Concern", "Politis survey 2024",
  "Sustainability",       "Fund surplus and EUR 707m reserve (2024); viable to about 2032",           "Success", "HIO / ILO",
  "Health outcomes",      "8th of 27 in treatable mortality (2023), but already low before GeSY",     "Unclear", "Eurostat"
)

verdict %>% count(rating)


# 5. Save for the dashboard -----------------------------------

write_csv(did_results, "data/processed/did_results.csv")
write_csv(verdict,     "data/processed/verdict.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# Difference-in-differences: Cyprus vs Greece, Malta, Portugal,
# before (2016-2018) vs after GeSY (2022-2024).
#
# 1. GeSY cut the out-of-pocket share by about 25 points.
#    Cyprus:      44.6% -> 17.1% (-27.5 points)
#    Comparators: 33.3% -> 31.1% (-2.2 points)
#    DiD estimate: -25.3 points. Almost all of the fall happened only
#    in Cyprus, so it is not explained by COVID, inflation or the
#    economic cycle, which affected all four countries.
#
# 2. GeSY raised the pooled (public) share by about 35 points.
#    Cyprus:      41.8% -> 78.4% (+36.6 points)
#    Comparators: 61.6% -> 63.4% (+1.8 points)
#    DiD estimate: +34.9 points.
#
# 3. Skipping care because of cost: no extra effect can be shown.
#    Poorest 20%, Cyprus:      2.9% -> 0.4% (-2.5 points)
#    Poorest 20%, comparators: 9.4% -> 6.4% (-2.9 points)
#    DiD estimate: +0.4 points (no extra improvement in Cyprus).
#    Why: Cyprus started close to zero and cannot fall below it
#    (floor effect), while comparators started much higher (mainly
#    Greece, about 19%). In relative terms Cyprus cut the problem by
#    86%, comparators by 31%. And the improvement in Cyprus started
#    in 2015, before GeSY (see 10_unmet_needs.R).
#
# 4. Scorecard (13 items): 6 Success, 5 Concern, 1 Mixed, 1 Unclear.
#    Successes: financial protection, equity, satisfaction, finances.
#    Concerns: long-term care, outpatient costs, hospital-centred
#    growth, waiting times.
#
# 5. Overall verdict.
#    GeSY clearly changed WHO pays: from the sick person at the point
#    of care to everyone, through income-based contributions. It
#    protects patients financially. It did not make health care
#    cheaper for society, and gaps remain in long-term care,
#    outpatient care, efficiency and waiting times.
#
# Caveats:
#  - Simple DiD with three comparator countries, no confidence
#    intervals: an estimate, not proof.
#  - It assumes Cyprus would have followed the comparators' trend
#    without GeSY (parallel trends). Before GeSY, Cyprus's public
#    share was falling, so the true effect may be even larger.
#  - Comparators are averaged equally (each country counts once).
#  - Scorecard items from outside sources (WHO, HIO, Politis, ILO)
#    are not calculated in this project; see README for links.
# ============================================================