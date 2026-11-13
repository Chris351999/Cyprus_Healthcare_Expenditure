# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 16_money_flows.R
# PURPOSE: How money flows from payers to providers to types
#          of care (data for a Sankey diagram)
# INPUT:  data/processed/cystat_clean.RData
#         R/03_labels.R
# OUTPUT: data/processed/flows_financing_provider.csv
#         data/processed/flows_provider_care.csv
#         outputs/figures/14_sankey_2024.html
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
library(plotly)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # financing_names, provider_names, care_names


# 2. Who pays each provider (financing -> provider) -----------

flows_financing_provider <- provider_financing %>%
  filter(financing_code %in% names(financing_names),
         provider_code  %in% names(provider_names),
         value_meur > 0) %>%
  mutate(from = financing_names[financing_code],
         to   = provider_names[provider_code]) %>%
  select(year, from, to, value_meur)


# 3. What each provider delivers (provider -> type of care) ---

flows_provider_care <- care_provider %>%
  filter(provider_code %in% names(provider_names),
         care_code     %in% names(care_names),
         value_meur > 0) %>%
  mutate(from = provider_names[provider_code],
         to   = care_names[care_code]) %>%
  select(year, from, to, value_meur)


# 4. Check: both flows add up to the same total ---------------

bind_rows(
  flows_financing_provider %>% mutate(step = "payer -> provider"),
  flows_provider_care      %>% mutate(step = "provider -> care")
) %>%
  group_by(year, step) %>%
  summarise(total = sum(value_meur), .groups = "drop") %>%
  pivot_wider(names_from = step, values_from = total) %>%
  print(n = Inf)


# 5. Who pays the hospitals? (2018 vs 2024) -------------------

flows_financing_provider %>%
  filter(to == "Hospitals", year %in% c(2018, 2024)) %>%
  group_by(year) %>%
  mutate(share = round(100 * value_meur / sum(value_meur), 1)) %>%
  arrange(year, desc(share))


# 6. Who pays the pharmacies? (2018 vs 2024) ------------------

flows_financing_provider %>%
  filter(to == "Pharmacies & medical goods", year %in% c(2018, 2024)) %>%
  group_by(year) %>%
  mutate(share = round(100 * value_meur / sum(value_meur), 1)) %>%
  arrange(year, desc(share))


# 7. Sankey diagram for 2024 ----------------------------------

flows_2024 <- bind_rows(flows_financing_provider, flows_provider_care) %>%
  filter(year == 2024)

nodes <- unique(c(flows_2024$from, flows_2024$to))

sankey <- plot_ly(
  type = "sankey",
  node = list(label = nodes, pad = 15, thickness = 15),
  link = list(source = match(flows_2024$from, nodes) - 1,
              target = match(flows_2024$to,   nodes) - 1,
              value  = flows_2024$value_meur)
) %>%
  layout(title = "Where the money flows: payers -> providers -> type of care, 2024 (EUR million)")

sankey

# Save as an interactive web page (needs the htmlwidgets package)
if (requireNamespace("htmlwidgets", quietly = TRUE)) {
  htmlwidgets::saveWidget(sankey, "outputs/figures/14_sankey_2024.html", selfcontained = FALSE)
}


# 8. Save the results for the dashboard -----------------------

write_csv(flows_financing_provider, "data/processed/flows_financing_provider.csv")
write_csv(flows_provider_care,      "data/processed/flows_provider_care.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. The money flows add up in every year.
#    Payers -> providers = providers -> types of care, 2010-2024
#    (e.g. EUR 2,782m in 2024). No money is lost or counted twice.
#
# 2. GeSY now pays most hospital bills.
#    Who pays the hospitals:     2018    2024
#    Government                   70%     32%
#    GeSY                          0%     62%
#    Out-of-pocket                18%      3%
#    Voluntary insurance          11%      3%
#    Patients and private insurers paid about 30% of hospital revenue
#    before GeSY, and about 6% in 2024.
#
# 3. Pharmacies moved from patients' pockets to GeSY.
#    Who pays the pharmacies:    2018    2024
#    Out-of-pocket                88%     30%
#    GeSY                          0%     67%
#    Voluntary insurance          10%      2%
#    In 2018 almost 9 of every 10 euros spent in pharmacies came from
#    patients; in 2024 it was 3 of 10.
#
# 4. Where patients still pay (Sankey, 2024).
#    The biggest flow is GeSY -> hospitals -> hospital care.
#    Out-of-pocket money now goes mainly to ambulatory care (private
#    doctors, dentists, physiotherapists) and pharmacies: this is
#    where the remaining cost for patients sits.
#
# Caveats:
#  - Current prices. 2023-2024 provisional.
#  - 2021 out-of-pocket filled as total minus other payers.
#  - Providers can deliver several types of care (e.g. hospitals also
#    run outpatient clinics and labs).
# ============================================================