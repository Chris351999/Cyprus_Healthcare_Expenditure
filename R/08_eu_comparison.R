# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 08_eu_comparison.R
# PURPOSE: Compare Cyprus with Greece, Malta, Portugal and EU
# INPUT:  data/raw/eurostat_spending.csv   (from 01_download.R,
#         Eurostat hlth_sha11_hf, all EU countries)
# OUTPUT: data/processed/eu_comparison.csv
#         outputs/figures/07_eu_comparison.png
# ============================================================


# 1. Load packages --------------------------------------------

library(tidyverse)


# 2. Countries to compare -------------------------------------

countries <- c("CY"        = "Cyprus",
               "EL"        = "Greece",
               "MT"        = "Malta",
               "PT"        = "Portugal",
               "EU27_2020" = "EU-27")


# 3. Read the Eurostat data (downloaded in 01_download.R) -----
#    TOT_HF = all schemes, HF1 = public, HF3 = out-of-pocket
#    PC_GDP = % of GDP, PPS_HAB = per person in PPS,
#    PC_CHE = % of current health expenditure

eu_raw <- read_csv("data/raw/eurostat_spending.csv", show_col_types = FALSE) %>%
  filter(geo %in% names(countries))


# 4. Keep the four indicators we need -------------------------

eu_comparison <- eu_raw %>%
  mutate(
    indicator = case_when(
      icha11_hf == "TOT_HF" & unit == "PC_GDP"  ~ "Health spending, % of GDP",
      icha11_hf == "TOT_HF" & unit == "PPS_HAB" ~ "Health spending per person (PPS)",
      icha11_hf == "HF1"    & unit == "PC_CHE"  ~ "Public share (%)",
      icha11_hf == "HF3"    & unit == "PC_CHE"  ~ "Out-of-pocket share (%)"
    ),
    country = factor(countries[geo], levels = countries),
    year    = as.integer(year)
  ) %>%
  filter(!is.na(indicator), year >= 2010) %>%
  select(year, country, indicator, value = values)


# 5. Latest year for each country and indicator ---------------

eu_comparison %>%
  filter(!is.na(value)) %>%
  group_by(country, indicator) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  print(width = Inf)


# 6. Before GeSY (2018) vs latest year, Cyprus vs EU ----------

eu_comparison %>%
  filter(country %in% c("Cyprus", "EU-27"), year %in% c(2018, 2023)) %>%
  pivot_wider(names_from = c(country, year), values_from = value) %>%
  print(width = Inf)


# 7. Chart: four small panels, one per indicator --------------

ggplot(eu_comparison, aes(x = year, y = value, colour = country, linetype = country)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ indicator, scales = "free_y") +
  scale_colour_manual(values = c("Cyprus"   = "#d6293e",
                                 "Greece"   = "#2a78d6",
                                 "Malta"    = "#1baf7a",
                                 "Portugal" = "#eda100",
                                 "EU-27"    = "grey40")) +
  scale_linetype_manual(values = c("Cyprus" = "solid", "Greece" = "solid",
                                   "Malta"  = "solid", "Portugal" = "solid",
                                   "EU-27"  = "dashed")) +
  labs(title = "Cyprus compared with Greece, Malta, Portugal and the EU",
       x = NULL, y = NULL, colour = NULL, linetype = NULL,
       caption = "Source: Eurostat (hlth_sha11_hf). Grey band = GeSY roll-out. PPS = purchasing power standard.") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("outputs/figures/07_eu_comparison.png", width = 11, height = 7, bg = "white")


# 8. Save the result for the dashboard ------------------------

write_csv(eu_comparison, "data/processed/eu_comparison.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. Cyprus moved from outlier to EU average in who pays.
#    Public share:   42% (2018) -> 78% (2023); EU-27: 79% -> 80%.
#    Out-of-pocket:  44% (2018) -> 17% (2023); EU-27: 16% -> 15%.
#    Before GeSY, Cyprus had the highest out-of-pocket share in this
#    group (higher than Greece, Malta and Portugal). Now it is close
#    to the EU average.
#
# 2. Patients in Cyprus are now better protected than in the
#    comparison countries.
#    Out-of-pocket share, latest year: Cyprus 17%, Portugal 28%,
#    Malta 30%, Greece 34%. Public share: Cyprus 79%, Malta 68%,
#    Portugal 63%, Greece 61%.
#
# 3. Cyprus spends the least of the group as a share of GDP.
#    2024: Cyprus 8.0%, Greece 8.4%, Malta 8.7%, EU-27 10.2%,
#    Portugal 10.4% (2025).
#
# 4. The per-person gap with the EU is closing, but still large.
#    Per person in PPS (adjusted for price differences):
#    2018: Cyprus 1,795 vs EU 2,995 -> 60% of the EU average.
#    2023: Cyprus 2,793 vs EU 3,826 -> 73% of the EU average.
#    2024: Cyprus 3,042 vs EU 4,075 -> 75%.
#    Cyprus is above Greece (2,291) but below Malta and Portugal.
#
# 5. What this means.
#    In about two years GeSY gave Cyprus an EU-style financing
#    structure, while spending less of its economy on health than
#    the EU average. Whether this is "efficient" depends on outcomes
#    (access, waiting times, mortality), checked in later scripts.
#
# Caveats:
#  - Eurostat uses an older GDP vintage: Cyprus 2024 = 8.0% of GDP
#    here, 7.7% with the latest GDP (07_gdp_per_capita.R).
#  - EU-27 and Malta series start in 2014. Latest year differs by
#    country (Portugal: 2025 for some indicators).
#  - Descriptive comparison: countries differ in age structure,
#    income and health needs.
# ============================================================