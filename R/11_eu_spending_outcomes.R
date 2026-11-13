# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 11_eu_spending_outcomes.R
# PURPOSE: All 27 EU countries: health spending and health
#          outcomes (life expectancy, avoidable mortality)
# INPUT:  data/raw/eurostat_spending.csv          (hlth_sha11_hf)
#         data/raw/eurostat_life_expectancy.csv   (demo_mlexpec)
#         data/raw/eurostat_mortality.csv         (hlth_cd_apr)
#         all from 01_download.R
# OUTPUT: data/processed/eu_spending_all.csv
#         data/processed/eu_outcomes.csv
#         outputs/figures/10_spending_vs_mortality.png
# ============================================================


# 1. Load packages --------------------------------------------

library(tidyverse)
library(eurostat)   # only for the list of EU countries, nothing is downloaded


# 2. EU countries (list included in the eurostat package) -----

eu_codes <- eu_countries$code
eu_names <- setNames(eu_countries$name, eu_countries$code)


# 3. Read the Eurostat data (downloaded in 01_download.R) -----

spend_raw <- read_csv("data/raw/eurostat_spending.csv", show_col_types = FALSE) %>%
  filter(geo %in% eu_codes)

life_raw <- read_csv("data/raw/eurostat_life_expectancy.csv", show_col_types = FALSE)

mort_raw <- read_csv("data/raw/eurostat_mortality.csv", show_col_types = FALSE)

# Check: none of these should be 0
nrow(spend_raw); nrow(life_raw); nrow(mort_raw)


# 4. Spending indicators, all EU countries --------------------

eu_spending_all <- spend_raw %>%
  mutate(indicator = case_when(
    icha11_hf == "TOT_HF" & unit == "PC_GDP"  ~ "Health spending, % of GDP",
    icha11_hf == "TOT_HF" & unit == "PPS_HAB" ~ "Health spending per person (PPS)",
    icha11_hf == "HF1"    & unit == "PC_CHE"  ~ "Public share (%)",
    icha11_hf == "HF3"    & unit == "PC_CHE"  ~ "Out-of-pocket share (%)"
  ),
  year = as.integer(year)) %>%
  filter(!is.na(indicator), year >= 2010) %>%
  mutate(country = eu_names[geo]) %>%
  select(year, geo, country, indicator, value = values)


# 5. Outcome indicators, all EU countries ---------------------

eu_outcomes <- bind_rows(
  life_raw %>%
    transmute(year, geo, indicator = "Life expectancy at birth (years)", value = values),
  mort_raw %>%
    transmute(year, geo,
              indicator = if_else(mortalit == "TRT",
                                  "Treatable mortality (per 100,000)",
                                  "Preventable mortality (per 100,000)"),
              value = values)
) %>%
  mutate(year = as.integer(year)) %>%
  filter(year >= 2010) %>%
  mutate(country = eu_names[geo]) %>%
  select(year, geo, country, indicator, value)


# 6. Spending vs treatable mortality, latest common year ------

spending_pps <- eu_spending_all %>%
  filter(indicator == "Health spending per person (PPS)") %>%
  select(year, geo, country, spending = value)

treatable <- eu_outcomes %>%
  filter(indicator == "Treatable mortality (per 100,000)") %>%
  select(year, geo, treatable = value)

scatter <- inner_join(spending_pps, treatable, by = c("year", "geo")) %>%
  filter(!is.na(spending), !is.na(treatable))

latest_common <- max(scatter$year[scatter$geo == "CY"])
latest_common

scatter_latest <- filter(scatter, year == latest_common)

# Where does Cyprus rank? (1 = lowest treatable mortality = best)
scatter_latest %>%
  arrange(treatable) %>%
  mutate(rank = row_number(), out_of = n()) %>%
  filter(geo %in% c("CY", "EL", "MT", "PT")) %>%
  select(rank, out_of, country, spending, treatable)

# EU averages in that year, for comparison
scatter_latest %>%
  summarise(mean_spending = mean(spending), mean_treatable = mean(treatable))


# 7. Chart: spending vs treatable mortality -------------------

highlight <- filter(scatter_latest, geo %in% c("CY", "EL", "MT", "PT"))

ggplot(scatter_latest, aes(x = spending, y = treatable)) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE,
              colour = "grey70", linetype = "dashed") +  geom_point(colour = "grey60", size = 2.5) +
  geom_point(data = highlight, aes(colour = geo == "CY"), size = 3.5) +
  geom_text(data = highlight, aes(label = country, colour = geo == "CY"),
            vjust = -1, fontface = "bold", size = 3.5) +
  scale_colour_manual(values = c("TRUE" = "#d6293e", "FALSE" = "grey30"), guide = "none") +
  labs(title = paste("Health spending vs treatable mortality, EU-27,", latest_common),
       subtitle = "Each dot is a country. Lower = fewer deaths that good health care could prevent",
       x = "Health spending per person (PPS)",
       y = "Treatable deaths per 100,000",
       caption = "Source: Eurostat (hlth_sha11_hf, hlth_cd_apr). Dashed line = log trend (diminishing returns).") +  theme_minimal()

ggsave("outputs/figures/10_spending_vs_mortality.png", width = 9, height = 6, bg = "white")


# 8. Save the results for the dashboard -----------------------

write_csv(eu_spending_all, "data/processed/eu_spending_all.csv")
write_csv(eu_outcomes,     "data/processed/eu_outcomes.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. Cyprus has one of the lowest treatable mortality rates in the EU.
#    2023: 62.9 deaths per 100,000 that good health care could have
#    prevented. Cyprus ranks 8th of 27 (1 = best).
#    Average of the 27 countries: 99.2.
#
# 2. It achieves this while spending less than average.
#    Health spending per person: 2,793 PPS in Cyprus vs 3,282 average
#    (about 85% of the average).
#
# 3. Better results than the comparison countries.
#    Portugal: 3,017 PPS, 74.2 deaths (14th).
#    Malta:    3,593 PPS, 74.3 deaths (15th) - spends 28% more than
#              Cyprus, with worse results.
#    Greece:   2,187 PPS, 84.1 deaths (17th).
#
# 4. Cyprus is far below the trend line.
#    For its level of spending, the trend predicts about 115 treatable
#    deaths per 100,000; Cyprus has 63, roughly half.
#
# 5. More money helps, but with diminishing returns.
#    Below about 2,500 PPS, countries that spend more have much lower
#    mortality. Above about 3,000 PPS, most countries level off at
#    55-70 deaths, however much they spend.
#
# 6. What this means.
#    More money does not automatically mean better outcomes.
#    Cyprus gets relatively good results for what it spends.
#
# Caveats:
#  - This is NOT evidence that GeSY caused low mortality: Cyprus
#    already had low treatable mortality before GeSY, and outcomes
#    react slowly (years) to changes in health systems.
#  - Ecological comparison: countries differ in age, diet, income,
#    smoking and how deaths are recorded.
#  - Averages are simple averages of the 27 countries (each country
#    counts once), not the EU-27 population-weighted figure.
#  - The high-spending outlier (about 5,400 PPS) is probably
#    Luxembourg, where many workers live abroad, which inflates
#    spending per resident.
# ============================================================