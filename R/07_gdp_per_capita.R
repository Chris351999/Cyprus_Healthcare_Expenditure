# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 07_gdp_per_capita.R
# PURPOSE: Health spending as % of GDP and per person
# INPUT:  data/processed/cystat_clean.RData
#         data/raw/eurostat_gdp.csv          (from 01_download.R)
#         data/raw/eurostat_population.csv   (from 01_download.R)
#         R/03_labels.R
# OUTPUT: data/processed/health_gdp.csv
#         outputs/figures/05_pct_gdp.png
#         outputs/figures/06_per_capita.png
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # public_codes, private_codes, colours


# 2. GDP of Cyprus, current prices, EUR million ---------------

gdp <- read_csv("data/raw/eurostat_gdp.csv", show_col_types = FALSE) %>%
  transmute(year = as.integer(year), gdp_meur = values)


# 3. Population of Cyprus on 1 January ------------------------

population <- read_csv("data/raw/eurostat_population.csv", show_col_types = FALSE) %>%
  transmute(year = as.integer(year), population = values)


# 4. Health spending: total, public, private ------------------

che <- care_financing %>%
  filter(care_code == "All HC") %>%
  group_by(year) %>%
  summarise(
    che_meur     = sum(value_meur[financing_code == "All HF"]),
    public_meur  = sum(value_meur[financing_code %in% public_codes]),
    private_meur = sum(value_meur[financing_code %in% private_codes])
  )


# 5. Combine and compute the indicators -----------------------

health_gdp <- che %>%
  left_join(gdp,        by = "year") %>%
  left_join(population, by = "year") %>%
  mutate(
    che_pct_gdp     = 100 * che_meur     / gdp_meur,
    public_pct_gdp  = 100 * public_meur  / gdp_meur,
    private_pct_gdp = 100 * private_meur / gdp_meur,
    che_per_capita  = che_meur * 1e6 / population,     # EUR per person
    provisional     = year >= 2023
  )

health_gdp %>%
  select(year, gdp_meur, population, che_pct_gdp, public_pct_gdp,
         private_pct_gdp, che_per_capita) %>%
  print(n = Inf)


# 6. Check: any year without GDP or population? ---------------

health_gdp %>%
  filter(is.na(gdp_meur) | is.na(population)) %>%
  select(year, gdp_meur, population)


# 7. Charts ---------------------------------------------------

ggplot(health_gdp, aes(x = year, y = che_pct_gdp)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_line(linewidth = 1, colour = col_pooled) +
  geom_point(colour = col_pooled) +
  scale_x_continuous(breaks = 2010:2024) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(title = "Health spending as a share of GDP, Cyprus",
       x = NULL, y = "% of GDP",
       caption = "Source: CYSTAT (2160015E), Eurostat (nama_10_gdp). Grey band = GeSY roll-out.") +
  theme_minimal()

ggsave("outputs/figures/05_pct_gdp.png", width = 9, height = 5, bg = "white")

ggplot(health_gdp, aes(x = year, y = che_per_capita)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_line(linewidth = 1, colour = col_pooled) +
  geom_point(colour = col_pooled) +
  scale_x_continuous(breaks = 2010:2024) +
  scale_y_continuous(limits = c(0, 3000)) +
  labs(title = "Health spending per person, Cyprus",
       x = NULL, y = "EUR per person (current prices)",
       caption = "Source: CYSTAT (2160015E), Eurostat (demo_gind). Grey band = GeSY roll-out.") +
  theme_minimal()

ggsave("outputs/figures/06_per_capita.png", width = 9, height = 5, bg = "white")


# 8. Save the result for the dashboard ------------------------

write_csv(health_gdp, "data/processed/health_gdp.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. Before GeSY, Cyprus spent about 6.5-6.9% of GDP on health.
#    The small rise in 2013-2014 (6.5% -> 6.9%) was not more spending:
#    GDP fell in the financial crisis (EUR 19.5bn -> 17.5bn), so the
#    same spending became a bigger share of a smaller economy.
#
# 2. The peak came in 2020-2021: 8.7% and 8.9% of GDP.
#    Two things at once: GeSY phase 2 (hospitals, June 2020) raised
#    spending, and COVID-19 cut GDP in 2020 (EUR 23.4bn -> 22.4bn).
#
# 3. Since 2022 the share fell back to about 7.6-7.7%, but spending
#    did NOT fall. Spending kept rising (EUR 2,279m -> 2,782m), but GDP
#    grew faster (EUR 25.7bn -> 36.3bn, +41%, helped by inflation).
#
# 4. Pooled money doubled as a share of the economy.
#    Pooled (taxes + GeSY): 2.9% of GDP (2018) -> 6.1% (2024).
#    Private:               3.9% of GDP (2018) -> 1.6% (2024).
#    The cost moved from patients' pockets to taxes and contributions.
#
# 5. Spending per person: down in the crisis, up strongly with GeSY.
#    EUR 1,544 (2010) -> 1,409 (2014, -9%) -> 1,684 (2018)
#    -> 2,879 (2024), +71% since 2018.
#    The population also grew: 879,000 (2018) -> 966,000 (2024), +10%.
#
# Caveats:
#  - Current prices: part of the per-person rise is inflation
#    (real prices are calculated in a later script).
#  - GDP is revised over time. With the latest Eurostat data, 2024 is
#    7.7% of GDP; older GDP vintages gave about 8.0%.
#  - 2023-2024 provisional. COVID-19 affects 2020-2022.
# ============================================================