# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 09_real_prices.R
# PURPOSE: Remove inflation: express all amounts in 2024 euros
# INPUT:  data/processed/health_gdp.csv     (from 07_gdp_per_capita.R)
#         data/raw/eurostat_prices.csv      (from 01_download.R,
#         Eurostat HICP, prc_hicp_aind)
#         R/03_labels.R
# OUTPUT: data/processed/price_factors.csv
#         data/processed/health_real.csv
#         outputs/figures/08_real_per_capita.png
# ============================================================


# 1. Load packages and labels ---------------------------------

library(tidyverse)
source("R/03_labels.R")   # colours


# 2. Consumer price index of Cyprus (all items, annual average)

hicp <- read_csv("data/raw/eurostat_prices.csv", show_col_types = FALSE) %>%
  transmute(year = as.integer(year), hicp = values) %>%
  filter(year >= 2010, year <= 2024)


# 3. Price factor: how many 2024 euros is one euro of each year?
#    e.g. if prices rose 20% since 2015, 1 euro of 2015 = 1.20 euros of 2024

price_factors <- hicp %>%
  mutate(price_factor = hicp[year == 2024] / hicp)

price_factors


# 4. Health spending in 2024 euros ----------------------------

health_real <- read_csv("data/processed/health_gdp.csv", show_col_types = FALSE) %>%
  left_join(price_factors, by = "year") %>%
  mutate(
    che_real_meur       = che_meur       * price_factor,
    che_per_capita_real = che_per_capita * price_factor
  )

# Nominal vs real, per person
health_real %>%
  filter(year %in% c(2010, 2015, 2018, 2024)) %>%
  select(year, che_per_capita, che_per_capita_real)


# 5. Real growth: how much did spending really rise? ----------

health_real %>%
  filter(year %in% c(2010, 2015, 2018, 2024)) %>%
  select(year, che_per_capita_real) %>%
  mutate(change_pct = round(100 * (che_per_capita_real / lag(che_per_capita_real) - 1)))


# 6. Chart: nominal vs real spending per person ---------------

health_real %>%
  select(year,
         "Current prices" = che_per_capita,
         "2024 prices (inflation removed)" = che_per_capita_real) %>%
  pivot_longer(-year, names_to = "prices", values_to = "eur") %>%
  ggplot(aes(x = year, y = eur, colour = prices)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_colour_manual(values = c("Current prices" = "grey60",
                                 "2024 prices (inflation removed)" = col_pooled)) +
  scale_x_continuous(breaks = 2010:2024) +
  scale_y_continuous(limits = c(0, 3000)) +
  labs(title = "Health spending per person: with and without inflation",
       x = NULL, y = "EUR per person", colour = NULL,
       caption = "Source: CYSTAT, Eurostat (demo_gind, prc_hicp_aind). Grey band = GeSY roll-out.") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("outputs/figures/08_real_per_capita.png", width = 9, height = 5, bg = "white")


# 7. Save the results for the dashboard -----------------------
#    price_factors lets the dashboard switch ANY euro chart to 2024 prices

write_csv(price_factors, "data/processed/price_factors.csv")
write_csv(health_real,   "data/processed/health_real.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. Prices were flat for a decade, then rose fast.
#    The consumer price index barely moved from 2012 to 2021
#    (prices even fell slightly in 2015-2016), then rose by about
#    15% in 2021-2024. One euro of 2018 = EUR 1.17 of 2024.
#
# 2. The financial crisis cut real health spending.
#    Per person, in 2024 prices: EUR 1,902 (2010) -> 1,658 (2015), -13%.
#
# 3. Spending recovered before GeSY.
#    EUR 1,658 (2015) -> 1,967 (2018), +19%.
#
# 4. The rise with GeSY is real, not just inflation.
#    EUR 1,967 (2018) -> 2,879 (2024), +46% after removing inflation
#    (+71% in current prices). About a third of the euro growth
#    since 2018 was inflation; two thirds was real growth.
#
# 5. The real peak was in 2021 (about EUR 2,870 per person).
#    In 2022-2023 real spending per person fell slightly (about 2,730
#    and 2,710): euros kept rising, but inflation rose faster.
#    In 2024 it reached a new high (EUR 2,879).
#
# 6. Why this matters.
#    Euro growth rates overstate the real change. All euro charts in
#    the dashboard can be switched to 2024 prices with
#    price_factors.csv.
#
# Caveats:
#  - Deflated with the general consumer price index (HICP, all items),
#    not a health-specific price index.
#  - 2023-2024 provisional.
# ============================================================
