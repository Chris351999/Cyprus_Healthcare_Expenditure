# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 10_unmet_needs.R
# PURPOSE: Did people skip medical care because it was too
#          expensive? By income group, before and after GeSY
# INPUT:  data/raw/eurostat_unmet_needs.csv   (from 01_download.R,
#         Eurostat hlth_silc_08, EU-SILC survey)
#         R/03_labels.R
# OUTPUT: data/processed/unmet_needs.csv
#         outputs/figures/09_unmet_needs_cyprus.png
# ============================================================


# 1. Load packages and labels ---------------------------------

library(tidyverse)
source("R/03_labels.R")   # colours


# 2. Countries ------------------------------------------------

countries <- c("CY" = "Cyprus", "EL" = "Greece", "MT" = "Malta",
               "PT" = "Portugal", "EU27_2020" = "EU-27")


# 3. Read the survey data (people aged 16+, both sexes, %) ----

unmet_raw <- read_csv("data/raw/eurostat_unmet_needs.csv", show_col_types = FALSE)


# 4. Inspect columns and codes --------------------------------

names(unmet_raw)

# Find the income column (its name contains "quant") and rename it
income_col <- names(unmet_raw)[str_detect(names(unmet_raw), "quant")]
unmet_raw  <- rename(unmet_raw, income_code = all_of(income_col))

distinct(unmet_raw, income_code)
distinct(unmet_raw, reason)


# 5. Keep the reasons and income groups we need ----------------

unmet_needs <- unmet_raw %>%
  filter(reason      %in% c("TXP", "TXP_TFAR_WLIST"),
         income_code %in% c("TOTAL", "QU1", "QU2", "QU3", "QU4", "QU5"),
         year >= 2010) %>%
  mutate(
    country = factor(countries[geo], levels = countries),
    reason  = recode(reason,
                     "TXP"            = "Too expensive",
                     "TXP_TFAR_WLIST" = "Too expensive, too far or waiting list"),
    income  = factor(recode(income_code,
                            "TOTAL" = "All people",
                            "QU1"   = "Poorest 20%",
                            "QU2"   = "2nd quintile",
                            "QU3"   = "3rd quintile",
                            "QU4"   = "4th quintile",
                            "QU5"   = "Richest 20%"),
                     levels = c("All people", "Poorest 20%", "2nd quintile",
                                "3rd quintile", "4th quintile", "Richest 20%")),
    year    = as.integer(year)
  ) %>%
  select(year, country, reason, income, value = values)

# How many rows did we keep? (should be several hundred, not 0)
nrow(unmet_needs)


# 6. Cyprus: "too expensive", poorest vs richest, by year ------

unmet_needs %>%
  filter(country == "Cyprus", reason == "Too expensive",
         income %in% c("All people", "Poorest 20%", "Richest 20%")) %>%
  pivot_wider(names_from = income, values_from = value) %>%
  select(-country, -reason) %>%
  print(n = Inf)


# 7. Poorest 20%: Cyprus vs other countries, 2018 vs latest ----

unmet_needs %>%
  filter(reason == "Too expensive", income == "Poorest 20%",
         year %in% c(2018, 2024)) %>%
  select(year, country, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "y")


# 8. Chart: Cyprus, by income group ---------------------------

unmet_needs %>%
  filter(country == "Cyprus", reason == "Too expensive",
         income %in% c("All people", "Poorest 20%", "Richest 20%")) %>%
  ggplot(aes(x = year, y = value, colour = income)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_colour_manual(values = c("All people"  = "grey50",
                                 "Poorest 20%" = col_private,
                                 "Richest 20%" = col_pooled)) +
  scale_x_continuous(breaks = 2010:2025) +
  labs(title = "Skipped medical care because it was too expensive, Cyprus",
       subtitle = "% of people aged 16+, grey band = GeSY roll-out",
       x = NULL, y = "%", colour = NULL,
       caption = "Source: Eurostat, EU-SILC (hlth_silc_08). Self-reported survey data.") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("outputs/figures/09_unmet_needs_cyprus.png", width = 9, height = 5, bg = "white")


# 9. Save the result for the dashboard ------------------------

write_csv(unmet_needs, "data/processed/unmet_needs.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. During the crisis, many poor people skipped care because of cost.
#    Poorest 20% who skipped medical care as too expensive:
#    6-7.5% in 2010-2014 (peak 7.5% in 2014). Richest 20%: about 1%.
#
# 2. The improvement started BEFORE GeSY.
#    Poorest 20%: 7.5% (2014) -> 3.2% (2015) -> 3.0% (2018).
#    The first big drop came with the economic recovery, four years
#    before GeSY. So not all of the improvement is due to GeSY.
#
# 3. GeSY finished the job: cost is almost no longer a barrier.
#    Poorest 20%: 3.0% (2018) -> 0.4% (2021) -> 0.3% (2025).
#    All people:  1.4% (2018) -> 0.1% (2021-2025).
#
# 4. The gap between poor and rich has almost closed.
#    Difference poorest vs richest: 6.3 points (2014),
#    3.0 points (2018), 0.3 points (2025).
#    Access to care no longer depends much on income.
#
# 5. Cyprus is now among the best in this group.
#    Poorest 20%, 2024: Cyprus 0.4%, Malta 0.5%, EU-27 2.1%,
#    Portugal 2.8%, Greece 13.8%.
#    In 2018 Cyprus (3.0%) was worse than the EU average (2.5%).
#
# Caveats:
#  - Self-reported survey (EU-SILC): people's own answers.
#  - Very small values (0.1-0.4%) rest on few survey answers and are
#    less precise.
#  - This measures only cost. Waiting lists and distance are a
#    separate reason (also saved in unmet_needs.csv).
#  - COVID-19 (2020-2021) changed how people used health care.
#  - Descriptive only: not proof that GeSY caused the change.
# ============================================================