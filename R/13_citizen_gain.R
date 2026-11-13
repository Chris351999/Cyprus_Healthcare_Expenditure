# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 13_citizen_gain.R
# PURPOSE: What did GeSY mean for each person in Cyprus?
#          Out-of-pocket and pooled spending per person,
#          in 2024 euros, before and after GeSY
# INPUT:  data/processed/cystat_clean.RData
#         data/processed/health_real.csv   (from 09_real_prices.R)
#         data/processed/oop_by_care.csv   (from 05_out_of_pocket.R)
#         R/03_labels.R
# OUTPUT: data/processed/citizen_per_person.csv
#         data/processed/citizen_by_care.csv
#         outputs/figures/11_citizen_per_person.png
#         outputs/figures/12_citizen_by_care.png
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # care_names, colours

health_real <- read_csv("data/processed/health_real.csv", show_col_types = FALSE)
oop_by_care <- read_csv("data/processed/oop_by_care.csv", show_col_types = FALSE)


# 2. Out-of-pocket spending, total, by year -------------------

oop <- financing %>%
  filter(financing_code == "HF.3") %>%
  select(year, oop_meur = value_meur)


# 3. Per person, in 2024 euros --------------------------------
#    EUR million x 1,000,000 / population x price factor

per_person <- health_real %>%
  select(year, population, price_factor, public_meur) %>%
  left_join(oop, by = "year") %>%
  mutate(
    oop_per_person    = oop_meur    * 1e6 / population * price_factor,
    pooled_per_person = public_meur * 1e6 / population * price_factor
  )

per_person %>%
  select(year, oop_per_person, pooled_per_person) %>%
  print(n = Inf)


# 4. The headline numbers: 2018 vs 2024 -----------------------

before <- filter(per_person, year == 2018)
after  <- filter(per_person, year == 2024)

oop_saving   <- before$oop_per_person   - after$oop_per_person
oop_change   <- 100 * (after$oop_per_person / before$oop_per_person - 1)
pooled_extra <- after$pooled_per_person - before$pooled_per_person

cat("Out-of-pocket per person: EUR", round(before$oop_per_person), "(2018) ->",
    round(after$oop_per_person), "(2024)\n")
cat("Each person pays EUR", round(oop_saving), "less per year out-of-pocket (",
    round(oop_change), "%)\n")
cat("Pooled spending (taxes & GeSY contributions) per person: +EUR", round(pooled_extra), "\n")


# 5. Where did people save? By type of care, per person -------

citizen_by_care <- oop_by_care %>%
  left_join(select(health_real, year, population, price_factor), by = "year") %>%
  mutate(oop_per_person = oop_meur * 1e6 / population * price_factor) %>%
  select(year, care_name, oop_per_person)

citizen_by_care %>%
  filter(year %in% c(2018, 2024)) %>%
  pivot_wider(names_from = year, values_from = oop_per_person, names_prefix = "y") %>%
  mutate(saving = y2018 - y2024) %>%
  arrange(desc(saving))


# 6. Chart 1: out-of-pocket vs pooled, per person -------------

per_person %>%
  select(year,
         "Out-of-pocket (paid when sick)"      = oop_per_person,
         "Pooled (taxes & GeSY contributions)" = pooled_per_person) %>%
  pivot_longer(-year, names_to = "type", values_to = "eur") %>%
  ggplot(aes(x = year, y = eur, colour = type)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_colour_manual(values = c("Out-of-pocket (paid when sick)"      = col_private,
                                 "Pooled (taxes & GeSY contributions)" = col_pooled)) +
  scale_x_continuous(breaks = 2010:2024) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title    = "What GeSY changed for each person in Cyprus",
       subtitle = paste0("Per person, 2024 euros. Out-of-pocket: EUR ",
                         round(oop_saving), " less per year in 2024 than in 2018"),
       x = NULL, y = "EUR per person per year (2024 prices)", colour = NULL,
       caption = "Source: CYSTAT, Eurostat. Grey band = GeSY roll-out.") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("outputs/figures/11_citizen_per_person.png", width = 9, height = 5.5, bg = "white")


# 7. Chart 2: out-of-pocket per person by type of care --------

citizen_by_care %>%
  filter(year %in% c(2018, 2024)) %>%
  ggplot(aes(x = oop_per_person, y = fct_rev(factor(care_name, levels = care_names)),
             fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2018" = "grey70", "2024" = col_pooled),
                    labels = c("2018" = "2018 (before GeSY)", "2024" = "2024")) +
  labs(title    = "Out-of-pocket spending per person, by type of care",
       subtitle = "2018 (before GeSY) vs 2024, in 2024 euros",
       x = "EUR per person per year", y = NULL, fill = NULL,
       caption = "Source: CYSTAT, Eurostat. Hospital care = inpatient + day care.") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("outputs/figures/12_citizen_by_care.png", width = 9, height = 5.5, bg = "white")


# 8. Save the results for the dashboard -----------------------

write_csv(per_person,      "data/processed/citizen_per_person.csv")
write_csv(citizen_by_care, "data/processed/citizen_by_care.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# All amounts per person per year, in 2024 euros.
#
# 1. Each person pays EUR 385 less out-of-pocket.
#    Out-of-pocket: EUR 873 (2018) -> 488 (2024), -44%.
#    Most of the fall came in 2019-2021 (GeSY roll-out); since 2021
#    it has stayed at about EUR 460-490.
#
# 2. But pooled spending per person rose by EUR 1,455.
#    Pooled (taxes + GeSY contributions): EUR 827 (2018) -> 2,282 (2024).
#    For every EUR 1 saved at the point of care, about EUR 3.8 more
#    is now paid through taxes and contributions.
#
# 3. In total, about EUR 1,070 more per person goes to health.
#    Out-of-pocket + pooled: about EUR 1,700 (2018) -> 2,770 (2024).
#    GeSY did not make health care cheaper. It made the system bigger
#    and changed how it is paid: by everyone, by income, all year,
#    instead of by the sick person when they need care.
#
# 4. Where people saved the most (out-of-pocket, 2018 -> 2024):
#    Medicines        -EUR 148  (272 -> 124)
#    Outpatient care  -EUR 121  (330 -> 209)
#    Hospital care    -EUR 86   (121 -> 35)
#    Lab tests        -EUR 80   (106 -> 26)
#
# 5. Where people now pay more:
#    Long-term care   +EUR 44   (17 -> 61), almost 4 times more.
#    Older people needing care are the group GeSY protects least.
#
# 6. Who gains and who pays?
#    These are averages. People who are often ill (older people,
#    chronic patients) gain much more than EUR 385. Healthy people
#    with higher incomes pay more in contributions than they save.
#    This is how insurance works: risk is shared.
#    (Household burden is estimated in a separate script.)
#
# Caveats:
#  - Averages per resident, not per household or by income group.
#  - Pooled money includes taxes and contributions of employers and
#    the state, not only what each person pays directly.
#  - Deflated with the general consumer price index (HICP).
#  - 2023-2024 provisional; 2021 out-of-pocket filled.
# ============================================================
