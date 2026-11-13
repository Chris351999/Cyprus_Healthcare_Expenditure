# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 15_household_burden.R
# PURPOSE: Does the average person pay less than before GeSY,
#          counting their own GeSY contributions?
#          Direct burden = out-of-pocket + private insurance
#                          + own GeSY contribution
# INPUT:  data/processed/cystat_clean.RData
#         data/processed/health_real.csv   (from 09_real_prices.R)
#         HIO (OAY) annual report 2023 and budget 2022
#         R/03_labels.R
# OUTPUT: data/processed/household_burden.csv
#         outputs/figures/13_household_burden.png
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # colours
health_real <- read_csv("data/processed/health_real.csv", show_col_types = FALSE)


# 2. GeSY contributions (HIO annual report 2023, table 21) ----
#    No GeSY contributions existed before March 2019

gesy_contributions <- tribble(
  ~year, ~contrib_meur,
  2018,  0,
  2023,  1534.4
)

# Who pays the contributions? (HIO budget 2022, EUR million)
employees_pensioners <- 385.0   # employees, pensioners, other income earners
employers            <- 284.6
state                <- 587.1

# Share paid directly by people (the rest: employers and the state)
household_share <- employees_pensioners / (employees_pensioners + employers + state)
household_share    # about 0.31


# 3. What households pay directly, 2018 vs 2023 ---------------

household_burden <- financing %>%
  filter(year %in% c(2018, 2023),
         financing_code %in% c("HF.3", "HF.2.1")) %>%
  select(year, financing_code, value_meur) %>%
  pivot_wider(names_from = financing_code, values_from = value_meur) %>%
  rename(oop_meur = HF.3, insurance_meur = HF.2.1) %>%
  left_join(gesy_contributions, by = "year") %>%
  mutate(own_contrib_meur = household_share * contrib_meur) %>%
  left_join(select(health_real, year, population, price_factor, che_per_capita_real),
            by = "year") %>%
  mutate(
    oop_pp            = oop_meur         * 1e6 / population * price_factor,
    insurance_pp      = insurance_meur   * 1e6 / population * price_factor,
    own_contrib_pp    = own_contrib_meur * 1e6 / population * price_factor,
    direct_total_pp   = oop_pp + insurance_pp + own_contrib_pp,
    share_of_spending = 100 * direct_total_pp / che_per_capita_real
  )

household_burden %>%
  select(year, oop_pp, insurance_pp, own_contrib_pp, direct_total_pp,
         che_per_capita_real, share_of_spending)


# 4. Before vs after: how much did the direct burden change? --

household_burden %>%
  select(year, direct_total_pp) %>%
  pivot_wider(names_from = year, values_from = direct_total_pp, names_prefix = "y") %>%
  mutate(change_eur = y2023 - y2018,
         change_pct = 100 * (y2023 / y2018 - 1))


# 5. Chart: what the average person pays directly -------------

household_burden %>%
  select(year, `Out-of-pocket` = oop_pp,
         `Private insurance` = insurance_pp,
         `Own GeSY contribution` = own_contrib_pp) %>%
  pivot_longer(-year, names_to = "item", values_to = "eur") %>%
  mutate(item  = factor(item, levels = c("Own GeSY contribution",
                                         "Private insurance", "Out-of-pocket")),
         label = if_else(year == 2018, "2018\nbefore GeSY", "2023\nwith GeSY")) %>%
  ggplot(aes(x = label, y = eur, fill = item)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = if_else(eur > 30, paste0("\u20ac", round(eur)), "")),
            position = position_stack(vjust = 0.5), colour = "white", fontface = "bold") +
  scale_fill_manual(values = c("Own GeSY contribution" = col_pooled,
                               "Private insurance"     = "#9a9a9a",
                               "Out-of-pocket"         = col_private)) +
  labs(title    = "What the average person in Cyprus pays directly for health care",
       subtitle = "Per person per year, 2024 euros. Employer and state contributions not included.",
       x = NULL, y = "\u20ac per person", fill = NULL,
       caption  = "Data: CYSTAT, Eurostat, HIO annual report 2023 | Analysis: Christos Fakontis") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

ggsave("outputs/figures/13_household_burden.png", width = 8, height = 6.5, bg = "white")


# 6. Save -----------------------------------------------------

write_csv(household_burden, "data/processed/household_burden.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# All amounts per person per year, in 2024 euros.
#
# 1. Counting their own GeSY contribution, the average person pays
#    about the same as before GeSY.
#    Direct burden = out-of-pocket + private insurance + own contribution
#    2018: EUR 873 + 234 + 0   = EUR 1,107
#    2023: EUR 473 + 98  + 506 = EUR 1,078   (-EUR 29, -3%)
#
# 2. But they get much more care for it.
#    Total health spending per person: EUR 1,967 (2018) -> 2,714 (2023),
#    +38%. People now pay directly 40% of what is spent on them,
#    down from 56%. Employers and the state pay the difference.
#
# 3. The way people pay changed more than the amount.
#    Before: unpredictable bills when sick, plus expensive private
#    insurance (EUR 234 -> 98; many people dropped it).
#    Now: a fixed share of income every month, whether sick or not.
#    This is financial protection: no large bill at the worst moment.
#
# 4. Who gains and who pays?
#    These are averages. People who use care often (older people,
#    chronic patients, low incomes) gain much more. Healthy people
#    with higher incomes pay more in contributions than they save.
#    This is how social insurance works: risk is shared.
#
# Caveats:
#  - "Own contribution" is an ESTIMATE: total 2023 contributions
#    (HIO annual report, EUR 1,534m) x the share paid by employees,
#    pensioners and other earners in the HIO 2022 budget (31%).
#  - Per resident, including children and people without income;
#    contributions are in fact paid by earners.
#  - Employer and state contributions are not included, but people
#    pay them indirectly (through taxes and wages).
#  - One year after GeSY (2023), because contribution data for 2024
#    were not used.
# ============================================================