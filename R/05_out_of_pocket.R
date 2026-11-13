# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 05_out_of_pocket.R
# PURPOSE: How much do patients pay out-of-pocket for each
#          type of care, before GeSY (2018) and after (2024)?
# INPUT:  data/processed/cystat_clean.RData
#         R/03_labels.R
# OUTPUT: data/processed/oop_by_care.csv
#         outputs/figures/02_oop_by_care.png
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # care_names (9 types), colours


# 2. Check: do the 9 types of care add up to the total? -------

care_financing %>%
  filter(financing_code == "All HF") %>%
  group_by(year) %>%
  summarise(
    total      = value_meur[care_code == "All HC"],
    sum_of_9   = sum(value_meur[care_code %in% names(care_names)]),
    difference = round(total - sum_of_9, 1)
  )


# 3. Total and out-of-pocket spending, per type of care -------

care_total <- care_financing %>%
  filter(care_code %in% names(care_names), financing_code == "All HF") %>%
  select(year, care_code, total_meur = value_meur)

care_oop <- care_financing %>%
  filter(care_code %in% names(care_names), financing_code == "HF.3") %>%
  select(year, care_code, oop_meur = value_meur)


# 4. Put them together and compute the out-of-pocket share ----

oop_by_care <- left_join(care_total, care_oop, by = c("year", "care_code")) %>%
  mutate(
    care_name   = factor(care_names[care_code], levels = care_names),
    oop_share   = 100 * oop_meur / total_meur,
    provisional = year >= 2023
  ) %>%
  select(year, care_code, care_name, total_meur, oop_meur, oop_share, provisional)


# 5. Look at the numbers: before GeSY (2018) vs 2024 ----------

oop_by_care %>%
  filter(year %in% c(2018, 2024)) %>%
  select(year, care_name, oop_meur, oop_share) %>%
  pivot_wider(names_from = year, values_from = c(oop_meur, oop_share))


# 6. Chart: out-of-pocket share, 2018 vs 2024 -----------------

oop_by_care %>%
  filter(year %in% c(2018, 2024)) %>%
  ggplot(aes(x = oop_share, y = fct_rev(care_name), fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2018" = "grey70", "2024" = col_pooled),
                    labels = c("2018" = "2018 (before GeSY)", "2024" = "2024")) +
  labs(title    = "How much do patients pay themselves?",
       subtitle = "Out-of-pocket share of spending, by type of care",
       x = "% paid out-of-pocket", y = NULL, fill = NULL,
       caption  = "Source: CYSTAT (2160015E). 2024 provisional. Hospital care = inpatient + day care.") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("outputs/figures/02_oop_by_care.png", width = 9, height = 5, bg = "white")


# 7. Save the result for the dashboard ------------------------

write_csv(oop_by_care, "data/processed/oop_by_care.csv")


# 8. Robustness checks ----------------------------------------

# 8a. Share AND amount: out-of-pocket in EUR million, 2018 vs 2024
#     (a high % on a small amount matters less for patients overall)
oop_by_care %>%
  filter(year %in% c(2018, 2024)) %>%
  select(year, care_name, total_meur, oop_meur, oop_share) %>%
  arrange(care_name, year) %>%
  print(n = Inf)

# 8b. Long-term care, year by year: did the rise happen suddenly
#     in 2021 (the year of the methodology change)?
oop_by_care %>%
  filter(care_name == "Long-term care") %>%
  select(year, total_meur, oop_meur, oop_share)

# 8c. Long-term care split into its four parts
#     HC.3.1 inpatient, HC.3.2 day, HC.3.3 outpatient, HC.3.4 home
care_financing %>%
  filter(care_code %in% c("HC.3.1", "HC.3.2", "HC.3.3", "HC.3.4"),
         financing_code %in% c("All HF", "HF.3")) %>%
  select(year, care_code, financing_code, value_meur) %>%
  pivot_wider(names_from = financing_code, values_from = value_meur) %>%
  mutate(oop_share = 100 * HF.3 / `All HF`) %>%
  print(n = Inf)

# ============================================================
# FINDINGS
# ============================================================
#
# 1. Patients pay much less at the point of care.
#    Total out-of-pocket fell from EUR 656m (2018) to EUR 472m (2024),
#    a 28% drop, while total health spending almost doubled
#    (EUR 1,480m -> 2,782m).
#
# 2. Hospital care is now almost free at the point of use.
#    Out-of-pocket share: 18.7% -> 2.8% (EUR 91m -> 34m).
#    Out of every EUR 100 spent in hospitals, patients now pay EUR 3.
#    Meanwhile hospital spending grew from EUR 489m to 1,212m:
#    the difference is paid by GeSY, i.e. by everyone's contributions.
#
# 3. The biggest gains for patients: medicines and lab tests.
#    Medicines: 76% -> 26% out-of-pocket (EUR 204m -> 120m),
#               the largest drop in euros (-EUR 84m).
#    Lab tests: 49% -> 12% (EUR 80m -> 25m).
#
# 4. Outpatient care is the main remaining cost for patients.
#    Its share halved (59% -> 31%), but at EUR 202m it is still the
#    largest out-of-pocket item: 43% of everything patients pay
#    themselves in 2024 (private doctors, dentists, physiotherapy,
#    co-payments).
#
# 5. Long-term care moved in the opposite direction.
#    Out-of-pocket share: 19.5% (2018) -> 43.7% (2024),
#    from EUR 13m to EUR 59m, almost 5 times more.
#    - It was stable at about 19-20% from 2010 to 2020.
#    - It tripled in one year (2021: EUR 13.5m -> 39.2m), the year
#      CYSTAT changed its method, so part of the jump is a data break.
#    - But it kept rising after: EUR 39m (2021) -> 59m (2024), +51%.
#      This part is a real increase.
#
# 6. The rise is mainly in home-based long-term care (HC.3.4).
#    Families paid 23% of it in 2020 and 51% in 2024
#    (EUR 9.5m -> 43.5m). Nursing homes (HC.3.1) also rose: 17% -> 39%.
#    Care for older people is largely outside GeSY: with an ageing
#    population, families pay more and more from their own pocket.
#
# 7. Some items GeSY barely touched.
#    Medical devices (glasses, hearing aids, orthoses) are still
#    about 73% out-of-pocket, the same as before GeSY (70%).
#
# Caveats:
#  - Current prices (not adjusted for inflation). 2023-2024 provisional.
#  - Break in series in 2021: the long-term care jump is partly method.
#  - 2021 out-of-pocket filled as total minus other payers.
#  - Hospital care = inpatient + day care (joined in 02_clean.R).
#  - Descriptive only: not proof that GeSY caused each change.
# ============================================================
