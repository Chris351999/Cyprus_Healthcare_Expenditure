# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 06_where_money_goes.R
# PURPOSE: Where does the money go? Spending by type of care
#          and by provider (hospitals vs outpatient)
# INPUT:  data/processed/cystat_clean.RData
#         R/03_labels.R
# OUTPUT: data/processed/spending_by_care.csv
#         data/processed/spending_by_provider.csv
#         outputs/figures/03_spending_by_care.png
#         outputs/figures/04_spending_by_provider.png
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # care_names, provider_names, colours


# 2. Spending by type of care ---------------------------------

spending_by_care <- care %>%
  filter(care_code %in% names(care_names)) %>%
  mutate(care_name   = factor(care_names[care_code], levels = care_names),
         provisional = year >= 2023) %>%
  group_by(year) %>%
  mutate(share = 100 * value_meur / sum(value_meur)) %>%
  ungroup() %>%
  select(year, care_code, care_name, value_meur, share, provisional, imputed)


# 3. Spending by provider -------------------------------------

spending_by_provider <- provider %>%
  filter(provider_code %in% names(provider_names)) %>%
  mutate(provider_name = factor(provider_names[provider_code], levels = provider_names),
         provisional   = year >= 2023) %>%
  group_by(year) %>%
  mutate(share = 100 * value_meur / sum(value_meur)) %>%
  ungroup() %>%
  select(year, provider_code, provider_name, value_meur, share, provisional)


# 4. Check: do the parts add up to the total? -----------------

# a) 9 types of care = total (All HC)
care %>%
  group_by(year) %>%
  summarise(
    total      = value_meur[care_code == "All HC"],
    sum_of_9   = sum(value_meur[care_code %in% names(care_names)]),
    difference = round(total - sum_of_9, 1)
  )

# b) 9 providers = total (All HP)
provider %>%
  group_by(year) %>%
  summarise(
    total      = value_meur[provider_code == "All HP"],
    sum_of_9   = sum(value_meur[provider_code %in% names(provider_names)]),
    difference = round(total - sum_of_9, 1)
  )


# 5. Look at the numbers: 2018 vs 2024 ------------------------

spending_by_care %>%
  filter(year %in% c(2018, 2024)) %>%
  select(year, care_name, value_meur, share) %>%
  pivot_wider(names_from = year, values_from = c(value_meur, share))

spending_by_provider %>%
  filter(year %in% c(2018, 2024)) %>%
  select(year, provider_name, value_meur, share) %>%
  pivot_wider(names_from = year, values_from = c(value_meur, share))


# 6. Growth 2018 -> 2024: which type of care grew fastest? ----

spending_by_care %>%
  filter(year %in% c(2018, 2024)) %>%
  select(year, care_name, value_meur) %>%
  pivot_wider(names_from = year, values_from = value_meur, names_prefix = "y") %>%
  mutate(growth_pct = round(100 * (y2024 - y2018) / y2018)) %>%
  arrange(desc(growth_pct))


# 7. Charts ---------------------------------------------------

ggplot(spending_by_care, aes(x = year, y = value_meur, fill = care_name)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_area() +
  scale_x_continuous(breaks = 2010:2024) +
  labs(title = "Where does the money go? By type of care",
       x = NULL, y = "EUR million", fill = NULL,
       caption = "Source: CYSTAT (2160012E). 2023-2024 provisional. Grey band = GeSY roll-out. Hospital care = inpatient + day care.") +
  theme_minimal()

ggsave("outputs/figures/03_spending_by_care.png", width = 9, height = 5, bg = "white")

ggplot(spending_by_provider, aes(x = year, y = value_meur, fill = provider_name)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  geom_area() +
  scale_x_continuous(breaks = 2010:2024) +
  labs(title = "Where does the money go? By provider",
       x = NULL, y = "EUR million", fill = NULL,
       caption = "Source: CYSTAT (2160013E). 2023-2024 provisional. Grey band = GeSY roll-out.") +
  theme_minimal()

ggsave("outputs/figures/04_spending_by_provider.png", width = 9, height = 5, bg = "white")


# 8. Save the results for the dashboard -----------------------

write_csv(spending_by_care,     "data/processed/spending_by_care.csv")
write_csv(spending_by_provider, "data/processed/spending_by_provider.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. Health spending almost doubled after GeSY.
#    It was flat during the financial crisis (EUR 1,265m in 2010 ->
#    1,219m in 2015), then rose from EUR 1,480m (2018) to 2,782m (2024),
#    +88%. Most of the jump happened in 2019-2021 (GeSY roll-out + COVID).
#
# 2. The money is moving towards hospitals.
#    Hospital care (inpatient + day): EUR 489m -> 1,212m (+148%),
#    from 33% to 44% of all spending.
#    Outpatient care: EUR 417m -> 639m (+53%), from 28% to 23%.
#    In 2018, for every EUR 1 on outpatient care, EUR 1.2 went to
#    hospital care. In 2024 it was EUR 1.9.
#
# 3. Hospitals absorbed most of the new money.
#    Of the EUR 1,302m extra spending (2018 -> 2024), EUR 723m (56%)
#    went to hospital care.
#    By provider, hospitals now receive half of all health money:
#    EUR 666m (45%) -> 1,374m (49%). This is more than "hospital care"
#    alone, because hospitals also provide outpatient visits, labs and
#    imaging.
#
# 4. Other areas grew, but more slowly.
#    Medicines:       EUR 268m -> 465m (+73%), stable at about 17%.
#    Long-term care:  EUR 65m  -> 135m (+109%), still under 5%.
#    Prevention:      EUR 18m  -> 37m  (+101%), boosted by COVID
#                     testing and vaccination in 2021-2022.
#    Lab tests:       EUR 161m -> 209m (+29%), the slowest growth;
#                     share fell from 11% to 7.5%.
#
# 5. Administration stayed small.
#    Running the system (HIO, ministry, insurers) grew from EUR 41m to
#    55m, and fell from 2.8% to 2.0% of spending.
#
# 6. What this means.
#    GeSY brought many more people into hospitals, especially private
#    hospitals that joined the system in 2020. This raises an
#    efficiency question: international practice favours primary and
#    outpatient care, which is usually cheaper. In Cyprus the opposite
#    is happening.
#
# Caveats:
#  - Current prices (not adjusted for inflation). 2023-2024 provisional.
#  - 2021: CYSTAT moved spending between inpatient and day care
#    (that is why they are joined here as "Hospital care").
#  - COVID-19 (2020-2022) raised hospital and prevention spending.
#  - Descriptive only: not proof that GeSY caused each change.
# ============================================================