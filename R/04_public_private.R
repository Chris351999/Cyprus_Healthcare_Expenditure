# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 04_public_private.R
# PURPOSE: Who pays for health care in Cyprus?
#          Pooled (taxes & GeSY) vs private, by year
# INPUT:  data/processed/cystat_clean.RData
#         R/03_labels.R
# OUTPUT: data/processed/public_private.csv
#         outputs/figures/01_public_private.png
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # public_codes, private_codes, colours


# 2. Look at the payers in 2018 (before GeSY) -----------------

financing %>%
  filter(year == 2018) %>%
  select(financing_code, financing_label, value_meur)


# 3. Pooled money, by year (government + GeSY) ----------------

public <- financing %>%
  filter(financing_code %in% public_codes) %>%
  group_by(year) %>%
  summarise(public_meur = sum(value_meur))


# 4. Private money, by year -----------------------------------
#    (insurance, non-profit, employers, out-of-pocket)

private <- financing %>%
  filter(financing_code %in% private_codes) %>%
  group_by(year) %>%
  summarise(private_meur = sum(value_meur))


# 5. Put them together and compute the shares -----------------

public_private <- left_join(public, private, by = "year") %>%
  mutate(
    total_meur    = public_meur + private_meur,
    public_share  = 100 * public_meur  / total_meur,
    private_share = 100 * private_meur / total_meur,
    provisional   = year >= 2023
  )

public_private


# 6. Check ----------------------------------------------------

# a) the two shares add up to 100 in every year
summary(public_private$public_share + public_private$private_share)

# b) our total = CYSTAT total (All HF), except 2010-2015,
#    where foreign payers (HF.4) are left out on purpose
financing %>%
  filter(financing_code == "All HF") %>%
  select(year, all_hf = value_meur) %>%
  left_join(select(public_private, year, total_meur), by = "year") %>%
  mutate(difference = round(all_hf - total_meur, 1))


# 7. Chart ----------------------------------------------------

public_private %>%
  select(year,
         "Pooled (taxes & GeSY contributions)" = public_share,
         "Private (out-of-pocket & insurance)" = private_share) %>%
  pivot_longer(-year, names_to = "sector", values_to = "share") %>%
  ggplot(aes(x = year, y = share, colour = sector)) +
  annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf,
           fill = "grey92") +
  annotate("text", x = 2019.5, y = 95, label = "GeSY\nroll-out",
           size = 3, colour = col_grey) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_colour_manual(values = c(col_pooled, col_private)) +
  scale_x_continuous(breaks = 2010:2024) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title    = "Who pays for health care in Cyprus?",
       subtitle = "Share of current health expenditure (%)",
       x = NULL, y = "%", colour = NULL,
       caption  = paste("Source: CYSTAT (2160011E). 2023-2024 provisional. Break in series 2015.",
                        "2021 out-of-pocket filled as total minus other payers.")) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("outputs/figures/01_public_private.png", width = 9, height = 5, bg = "white")


# 8. Save the result for the dashboard ------------------------

write_csv(public_private, "data/processed/public_private.csv")

# ============================================================
# FINDINGS
# ============================================================
#
# 1. Before GeSY, patients were the biggest payer.
#    In 2018, households paid EUR 656m out-of-pocket: 44% of all
#    health spending, more than the government (EUR 618m, 42%).
#    Private insurance added another 12%. GeSY did not exist yet
#    (EUR 3m).
#
# 2. The pooled share was falling before GeSY.
#    It dropped from 48% (2011) to 42% (2017-2018). After the 2013
#    financial crisis, public spending was cut and more of the cost
#    moved to patients' pockets.
#
# 3. GeSY turned the picture upside down in two years.
#    Pooled share: 42% (2018) -> 54% (2019, phase 1: GPs, specialists,
#    medicines, labs) -> 72% (2020, phase 2: hospitals) -> 79% (2021).
#    The two lines cross in 2019.
#
# 4. Since 2021 the new balance is stable.
#    Pooled share stays at 78-79% (2021-2024). The big shift happened
#    once; it has not continued.
#
# 5. In euros, private spending fell, then started rising again.
#    Private: EUR 858m (2018) -> EUR 480m (2021) -> EUR 577m (2024).
#    Pooled:  EUR 622m (2018) -> EUR 2,205m (2024), about 3.5 times more.
#    Total spending almost doubled: EUR 1,480m -> EUR 2,782m (+88%).
#
# 6. What this means.
#    GeSY did not make health care free. It changed WHO pays and WHEN:
#    from the sick person at the moment of care, to everyone, all year,
#    through income-based contributions (employees, employers,
#    self-employed and the state). This protects patients from large
#    bills, but the total cost to society went up.
#
# Caveats:
#  - Amounts are in current prices (not adjusted for inflation).
#  - 2023-2024 are provisional. 2015 has a break in the series.
#  - 2021 out-of-pocket was filled as total minus other payers.
#  - Descriptive only: COVID-19 (2020-2021) happened at the same time.
# ============================================================