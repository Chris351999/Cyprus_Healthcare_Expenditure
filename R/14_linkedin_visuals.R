# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT: 14_linkedin_visuals.R
# PURPOSE: Two eye-catching static charts for LinkedIn
#          A. "Who pays for every EUR 100?" (waffle chart)
#          B. "Biggest change in the EU" (arrow chart)
# INPUT:  data/processed/cystat_clean.RData
#         data/processed/eu_spending_all.csv   (from 11_eu_spending_outcomes.R)
#         R/03_labels.R
# OUTPUT: outputs/figures/linkedin_waffle.png
#         outputs/figures/linkedin_eu_arrows.png
# ============================================================


# 1. Load packages, data and labels ---------------------------

library(tidyverse)
load("data/processed/cystat_clean.RData")
source("R/03_labels.R")   # public_codes, private_codes, colours

eu <- read_csv("data/processed/eu_spending_all.csv", show_col_types = FALSE)

payer_colours <- c("Taxes & GeSY contributions" = col_pooled,
                   "Out-of-pocket"              = col_private,
                   "Private insurance & other"  = "#c8c8c8")


# ---- A. Who pays for every EUR 100? ----------------------------

# 2. Shares of each payer in 2018 and 2024 --------------------

shares <- financing %>%
  filter(year %in% c(2018, 2024),
         financing_code %in% c(public_codes, private_codes)) %>%
  mutate(payer = case_when(
    financing_code %in% public_codes ~ "Taxes & GeSY contributions",
    financing_code == "HF.3"         ~ "Out-of-pocket",
    TRUE                             ~ "Private insurance & other"
  )) %>%
  group_by(year, payer) %>%
  summarise(value = sum(value_meur), .groups = "drop") %>%
  group_by(year) %>%
  mutate(n = round(100 * value / sum(value))) %>%
  ungroup()


# 3. Make sure each year has exactly 100 squares --------------
#    (rounding can give 99 or 101: we fix the largest group)

shares <- shares %>%
  group_by(year) %>%
  mutate(n = if_else(n == max(n), n + (100 - sum(n)), n)) %>%
  ungroup()

shares


# 4. One row per square, placed on a 10 x 10 grid -------------

tiles <- shares %>%
  mutate(payer = factor(payer, levels = names(payer_colours))) %>%
  arrange(year, payer) %>%
  uncount(n) %>%
  group_by(year) %>%
  mutate(i = row_number() - 1,
         x = i %% 10,
         y = i %/% 10) %>%
  ungroup() %>%
  mutate(panel = if_else(year == 2018,
                         "2018 - before GeSY\n\u20ac44 out-of-pocket",
                         "2024 - with GeSY\n\u20ac17 out-of-pocket"))

# 5. Waffle chart ---------------------------------------------

ggplot(tiles, aes(x = x, y = y, fill = payer)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  coord_equal() +
  facet_wrap(~ panel) +
  scale_fill_manual(values = payer_colours) +
  labs(title    = "Who pays for every \u20ac100 of health care in Cyprus?",
       subtitle = "Each square = \u20ac1",
       fill     = NULL,
       caption  = "Data: CYSTAT | Analysis: Christos Fakontis") +
  theme_void(base_size = 16) +
  theme(legend.position = "bottom",
        strip.text      = element_text(face = "bold", size = 15, margin = margin(b = 8)),
        plot.title      = element_text(face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(hjust = 0.5, colour = "grey40"),
        plot.caption    = element_text(colour = "grey50"),
        plot.margin     = margin(20, 20, 20, 20))

ggsave("outputs/figures/linkedin_waffle.png", width = 10.8, height = 7, dpi = 200, bg = "white")


# ---- B. Biggest change in the EU ---------------------------------

# 6. Out-of-pocket share in 2018 and 2023, every EU country ---

change <- eu %>%
  filter(indicator == "Out-of-pocket share (%)", year %in% c(2018, 2023)) %>%
  select(country, geo, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "y") %>%
  filter(!is.na(y2018), !is.na(y2023)) %>%
  mutate(change    = y2023 - y2018,
         is_cyprus = geo == "CY") %>%
  arrange(change)

# Which countries had the biggest fall? (check Cyprus is first!)
head(change, 5)


# 7. Arrow chart: one arrow per country, sorted by change -----

ggplot(change, aes(y = reorder(country, -change))) +
  geom_segment(aes(x = y2018, xend = y2023, yend = reorder(country, -change),
                   colour = is_cyprus, linewidth = is_cyprus),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  
  scale_colour_manual(values = c("TRUE" = "#d6293e", "FALSE" = "grey70"), guide = "none") +
  scale_linewidth_manual(values = c("TRUE" = 1.6, "FALSE" = 0.7), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title    = "Out-of-pocket payments for health care, 2018 \u2192 2023",
       subtitle = "Share of health spending paid directly by patients. Cyprus in red.",
       x = NULL, y = NULL,
       caption  = "Data: Eurostat (hlth_sha11_hf) | Analysis: Christos Fakontis") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title         = element_text(face = "bold"),
        plot.subtitle      = element_text(colour = "grey40"),
        plot.caption       = element_text(colour = "grey50"))

ggsave("outputs/figures/linkedin_eu_arrows.png", width = 9, height = 10, dpi = 200, bg = "white")

# ============================================================
# FINDINGS
# ============================================================
#
# A. Who pays for every EUR 100 of health care?
#    2018: EUR 44 out-of-pocket, EUR 42 taxes & GeSY,
#          EUR 14 private insurance & other.
#    2024: EUR 17 out-of-pocket, EUR 79 taxes & GeSY,
#          EUR 4 private insurance & other.
#    Before GeSY, patients paid more out of their own pocket than
#    taxes and contributions covered. Now four fifths is pooled.
#
# B. The biggest change in the EU.
#    Out-of-pocket share, 2018 -> 2023:
#    Cyprus    44.4% -> 17.4%  (-26.9 points)
#    Poland    20.4% -> 15.8%  (-4.6), the second largest fall
#    Finland   18.4% -> 14.3%  (-4.1)
#    Cyprus's fall was about 6 times larger than any other EU
#    country's. In several countries (e.g. Romania, Belgium,
#    Netherlands) the out-of-pocket share went up.
#
# Caveats:
#  - Panel A uses CYSTAT (2018 vs 2024); panel B uses Eurostat
#    (2018 vs 2023, the latest year with data for all countries).
#  - Squares are rounded to whole euros (total kept at exactly 100).
# ============================================================