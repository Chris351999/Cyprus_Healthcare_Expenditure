# ============================================================
# PROJECT: Cyprus Healthcare Expenditure Analysis
# SCRIPT : 03_labels.R
# PURPOSE: Short English names for CYSTAT codes, the GeSY
#          timeline and colours. Used by all scripts and the dashboard.
# ============================================================


# ---- 1. Types of care -------------------------------------------------------
# Nine types that do not overlap (their sum = total spending).
# Inpatient + day care are joined into "Hospital care" (made in 02_clean.R),
# because in 2021 only their sum is known.
care_names <- c(
  "HC.1.1+HC.1.2" = "Hospital care",
  "HC.1.3-HC.2.3" = "Outpatient care",
  "HC.1.4-HC.2.4" = "Home care",
  "HC.3"          = "Long-term care",
  "HC.4"          = "Lab tests & imaging",
  "HC5.1"         = "Medicines",
  "HC5.2"         = "Medical devices",
  "HC.6"          = "Prevention",
  "HC.7"          = "Administration"
)

# Five broad groups, for simpler charts in the dashboard
care_groups <- c(
  "HC.1.1+HC.1.2" = "Hospital care",
  "HC.1.3-HC.2.3" = "Outpatient care",
  "HC.1.4-HC.2.4" = "Long-term & home care",
  "HC.3"          = "Long-term & home care",
  "HC.4"          = "Labs, prevention & admin",
  "HC5.1"         = "Medicines & devices",
  "HC5.2"         = "Medicines & devices",
  "HC.6"          = "Labs, prevention & admin",
  "HC.7"          = "Labs, prevention & admin"
)


# ---- 2. Providers -----------------------------------------------------------
# Who delivers the care (their sum = total spending)
provider_names <- c(
  "HP.1" = "Hospitals",
  "HP.2" = "Residential long-term care",
  "HP.3" = "Ambulatory care (doctors, clinics)",
  "HP.4" = "Ancillary services (labs, imaging)",
  "HP.5" = "Pharmacies & medical goods",
  "HP.6" = "Preventive care",
  "HP.7" = "Administration & financing",
  "HP.8" = "Rest of economy",
  "HP.9" = "Rest of the world"
)


# ---- 3. Financing schemes ---------------------------------------------------
# Who pays
financing_names <- c(
  "HF.1.1" = "Government",
  "HF.1.2" = "GeSY",
  "HF.2.1" = "Voluntary insurance",
  "HF.2.2" = "Non-profit",
  "HF.2.3" = "Employers",
  "HF.3"   = "Out-of-pocket",
  "HF.4"   = "Foreign payers"     # only published until 2015, tiny
)

# Pooled = paid through taxes and GeSY contributions (everyone pays, by income)
# Private = paid by the patient, their insurer or employer
public_codes  <- c("HF.1.1", "HF.1.2")
private_codes <- c("HF.2.1", "HF.2.2", "HF.2.3", "HF.3")


# ---- 4. GeSY timeline -------------------------------------------------------
# Source: Health Insurance Organisation, gesy.org.cy
gesy_phase1 <- 2019        # June 2019: GPs, specialists, medicines, labs
gesy_phase2 <- 2020        # June 2020: hospitals, A&E, ambulances
pre_years   <- 2016:2018   # before GeSY
post_years  <- 2022:2024   # after GeSY (2019-2021 left out: roll-out + COVID)


# ---- 5. Colours -------------------------------------------------------------
# Same colours in every chart and in the dashboard
col_pooled  <- "#2a78d6"   # blue   = taxes & GeSY
col_private <- "#eb6834"   # orange = out-of-pocket & private
col_grey    <- "#52514e"   # text and notes
