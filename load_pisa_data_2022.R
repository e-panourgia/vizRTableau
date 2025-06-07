# =============================================================================
# Script: extract_pisa2022_subset.R (harmonize country + gender with 2018)
# =============================================================================

# ---- Load Required Packages ----
if (!require("haven")) install.packages("haven")
if (!require("dplyr")) install.packages("dplyr")
if (!require("fs")) install.packages("fs")

library(haven)
library(dplyr)
library(fs)

# ---- File Path and Columns to Load ----
sas_file_path <- "/Users/evangeliapanourgia/Desktop/pisa/pisa_competition_2018/STU_QQQ_SAS/CY08MSP_STU_QQQ.SAS7BDAT"

columns_needed <- c(
  "CNT",         # Country (ISO3 code)
  "ST004D01T",   # Gender
  "PV1MATH",     # Math score
  "PV1READ",     # Reading score
  "PV1SCIE"      # Science score
)

# ---- Load Data Subset ----
cat("Reading selected columns...\n")
pisa2022_subset <- read_sas(sas_file_path, col_select = all_of(columns_needed))

# ---- Harmonize Gender Codes (2022) to Match 2018 Labels ----
pisa2022_subset <- pisa2022_subset %>%
  mutate(ST004D01T = case_when(
    ST004D01T == 1 ~ "Male",
    ST004D01T == 2 ~ "Female",
    TRUE ~ NA_character_
  ))

# ---- Harmonize Country Codes using mapping ----
# Minimal mapping example — extend as needed
iso3_to_country <- c(
  "ALB" = "Albania",
  "QAZ" = "Baku (Azerbaijan)",
  "ARG" = "Argentina",
  "AUS" = "Australia",
  "AUT" = "Austria",
  "BEL" = "Belgium",
  "BRA" = "Brazil",
  "BRN" = "Brunei Darussalam",
  "BGR" = "Bulgaria",
  "KHM" = "Cambodia",                   # Not in 2018 list, may be unmatched
  "CAN" = "Canada",
  "CHL" = "Chile",
  "TAP" = "Chinese Taipei",
  "COL" = "Colombia",
  "CRI" = "Costa Rica",
  "HRV" = "Croatia",
  "CZE" = "Czech Republic",
  "DNK" = "Denmark",
  "DOM" = "Dominican Republic",
  "SLV" = "El Salvador",               # Not in 2018 list, if unmatched adjust accordingly
  "EST" = "Estonia",
  "FIN" = "Finland",
  "FRA" = "France",
  "GEO" = "Georgia",
  "PSE" = "Palestine",                 # Not listed in 2018
  "DEU" = "Germany",
  "GRC" = "Greece",
  "GTM" = "Guatemala",                 # Not listed in 2018
  "HKG" = "Hong Kong",
  "HUN" = "Hungary",
  "ISL" = "Iceland",
  "IDN" = "Indonesia",
  "IRL" = "Ireland",
  "ISR" = "Israel",
  "ITA" = "Italy",
  "KSV" = "Kosovo",
  "JAM" = "Jamaica",                   # Not listed in 2018
  "JPN" = "Japan",
  "KAZ" = "Kazakhstan",
  "JOR" = "Jordan",
  "KOR" = "Korea",
  "LVA" = "Latvia",
  "LTU" = "Lithuania",
  "MAC" = "Macao",
  "MYS" = "Malaysia",
  "MLT" = "Malta",
  "MEX" = "Mexico",
  "MNG" = "Mongolia",                 # Not in 2018 list
  "MDA" = "Moldova",
  "MNE" = "Montenegro",
  "MAR" = "Morocco",
  "NLD" = "Netherlands",
  "NZL" = "New Zealand",
  "NOR" = "Norway",
  "PAN" = "Panama",
  "PRY" = "Paraguay",                 # Not in 2018 list
  "PER" = "Peru",
  "PHL" = "Philippines",
  "POL" = "Poland",
  "PRT" = "Portugal",
  "QAT" = "Qatar",
  "ROU" = "Romania",
  "SAU" = "Saudi Arabia",
  "SRB" = "Serbia",
  "SGP" = "Singapore",
  "SVK" = "Slovak Republic",
  "VNM" = "Vietnam",
  "SVN" = "Slovenia",
  "ESP" = "Spain",
  "SWE" = "Sweden",
  "CHE" = "Switzerland",
  "THA" = "Thailand",
  "ARE" = "United Arab Emirates",
  "TUR" = "Turkey",
  "QUR" = "Qatar (Region)",           # Possibly a region; confirm
  "MKD" = "North Macedonia",
  "GBR" = "United Kingdom",
  "USA" = "United States",
  "URY" = "Uruguay",
  "UZB" = "Uzbekistan"                # Not in 2018 list
)


pisa2022_subset$CNT <- iso3_to_country[pisa2022_subset$CNT]

# ---- Missing Values BEFORE Imputation ----
cat("\nMissing values BEFORE imputation (Gender + Subjects):\n")
cols_to_check <- c("ST004D01T", "PV1MATH", "PV1READ", "PV1SCIE")
print(colSums(is.na(pisa2022_subset[, cols_to_check])))

# ---- Impute Missing Values for Numeric Columns ----
numeric_cols <- sapply(pisa2022_subset, is.numeric)
pisa2022_imputed <- pisa2022_subset
pisa2022_imputed[, numeric_cols] <- lapply(pisa2022_imputed[, numeric_cols], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# ---- Rename Columns to Match 2018 ----
pisa2022_imputed <- pisa2022_imputed %>%
  rename(
    MATH = PV1MATH,
    READ = PV1READ,
    SCIE = PV1SCIE
  )

# ---- Missing Values AFTER Imputation ----
cat("\nMissing values AFTER imputation (Gender + Subjects):\n")
print(colSums(is.na(pisa2022_imputed[, c("ST004D01T", "MATH", "READ", "SCIE")])))

# ---- Save Cleaned and Harmonized Data ----
output_dir <- "/Users/evangeliapanourgia/Desktop/karlis-final/pisa_competition_2018"
dir_create(output_dir)
save(pisa2022_imputed, file = file.path(output_dir, "pisa2022.RData"))
cat("\n✅ Cleaned, renamed, and harmonized dataset saved to:", file.path(output_dir, "pisa2022.RData"), "\n")
