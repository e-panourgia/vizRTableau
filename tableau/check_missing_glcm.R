# ===================== LOAD LIBRARIES =====================
library(dplyr)
library(readr)

# ===================== LOAD CSV =====================
csv_path <- "../data/processed_tableau/pisa2018_flat_tableau.csv"
data <- read_csv(csv_path)

# ===================== CHECK COUNTRIES WITH NO VALID GLCM VALUES =====================
cat("Countries with no valid (non-NA) numeric GLCM values:\n")

countries_no_valid_glcm <- data %>%
  group_by(Country) %>%
  summarise(has_valid_glcm = any(!is.na(GLCM) & is.numeric(GLCM))) %>%
  filter(!has_valid_glcm) %>%
  pull(Country)

if (length(countries_no_valid_glcm) == 0) {
  cat("✅ All countries have at least one valid GLCM value.\n")
} else {
  print(countries_no_valid_glcm)
}

# ===================== COUNT NA GLCM VALUES PER COUNTRY =====================
cat("\nNumber of missing GLCM values per country:\n")

na_glcm_counts <- data %>%
  group_by(Country) %>%
  summarise(missing_glcm_count = sum(is.na(GLCM))) %>%
  filter(missing_glcm_count > 0)

if (nrow(na_glcm_counts) == 0) {
  cat("✅ No missing GLCM values found in any country.\n")
} else {
  print(na_glcm_counts)
}
