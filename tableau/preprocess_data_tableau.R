# ===================== INSTALL & LOAD PACKAGES =====================
options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("treemapify", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggtext")) install.packages("ggtext")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(knitr)
  library(grid)
  library(gridExtra)
  library(fs)
  library(tidyverse)
  library(treemapify)
  library(ggtext)
})

# ===================== PROJECT PATHS =====================
project_dir <- "/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau"
data_dir <- file.path(project_dir, "data/deployed_data")
output_dir <- file.path(project_dir, "data/processed_tableau")
figures_dir <- file.path(project_dir, "figures")

if (!dir.exists(figures_dir)) dir.create(figures_dir)
if (!dir.exists(output_dir)) dir.create(output_dir)

setwd(data_dir)

# ===================== LOAD & INSPECT DATA =====================
cat("================= 2018 Data & Preprocessing =================\n")
load("pisa2018.Rdata")

if (!"newdata" %in% ls()) {
  stop("`newdata` object not found in loaded .Rdata file.")
}

cat("\nColumns in `newdata`:\n")
print(colnames(newdata))

# ===================== MISSING VALUE CHECK =====================
cat("\nChecking for missing values:\n")
missing_values <- colSums(is.na(newdata))
missing_values <- missing_values[missing_values > 0]

if (length(missing_values) == 0) {
  cat("No missing values found.\n")
} else {
  for (col in names(missing_values)) {
    cat(paste0("• ", col, " (", class(newdata[[col]]), "): ", missing_values[col], " missing\n"))
  }
}

# ===================== IMPUTE NUMERIC MISSING VALUES =====================
data_imputed <- newdata
numeric_cols <- sapply(data_imputed, is.numeric)

data_imputed[, numeric_cols] <- lapply(data_imputed[, numeric_cols], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# Re-check for NA
cat("\nRemaining missing values after numeric imputation:\n")
remaining_na <- colSums(is.na(data_imputed))
remaining_na <- remaining_na[remaining_na > 0]

if (length(remaining_na) == 0) {
  cat("✅ All numeric missing values have been imputed.\n")
} else {
  for (col in names(remaining_na)) {
    cat(paste0("• ", col, " (", class(data_imputed[[col]]), "): ", remaining_na[col], " remaining\n"))
  }
}

# ===================== SELECT NEEDED COLUMNS ONLY =====================
# Include only fields needed for Tableau
final_flat_data <- data_imputed %>%
  select(
    Country = CNT,
    Gender = ST004D01T,
    MATH,
    READ,
    SCIE,
    GLCM
  )

# ===================== EXPORT TO CSV =====================
output_file <- file.path(output_dir, "pisa2018_flat_tableau.csv")
write_csv(final_flat_data, output_file)
cat(paste0("\n✅ Flat Tableau-ready file saved to: ", output_file, "\n"))
