# PISA Gender & Country Analysis Dashboard

This project analyzes PISA 2018 and 2022 data to explore educational performance across countries, focusing on:

- Greece's position among participating countries  
- The gender gap in academic achievement across different nations and domains  

It includes data preprocessing in R, static plots, and an interactive Tableau dashboard.

## Tools Used

- **R** – for data cleaning, analysis, and plotting  
- **Tableau** – for dynamic dashboard visualization

## Execution Order Static Plots 
| Input | Script | Output |
|------|-------------|--------------------|
| data/raw/STU_QQQ_SAS | scripts/load_pisa_data_2022.R | data/deployed_data/pisa2022.RData           |
| data/deployed_data/pisa2018.Rdata, data/deployed_data/pisa2022.RData | scripts/script.R | figures/* |

Notes  :
- `scripts/load_pisa_data_2022.R`: This script loads, preprocesses, and structures the 2022 PISA dataset to prepare it for analysis or visualization, similar to how the 2018 data was handled.
- `scripts/script.R`: This script generates 10 static plots comparing PISA 2018 and 2022 results, focusing on country performance and gender gaps across key domains like Math, Reading, and Science.

## Execution Order Tableau
| Input | Script | Output |
|------|-------------|--------------------|
| data/deployed_data/pisa2018.Rdata | tableau/preprocess_data_tableau.R | data/processed_tableau/pisa2018_flat_tableau.csv |
| data/processed_tableau/pisa2018_flat_tableau.csv | tableau/pisa-domain-gender-advantage.twbx | - |

Notes  :
- `tableau/preprocess_data_tableau.R`: This R script loads and preprocesses PISA 2018 data by imputing missing numeric values, selecting key columns (Country, Gender, scores), and exporting a cleaned CSV file for Tableau visualization.
- `tableau/pisa-domain-gender-advantage.twbx`: This Tableau workbook visualizes gender differences in PISA 2018 domain scores across countries, using the cleaned dataset generated by the R script to highlight Greece’s position and international gender disparities.

## Dashboard Demo

![Dashboard Functionality](assets/GIF-ezgif.com-optimize.gif)

## Reports

- [`pisa_static_plots_with_r.pdf`](reports/pisa_static_plots_with_r.pdf) – Gender and country-based analysis using R  
- [`pisa_dynamic_dashboard_with_tableau.pdf`](reports/pisa_dynamic_dashboard_with_tableau.pdf) – Tableau dashboard summary and screenshots

