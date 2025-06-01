# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Set working directory and load data
setwd("/Users/evangeliapanourgia/Desktop/pisa/pisa_competition_2018")
# Loads 'newdata' object containing the PISA dataset
load("pisa2018.Rdata")

# Check column names if data frame exists
if ("newdata" %in% ls()) {
  cat("\nColumn names in `newdata`:\n")
  print(colnames(newdata))
} else {
  cat("\nData frame `newdata` not found. Please check the object name.\n")
}

# ----- DATA PREPROCESSING -----
# Check for missing values
cat("Checking for missing values in the original dataset:\n")
cat("====================================================\n")
missing_values <- colSums(is.na(newdata))
missing_values <- missing_values[missing_values > 0]

if (length(missing_values) == 0) {
  cat("No missing values found.\n")
} else {
  for (col_name in names(missing_values)) {
    col_type <- class(newdata[[col_name]])
    cat(paste0("• ", col_name, " (", col_type, "): ", missing_values[col_name], " missing values\n"))
  }
}

# Impute missing values for numeric columns with the mean
data_imputed <- newdata
numeric_cols <- sapply(data_imputed, is.numeric)

data_imputed[, numeric_cols] <- lapply(data_imputed[, numeric_cols], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})

# Confirm remaining missing values after imputation
cat("\nRemaining missing values after numeric imputation:\n")
cat("====================================================\n")
remaining_na <- colSums(is.na(data_imputed))
remaining_na <- remaining_na[remaining_na > 0]

if (length(remaining_na) == 0) {
  cat("All missing values in numeric columns have been imputed.\n")
} else {
  for (col_name in names(remaining_na)) {
    col_type <- class(data_imputed[[col_name]])
    cat(paste0("• ", col_name, " (", col_type, "): ", remaining_na[col_name], " missing values\n"))
  }
}

# Load required packages
library(dplyr)
library(tidyr)
library(knitr)

# Use imputed dataset
data <- data_imputed

# Subset data
data_greece         <- data %>% filter(CNT == "Greece")
data_exclude_greece <- data %>% filter(CNT != "Greece")

# Summary for Greece with descriptive statistics
summary_greece <- data_greece %>%
  summarize(
    Math_Mean      = mean(MATH, na.rm = TRUE),
    Math_SD        = sd(MATH, na.rm = TRUE),
    Math_Median    = median(MATH, na.rm = TRUE),
    Math_Q1        = quantile(MATH, 0.25, na.rm = TRUE),
    Math_Q3        = quantile(MATH, 0.75, na.rm = TRUE),
    
    Reading_Mean   = mean(READ, na.rm = TRUE),
    Reading_SD     = sd(READ, na.rm = TRUE),
    Reading_Median = median(READ, na.rm = TRUE),
    Reading_Q1     = quantile(READ, 0.25, na.rm = TRUE),
    Reading_Q3     = quantile(READ, 0.75, na.rm = TRUE),
    
    Science_Mean   = mean(SCIE, na.rm = TRUE),
    Science_SD     = sd(SCIE, na.rm = TRUE),
    Science_Median = median(SCIE, na.rm = TRUE),
    Science_Q1     = quantile(SCIE, 0.25, na.rm = TRUE),
    Science_Q3     = quantile(SCIE, 0.75, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Value")

# Print table for Greece
cat("\nDescriptive Statistics for Greece:\n")
kable(summary_greece, digits = 2, caption = "Summary statistics for students in Greece")

# Summary for top countries (excluding Greece)
summary_top_countries <- data_exclude_greece %>%
  group_by(CNT) %>%
  summarize(
    Count_Students = n(),
    Math_Mean      = mean(MATH, na.rm = TRUE),
    Math_SD        = sd(MATH, na.rm = TRUE),
    Reading_Mean   = mean(READ, na.rm = TRUE),
    Reading_SD     = sd(READ, na.rm = TRUE),
    Science_Mean   = mean(SCIE, na.rm = TRUE),
    Science_SD     = sd(SCIE, na.rm = TRUE)
  ) %>%
  mutate(Overall_Mean = (Math_Mean + Reading_Mean + Science_Mean) / 3) %>%
  arrange(desc(Overall_Mean))

# Print table for top countries
cat("\nSummary Statistics for Countries (excluding Greece):\n")
kable(head(summary_top_countries, 10), digits = 2, caption = "Top 10 countries by overall average score")

# Gender distribution by country
gender_distribution <- data %>%
  group_by(CNT, ST004D01T) %>%
  summarize(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ST004D01T, values_from = Count, values_fill = 0)

# Print gender distribution table
cat("\nGender Distribution by Country:\n")
kable(gender_distribution, caption = "Student gender distribution by country")


# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Calculate country-level average scores
ranking_df <- data %>%
  group_by(CNT) %>%
  summarize(
    Reading = mean(READ, na.rm = TRUE),
    Math    = mean(MATH, na.rm = TRUE),
    Science = mean(SCIE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Country = CNT)

# Step 2: Get top-scoring country for each subject
top_scores <- ranking_df %>%
  pivot_longer(cols = Reading:Science, names_to = "Subject", values_to = "Score") %>%
  group_by(Subject) %>%
  slice_max(Score, n = 1) %>%
  mutate(Group = "Top Country")

# Step 3: Prepare simplified subtitle (one country repeated)
top_country_unique <- top_scores$Country[1]  # All are the same
subtitle_text <- paste0("Compared against top-performing country: ", top_country_unique)

# Step 4: Extract Greece's scores
greece_scores <- ranking_df %>%
  filter(Country == "Greece") %>%
  pivot_longer(cols = Reading:Science, names_to = "Subject", values_to = "Score") %>%
  mutate(Group = "Greece")

# Step 5: Combine data for plotting
plot_data <- bind_rows(top_scores, greece_scores)

# Step 6: Compute differences for annotation
plot_data_diff <- plot_data %>%
  pivot_wider(names_from = Group, values_from = Score) %>%
  mutate(Difference = `Top Country` - Greece)

# Step 7: Create the plot
p1 <- ggplot(plot_data, aes(x = Subject, y = Score, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.35) +
  geom_text(aes(label = ceiling(Score)), position = position_dodge(width = 0.4), vjust = -1.2, size = 4, fontface = "bold") +
  geom_text(data = plot_data_diff, aes(x = Subject, y = pmax(`Top Country`, Greece) + 10, label = paste0("Δ ", ceiling(Difference), " pts")), inherit.aes = FALSE, size = 4, fontface = "italic", color = "darkred") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)), limits = c(0, NA)) +
  scale_fill_manual(values = c("Greece" = "#1f77b4", "Top Country" = "#ff7f0e")) +
  labs(title = "Greece vs Top-Performing Country by Subject (PISA 2018)", subtitle = subtitle_text, x = "Subject", y = "Average Score", fill = "Group") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)), plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)), panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), legend.title = element_text(face = "bold"), legend.position = "top", legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5), axis.line = element_line(color = "black", linewidth = 0.6), axis.ticks = element_line(color = "black", linewidth = 0.4))
ggsave("figures/greece_vs_top_subjects_diff.png", plot = p1, width = 10, height = 6, dpi = 300, bg = "white")

# Define OECD countries
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia",
  "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Turkey", "United Kingdom", "United States"
)

# STEP 1: Compute medians for Math, Reading, Science (all countries)
base_medians <- data_imputed %>%
  group_by(CNT) %>%
  summarise(
    Reading = median(READ, na.rm = TRUE),
    Math = median(MATH, na.rm = TRUE),
    Science = median(SCIE, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 2: Compute GLCM medians only for countries with GLCM data
glcm_medians <- data_imputed %>%
  filter(!is.na(GLCM)) %>%
  group_by(CNT) %>%
  summarise(`Global Competence` = median(GLCM, na.rm = TRUE), .groups = "drop")

# STEP 3: Join all medians together
country_medians <- full_join(base_medians, glcm_medians, by = "CNT")

# STEP 4: Reshape for plotting
long_df <- country_medians %>%
  pivot_longer(cols = Reading:`Global Competence`, names_to = "Subject", values_to = "Score") %>%
  filter(!is.na(Score))

# STEP 5: Greece data
greece_medians <- long_df %>%
  filter(CNT == "Greece") %>%
  mutate(Type = "Greece")

# STEP 6: OECD medians
oecd_medians <- data_imputed %>%
  filter(CNT %in% oecd_countries) %>%
  summarise(
    Reading = median(READ, na.rm = TRUE),
    Math = median(MATH, na.rm = TRUE),
    Science = median(SCIE, na.rm = TRUE),
    `Global Competence` = median(GLCM, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Subject", values_to = "OECD_Median")

# Facet labels
subject_labels <- c(
  "Reading" = "Reading",
  "Math" = "Math",
  "Science" = "Science",
  "Global Competence" = "Global Comp."
)

# STEP 7: Plot
p2 <- ggplot(long_df, aes(x = "", y = Score)) +
  geom_boxplot(fill = "#dddddd", color = "gray30", outlier.color = "gray60", width = 0.3) +
  geom_hline(data = oecd_medians, aes(yintercept = OECD_Median, linetype = "OECD Median"), color = "red", linewidth = 1.1, inherit.aes = FALSE) +
  geom_point(data = greece_medians, aes(x = "", y = Score, color = "Greece"), size = 3) +
  geom_text(data = greece_medians, aes(x = "", y = Score, label = paste0("Greece: ", round(Score))), color = "#1f77b4", fontface = "bold", size = 4, vjust = -1.5) +
  facet_wrap(~Subject, nrow = 1, labeller = labeller(Subject = subject_labels)) +
  labs(title = "Greece vs Global and OECD Performance (PISA 2018)", subtitle = "• Gray = Global scores   •   Dashed = OECD median   •   Dot = Greece\n*Countries without GLCM data excluded from Global Comp.", x = NULL, y = "Median Score", linetype = NULL, color = NULL) +
  scale_color_manual(values = c("Greece" = "#1f77b4")) +
  scale_linetype_manual(values = c("OECD Median" = "dashed")) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5), legend.position = "bottom", legend.box = "horizontal")
ggsave("figures/greece_filtered_glcm_boxplots_fixed.png", plot = p2, width = 12, height = 5, dpi = 300, bg = "white")

# explore the reason why in the first boxplot we don't see rectangle.
long_df %>%
  filter(Subject == "Global Competence") %>%
  summarise(
    n = n_distinct(Score),
    min = min(Score),
    max = max(Score)
  )

long_df %>%
  filter(Subject == "Global Competence") %>%
  summarise(
    Q1 = quantile(Score, 0.25),
    Median = quantile(Score, 0.5),
    Q3 = quantile(Score, 0.75)
  )

library(dplyr)
library(ggplot2)

# Step 1: Detect the Global Competence column
glcm_col <- grep("GLO|GLCM|global", names(data), ignore.case = TRUE, value = TRUE)[1]
stopifnot(!is.na(glcm_col))

# Step 2: Compute average scores and ranks
glcm_summary <- data %>%
  filter(!is.na(.data[[glcm_col]])) %>%
  group_by(CNT) %>%
  summarise(GLCM_Mean = mean(.data[[glcm_col]], na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(GLCM_Mean)) %>%
  mutate(Rank = row_number()) %>%
  rename(Country = CNT)

# Step 3: Extract Greece and countries ranked above
greece_row <- glcm_summary %>% filter(Country == "Greece")
greece_rank <- greece_row$Rank
above_greece <- glcm_summary %>% filter(Rank < greece_rank)

# Combine into plotting dataset
plot_data <- bind_rows(above_greece, greece_row) %>%
  mutate(Highlight = if_else(Country == "Greece", "Greece", "Other"))

# Step 4: Global average baseline
global_avg <- round(mean(glcm_summary$GLCM_Mean, na.rm = TRUE), 1)
baseline_label <- paste0("Global Average (", global_avg, ")")
baseline_df <- data.frame(y = global_avg, label = baseline_label)

# Step 5: Create the plot
p3 <- ggplot(plot_data, aes(x = reorder(Country, -GLCM_Mean), y = GLCM_Mean, fill = Highlight)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(GLCM_Mean, 1)), vjust = -0.5, size = 3.3, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("Greece" = "#1f77b4", "Other" = "gray80"), name = "Country Type", labels = c("Greece", "Other Countries")) +
  geom_hline(data = baseline_df, aes(yintercept = y, linetype = label), color = "darkred", linewidth = 0.8, show.legend = TRUE) +
  scale_linetype_manual(name = NULL, values = setNames("dashed", baseline_df$label)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Greece and Higher-Scoring Countries\nin Global Competence (PISA 2018)", subtitle = paste("Greece ranked", greece_rank, "out of", nrow(glcm_summary), "countries –", greece_rank - 1, "countries scored higher"), x = NULL, y = "Average Global Competence Score") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 11), plot.title.position = "plot", axis.text.x = element_text(angle = 45, hjust = 1, size = 10), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 11), plot.margin = margin(10, 10, 10, 10))
ggsave("figures/glcm_plot_wrapped_title.png", plot = p3, width = 10, height = 6, dpi = 320, bg = "white")

# Define OECD countries
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia",
  "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Turkey", "United Kingdom", "United States"
)

# Detect GLCM column name
glcm_col <- grep("GLO|GLCM|global", names(data), ignore.case = TRUE, value = TRUE)[1]

# Calculate OECD average
oecd_avg_glcm <- data %>%
  filter(CNT %in% oecd_countries, !is.na(.data[[glcm_col]])) %>%
  summarise(OECD_GLCM_Average = mean(.data[[glcm_col]], na.rm = TRUE)) %>%
  pull(OECD_GLCM_Average)

# Round result
round(oecd_avg_glcm, 1)


library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Define OECD countries
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia",
  "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Turkey", "United Kingdom", "United States"
)

# Step 2: Prepare data
df <- data_imputed %>%
  rename(
    Country = CNT,
    Gender = ST004D01T,
    Reading = READ,
    Math = MATH,
    Science = SCIE,
    GlobalCompetence = GLCM
  )

df_valid <- df %>%
  filter(!is.na(Gender), !is.na(Reading), !is.na(Math), !is.na(Science), !is.na(GlobalCompetence)) %>%
  filter(Country %in% oecd_countries)

# Step 3: Compute Greece, OECD, and TopGap averages
subject_avg <- df_valid %>%
  group_by(Country, Gender) %>%
  summarise(
    Reading = mean(Reading, na.rm = TRUE),
    Math = mean(Math, na.rm = TRUE),
    Science = mean(Science, na.rm = TRUE),
    `Global Competence` = mean(GlobalCompetence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = Reading:`Global Competence`, names_to = "Subject", values_to = "Score")

# Step 4: Identify top-gap country for each gender and subject
top_gap <- subject_avg %>%
  group_by(Gender, Subject) %>%
  slice_max(order_by = Score, n = 1) %>%
  mutate(Group = "TopGap")

# Step 5: Combine Greece, OECD average, and top-gap rows
oecd_avg <- subject_avg %>%
  filter(Country %in% oecd_countries & Country != "Greece") %>%
  group_by(Gender, Subject) %>%
  summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Group = "OECD")

greece <- subject_avg %>%
  filter(Country == "Greece") %>%
  mutate(Group = "Greece")

plot_data <- bind_rows(greece, oecd_avg, top_gap) %>%
  mutate(
    BarGroup = factor(paste0(Group, " (", substr(Gender, 1, 1), ")"),
      levels = c("Greece (F)", "OECD (F)", "TopGap (F)",
                 "Greece (M)", "OECD (M)", "TopGap (M)"))
  )

# Step 6: Plot
p4 <- ggplot(plot_data, aes(x = Subject, y = Score, fill = BarGroup)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0.1), width = 0.7) +
  scale_fill_manual(values = c("Greece (F)" = "#e60073", "OECD (F)" = "#fddde6", "TopGap (F)" = "#ffb6c1", "Greece (M)" = "#003399", "OECD (M)" = "#aec7e8", "TopGap (M)" = "#7fbfff"), name = NULL, labels = c("Greece (F)", "OECD (F)", "Top Country (F)", "Greece (M)", "OECD (M)", "Top Country (M)")) +
  labs(title = "Scores by Gender and Country Group (PISA 2018)", x = NULL, y = "Mean Score") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(face = "bold"), legend.position = "top", legend.text = element_text(size = 11))
ggsave("figures/scores_by_gender_country_group.png", plot = p4, width = 10, height = 6, dpi = 300, bg = "white")

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Step 1: Assign and rename columns
df <- data_imputed %>%
  rename(
    Country = CNT,
    Gender = ST004D01T,
    Reading = READ,
    Math = MATH,
    Science = SCIE,
    GlobalCompetence = GLCM
  )

# Step 2: Filter and summarise
df_valid <- df %>%
  filter(
    !is.na(GlobalCompetence),
    !is.na(Reading),
    !is.na(Math),
    !is.na(Science),
    !is.na(Gender)
  )

df_gender_country <- df_valid %>%
  group_by(Country, Gender) %>%
  summarise(
    Mean_GLCM = mean(GlobalCompetence, na.rm = TRUE),
    Mean_Academic = mean(c(Reading, Math, Science), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Distance_vertical = abs(Mean_GLCM - Mean_Academic) / sqrt(2),
    Direction = ifelse(Mean_GLCM > Mean_Academic, "Above", "Below")
  )

# Step 3: Extract Greece data for both genders
greece_summary <- df_gender_country %>%
  filter(Country == "Greece")

# Create custom labels with distance for legend
greece_summary <- greece_summary %>%
  mutate(GenderLabel = paste0(Gender, " (perpendicular \n distance) dist: ", round(Distance_vertical, 1), ")"))

# Step 4: Plot function
plot_gender_distance <- function(greece_data, color) {
  df_sub <- df_gender_country %>%
    filter(Gender == greece_data$Gender)
  
  ggplot(df_sub, aes(x = Mean_Academic, y = Mean_GLCM)) +
    geom_point(color = "gray50", size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    geom_point(data = greece_data,
               aes(x = Mean_Academic, y = Mean_GLCM, shape = GenderLabel),
               color = color, size = 4, stroke = 1.2) +
    geom_segment(data = greece_data,
                 aes(x = Mean_Academic, xend = Mean_Academic,
                     y = Mean_Academic, yend = Mean_GLCM),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = color, linewidth = 0.8) +
    labs(
      x = "Academic Mean Score",
      y = "Global Competence Mean"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

# Step 5: Generate plots
plot_female <- plot_gender_distance(greece_summary %>% filter(Gender == "Female"), "#FF69B4")
plot_male   <- plot_gender_distance(greece_summary %>% filter(Gender == "Male"), "#1f77b4")

# Step 6: Display side by side with common legend
g <- arrangeGrob(
  plot_female + ggtitle("Greece – Female"),
  plot_male + ggtitle("Greece – Male"),
  ncol = 2,
  top = textGrob("Greece's Global Competence vs Academic Scores by Gender", gp = gpar(fontsize = 15, fontface = "bold"))
)
ggsave("figures/glcm_vs_academic_gender_greece_dist_legend.png", g, width = 13, height = 6, dpi = 300, bg = "white")

library(dplyr)
library(ggplot2)

# Step 1: Prepare gender-wise scores
gender_summary <- df %>%
  filter(!is.na(Reading), !is.na(Math), !is.na(Science), !is.na(Gender)) %>%
  group_by(Country, Gender) %>%
  summarise(
    Reading = mean(Reading, na.rm = TRUE),
    Math = mean(Math, na.rm = TRUE),
    Science = mean(Science, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Compute gender gaps
gender_gap <- gender_summary %>%
  pivot_longer(cols = Reading:Science, names_to = "Subject", values_to = "Score") %>%
  pivot_wider(names_from = Gender, values_from = Score) %>%
  mutate(Gap = Female - Male) %>%
  group_by(Country) %>%
  summarise(
    Mean_Gap = mean(Gap, na.rm = TRUE),
    Overall_Score = mean(c(Female, Male), na.rm = TRUE)
  ) %>%
  mutate(IsGreece = ifelse(Country == "Greece", "Greece", "Other"))

# Step 3: Plot
p6 <- ggplot(gender_gap, aes(x = Overall_Score, y = Mean_Gap)) +
  geom_point(aes(color = IsGreece), size = 3.8, alpha = 0.85, stroke = 1.2, shape = 21, fill = ifelse(gender_gap$IsGreece == "Greece", "#1f77b4", "gray90")) +
  geom_text(data = filter(gender_gap, Country == "Greece"), aes(label = "Greece"), color = "#1f77b4", fontface = "bold", hjust = -0.15, vjust = 0.5, size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Greece" = "#1f77b4", "Other" = "gray40")) +
  guides(color = "none") +
  labs(title = "Gender Gap vs Average Academic Performance \n(PISA 2018)", subtitle = "● Gray: All countries  ● Blue: Greece  ▬ Dashed: Trend line\nGender gap = Female − Male avg. (Reading, Math, Science)", x = "Mean Academic Score", y = "Mean Gender Gap") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 11), legend.position = "none")
ggsave("figures/gender_gap_vs_academic_performance.png", plot = p6, width = 10, height = 6, dpi = 300, bg = "white")


#######
######




### Load data for 2022 for suppoting comparison plots 
# ---- Setup ----
# if (!require("haven")) install.packages("haven")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("fs")) install.packages("fs")

# library(haven)
# library(dplyr)
# library(fs)

# # ---- Define path and columns ----
# sas_file_path <- "/Users/evangeliapanourgia/Desktop/pisa/pisa_competition_2018/STU_QQQ_SAS/CY08MSP_STU_QQQ.SAS7BDAT"

# # Columns corresponding to 2018 equivalents
# columns_needed <- c("ST003D02T", "ST004D01T", "ST005Q01JA", "ST007Q01JA", "PV1MATH", "PV1READ", "PV1SCIE")

# # ---- Read only selected columns ----
# cat("Reading selected columns...\n")
# pisa2022_subset <- read_sas(sas_file_path, col_select = all_of(columns_needed))

# # ---- Save the subset data ----
# save(pisa2022_subset, file = file.path("pisa2022.RData"))
# cat("✅ Subset saved to: ", file.path("pisa2022.RData"), "\n")

# ----- DATA PREPROCESSING FOR PISA 2022 -----

# Optional: set working directory to the current script location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the subset data (assumed to be in the same folder)

library(dplyr)
library(tidyr)
library(ggplot2)

# --- Load 2018 data ---
load("pisa2018.RData") # loads 'newdata' or 'data'
if (exists("newdata")) {
  data_2018 <- newdata
} else if (exists("data")) {
  data_2018 <- data
} else {
  stop("No 2018 data found!")
}

# --- Preprocess 2018: Impute missing numeric values with mean ---
data_2018_imputed <- data_2018
num_cols_2018 <- sapply(data_2018_imputed, is.numeric)
data_2018_imputed[, num_cols_2018] <- lapply(data_2018_imputed[, num_cols_2018], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# --- Load 2022 data ---
load("pisa2022.RData") # loads 'pisa2022_subset' or similar
if (exists("pisa2022_subset")) {
  data_2022 <- pisa2022_subset
} else if (exists("data_2022")) {
  data_2022 <- data_2022
} else {
  stop("No 2022 data found!")
}

# --- Preprocess 2022: Impute missing numeric values with mean ---
data_2022_imputed <- data_2022
num_cols_2022 <- sapply(data_2022_imputed, is.numeric)
data_2022_imputed[, num_cols_2022] <- lapply(data_2022_imputed[, num_cols_2022], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# --- Gender mapping function (adjust if needed) ---
map_gender <- function(x) {
  ifelse(x == 1 | x == "1" | x == "Male", "Male",
         ifelse(x == 2 | x == "2" | x == "Female", "Female", NA_character_))
}

# --- Prepare 2018 tidy data ---
data_2018_tidy <- data_2018_imputed %>%
  transmute(
    Year = "2018",
    Gender = map_gender(ST004D01T),
    Math = MATH,
    Reading = READ,
    Science = SCIE
  ) %>%
  filter(!is.na(Gender)) %>%
  mutate(Total = rowMeans(select(., Math, Reading, Science), na.rm = TRUE))

# --- Prepare 2022 tidy data ---
data_2022_tidy <- data_2022_imputed %>%
  transmute(
    Year = "2022",
    Gender = map_gender(ST004D01T),
    Math = PV1MATH,
    Reading = PV1READ,
    Science = PV1SCIE
  ) %>%
  filter(!is.na(Gender)) %>%
  mutate(Total = rowMeans(select(., Math, Reading, Science), na.rm = TRUE))

# --- Combine and pivot longer ---
combined_tidy <- bind_rows(data_2018_tidy, data_2022_tidy) %>%
  pivot_longer(cols = Math:Total, names_to = "Subject", values_to = "Score")

# --- Summarise mean score per group ---
plot_data <- combined_tidy %>%
  group_by(Year, Gender, Subject) %>%
  summarise(MeanScore = mean(Score, na.rm = TRUE), .groups = "drop")

# --- Plot: 2018 vs 2022 Change with Arrows (Total, Professional Colors) ---
# Aggregate mean scores per subject for each year (all students)
total_arrow_data <- combined_tidy %>%
  group_by(Year, Subject) %>%
  summarise(MeanScore = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  filter(Subject %in% c("Math", "Reading", "Science", "Total")) %>%
  tidyr::pivot_wider(names_from = Year, values_from = MeanScore) %>%
  filter(!is.na(`2018`), !is.na(`2022`)) %>%
  mutate(
    Change = `2022` - `2018`,
    ArrowColor = ifelse(Change > 0, "#2ca02c", "#d62728") # green up, red down
  )

# Calculate triangle positions for decreases only
triangle_offset <- 6  # adjust as needed for visual separation

total_arrow_data <- total_arrow_data %>%
  mutate(
    triangle_y = ifelse(Change < 0, `2022` + triangle_offset, NA),
    triangle_shape = ifelse(Change < 0, 25, NA)
  )

# Define colors
col_2018 <- "#1f77b4"   # strong blue
col_2022 <- "#ff7f0e"   # strong orange
col_up   <- "#2ca02c"   # green
col_down <- "#d62728"   # red

p_arrow_total <- ggplot(total_arrow_data, aes(x = Subject)) +
  geom_point(aes(y = `2018`, shape = "2018", color = "2018"), size = 5) +
  geom_point(data = subset(total_arrow_data, Change >= 0),
             aes(y = `2022`, shape = "2022", color = "2022"), size = 5) +
  geom_point(data = subset(total_arrow_data, Change < 0),
             aes(y = `2022`, shape = "down_triangle", color = "down_triangle"), size = 6) +
  geom_text(aes(y = `2018`, label = round(`2018`, 1)), vjust = 1.7, size = 4, color = col_2018, fontface = "bold") +
  geom_text(aes(y = `2022`, label = round(`2022`, 1)), vjust = -1.2, size = 4, color = col_down, fontface = "bold") +
  scale_color_manual(
    name = "Year",
    values = c("2018" = col_2018, "2022" = col_2022, "down_triangle" = col_down),
    labels = c("2018", "2022 (change: increase)", "2022 (change: decrease)")
  ) +
  scale_shape_manual(
    name = "Year",
    values = c("2018" = 16, "2022" = 17, "down_triangle" = 25),
    labels = c("2018", "2022 (change: increase)", "2022 (change: decrease)")
  ) +
  labs(
    title = "Change in PISA Subject and Total Scores: 2018 to 2022",
    x = "Subject (and Total)",
    y = "Mean Score"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

ggsave("figures/subject_trend_arrows_total.png", plot = p_arrow_total, width = 10, height = 6, dpi = 300, bg = "white")

# --- Faceted Line Plot: Mean Score per Subject, Gender, and Year ---
library(ggplot2)

p_line_gender <- ggplot(plot_data, aes(x = Subject, y = MeanScore, group = Year, color = Gender)) +
  geom_line(aes(linetype = Year), size = 1.2, position = position_dodge(width = 0.2)) +
  geom_point(aes(shape = Year), size = 4, position = position_dodge(width = 0.2)) +
  facet_wrap(~Gender) +
  scale_color_manual(values = c("Female" = "#e60073", "Male" = "#003399")) +
  scale_shape_manual(values = c("2018" = 16, "2022" = 17)) +
  scale_linetype_manual(values = c("2018" = "solid", "2022" = "dashed")) +
  labs(
    title = "PISA Subject Scores by Gender and Year",
    x = "Subject",
    y = "Average Score",
    color = "Gender",
    shape = "Year",
    linetype = "Year"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("figures/pisa_subject_scores_faceted_lineplot.png", plot = p_line_gender, width = 10, height = 6, dpi = 300, bg = "white")






