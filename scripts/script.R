# intall packages # Load required libraries
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("treemapify")
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

# Create figures folder if it doesn't exist
if (!dir.exists("../figures")) dir.create("../figures")

### Load and Explore Data for 2018 
# Load 2018 
setwd("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/data/deployed_data")
load("pisa2018.Rdata")
# Explore 2018 
# Check column names if data frame exists
cat("=================2018 Data & Preprocessing=================")
if ("newdata" %in% ls()) {
  cat("\nColumn names in `newdata`:\n")
  cat("====================================================\n")
  print(colnames(newdata))
} else {
  cat("\nData frame `newdata` not found. Please check the object name.\n")
}
# Check for missing values
cat("\nChecking for missing values in the original dataset:\n")
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
### Load and Explore Data for 2022
# Print table for Greece
cat("\nDescriptive Statistics for Greece:\n")
cat("====================================================\n")
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
cat("====================================================\n")
kable(head(summary_top_countries, 10), digits = 2, caption = "Top 10 countries by overall average score")

# Gender distribution by country
gender_distribution <- data %>%
  group_by(CNT, ST004D01T) %>%
  summarize(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ST004D01T, values_from = Count, values_fill = 0)

# Print gender distribution table
cat("\nGender Distribution by Country:\n")
cat("====================================================\n")
kable(gender_distribution, caption = "Student gender distribution by country")

### Plots 2018 

# --- PLOT 1: Bar plot comparing Greece to the top-performing country in each subject (Math, Reading, Science) ---
# Note: comparing Greece against the best-performing country in each subject separately
# Step 1: Country-level average scores
ranking_df <- data %>%
  group_by(CNT) %>%
  summarize(
    Reading = mean(READ, na.rm = TRUE),
    Math    = mean(MATH, na.rm = TRUE),
    Science = mean(SCIE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Country = CNT)

# Step 2: Top-performing country per subject
top_scores <- ranking_df %>%
  pivot_longer(cols = Reading:Science, names_to = "Subject", values_to = "Score") %>%
  group_by(Subject) %>%
  slice_max(Score, n = 1, with_ties = FALSE) %>%
  mutate(Group = "Top Country")

# Step 3: Greece's scores
greece_scores <- ranking_df %>%
  filter(Country == "Greece") %>%
  pivot_longer(cols = Reading:Science, names_to = "Subject", values_to = "Score") %>%
  mutate(Group = "Greece")

# Step 4: Combine data
plot_data <- bind_rows(top_scores, greece_scores)

# Step 5: Compute difference (Δ)
plot_data_diff <- plot_data %>%
  pivot_wider(names_from = Group, values_from = Score) %>%
  mutate(Difference = `Top Country` - Greece) %>%
  filter(!is.na(Difference) & !is.na(`Top Country`) & !is.na(Greece))  # ✅ Filter to avoid warnings

# Step 6: Clean color palette
optimal_colors <- c("Greece" = "#0072B2", "Top Country" = "#D55E00")

# Step 7: Subtitle generation (one top country per subject)
subtitle_text <- paste(
  "Top countries:",
  paste0("Reading - ", top_scores$Country[top_scores$Subject == "Reading"], ", "),
  paste0("Math - ", top_scores$Country[top_scores$Subject == "Math"], ", "),
  paste0("Science - ", top_scores$Country[top_scores$Subject == "Science"])
)

# Step 8: Final plot
p1 <- ggplot(plot_data, aes(x = Subject, y = Score, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.48) +

  # Bar value labels
  geom_text(aes(label = round(Score, 1)),
            position = position_dodge(width = 0.5),
            vjust = -0.8, size = 4, fontface = "bold") +

  # Difference annotations (Δ)
  geom_text(data = plot_data_diff,
            aes(x = Subject,
                y = pmax(`Top Country`, Greece) + 10,
                label = paste0("Δ ", round(Difference, 1), " pts")),
            inherit.aes = FALSE,
            size = 4, fontface = "italic", color = "darkred") +

  # Scales
  scale_fill_manual(values = optimal_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), limits = c(0, NA)) +

  # Labels and title
  labs(
    title = "Greece vs Top-Performing Country by Subject (PISA 2018)",
    subtitle = subtitle_text,
    x = "Subject",
    y = "Average Score",
    fill = NULL
  ) +

  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

# Step 9: Save
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_1_greece_vs_top_subjects_diff.png", plot = p1, width = 10, height = 6, dpi = 300, bg = "white")
cat("✅ Plot 1 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_1_greece_vs_top_subjects_diff.png\n")

# End Plot 1 : ================================================================


# --- PLOT 2: Boxplots of country medians for each subject, highlighting Greece and OECD median ---
# Define OECD countries
oecd_countries <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Hungary",
  "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia",
  "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
  "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden",
  "Switzerland", "Turkey", "United Kingdom", "United States" # "Greece",
)

# STEP 1: Compute subject medians for countries
base_medians <- data_imputed %>%
  group_by(CNT) %>%
  summarise(
    Reading = median(READ, na.rm = TRUE),
    Math = median(MATH, na.rm = TRUE),
    Science = median(SCIE, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 2: Compute GLCM medians (only valid ones)
glcm_medians <- data_imputed %>%
  filter(!is.na(GLCM)) %>%
  group_by(CNT) %>%
  summarise(`Global Competence` = median(GLCM, na.rm = TRUE), .groups = "drop")

# STEP 3: Merge medians
country_medians <- full_join(base_medians, glcm_medians, by = "CNT")

# STEP 4: Long format and remove missing GLCM entries
long_df <- country_medians %>%
  pivot_longer(cols = Reading:`Global Competence`, names_to = "Subject", values_to = "Score") %>%
  filter(!is.na(Score))

# STEP 5: Greece-only data (filtered correctly)
greece_medians <- long_df %>%
  filter(CNT == "Greece")

# STEP 6: OECD-wide medians (per subject)
oecd_medians <- data_imputed %>%
  filter(CNT %in% oecd_countries) %>%
  summarise(
    Reading = median(READ, na.rm = TRUE),
    Math = median(MATH, na.rm = TRUE),
    Science = median(SCIE, na.rm = TRUE),
    `Global Competence` = median(GLCM, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Subject", values_to = "OECD_Median") %>%
  filter(!is.na(OECD_Median))

# STEP 7: Rename facets
subject_labels <- c(
  "Reading" = "Reading",
  "Math" = "Math",
  "Science" = "Science",
  "Global Competence" = "Global Competence"
)

# STEP 8: Final plot
p2 <- ggplot(long_df, aes(x = "", y = Score)) +
  geom_boxplot(fill = "#e6e6e6", color = "gray50", outlier.color = "gray70", width = 0.3) +

  # OECD median line
  geom_hline(data = oecd_medians, aes(yintercept = OECD_Median, linetype = "OECD Median"),
             color = "#D55E00", linewidth = 1) +

  # Greece point
  geom_point(data = greece_medians, aes(x = "", y = Score, color = "Greece"), size = 3) +

  facet_wrap(~Subject, nrow = 1, labeller = labeller(Subject = subject_labels)) +

  labs(
    title = "Greece Compared to Global and OECD Medians (PISA 2018)",
    x = NULL, y = "Country-Level Median Score",
    color = NULL, linetype = NULL
  ) +

  scale_color_manual(values = c("Greece" = "#0072B2")) +
  scale_linetype_manual(values = c("OECD Median" = "dashed")) +

  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "top",
    legend.box = "horizontal",
    legend.text = element_text(size = 11),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Save final version
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_2_greece_filtered_glcm_boxplots.png", plot = p2, width = 12, height = 5, dpi = 300, bg = "white")
cat("✅ Plot 2 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_2_greece_filtered_glcm_boxplots.png\n")
# End Plot 2 : ================================================================


# --- PLOT 3: Greece vs Higher-Scoring Countries in Global Competence (PISA 2018) ---

# Step 1: Identify the Global Competence column
glcm_col <- grep("GLO|GLCM|global", names(data), ignore.case = TRUE, value = TRUE)[1]
stopifnot(!is.na(glcm_col))

# Step 2: Compute country-level average GLCM scores and rank them
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

# Step 4: Prepare data for plotting
plot_data <- bind_rows(above_greece, greece_row) %>%
  mutate(Highlight = if_else(Country == "Greece", "Greece", "Other"))

# Step 5: Create a global average line with label
global_avg <- round(mean(glcm_summary$GLCM_Mean, na.rm = TRUE), 1)
baseline_label <- paste0("Global Average: ", global_avg)
baseline_df <- data.frame(y = global_avg, label = baseline_label)

# Step 6: Create the plot
p3 <- ggplot(plot_data, aes(x = reorder(Country, -GLCM_Mean), y = GLCM_Mean, fill = Highlight)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(GLCM_Mean, 1)), vjust = -0.5, size = 3.3, fontface = "bold", color = "black") +
  geom_hline(data = baseline_df, aes(yintercept = y, linetype = label), color = "darkred", linewidth = 0.9) +
  scale_fill_manual(
    name = NULL,
    values = c("Greece" = "#1f77b4", "Other" = "#cccccc"),
    labels = c("Greece", "Other Countries")
  ) +
  scale_linetype_manual(
    name = NULL,
    values = setNames("dashed", baseline_label)
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2, override.aes = list(color = "darkred"))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Greece and Higher-Scoring Countries in Global Competence (PISA 2018)",
    subtitle = paste("Greece ranked", greece_rank, "out of", nrow(glcm_summary),
                     "countries —", greece_rank - 1, "scored higher"),
    x = NULL,
    y = "Average Global Competence Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.text = element_text(size = 11),
    plot.margin = margin(10, 10, 10, 10)
  )

# Step 7: Save the plot
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_3_glcm_plot.png", plot = p3, width = 10, height = 6, dpi = 320, bg = "white")
cat("✅ Plot 3 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_3_glcm_plot.png\n")



# --- Plot 4: Gendered Scores – Grouped by Gender, Pink/Blue Palette ---
# Step 1: Detect Global Competence column
glcm_col <- grep("GLO|GLCM|global", names(data), ignore.case = TRUE, value = TRUE)[1]
stopifnot(!is.na(glcm_col))

# Step 2: Prepare and recode data
df <- data_imputed %>%
  rename(
    Country = CNT,
    Gender = ST004D01T,
    Reading = READ,
    Math = MATH,
    Science = SCIE,
    GlobalCompetence = !!glcm_col
  ) %>%
  filter(
    !is.na(Gender), !is.na(Reading), !is.na(Math),
    !is.na(Science), !is.na(GlobalCompetence)
  ) %>%
  mutate(Gender = recode(Gender, `1` = "Male", `2` = "Female"))

# Step 3: Compute averages per subject and group
subject_avg <- df %>%
  group_by(Country, Gender) %>%
  summarise(
    Reading = mean(Reading, na.rm = TRUE),
    Math = mean(Math, na.rm = TRUE),
    Science = mean(Science, na.rm = TRUE),
    `Global Competence` = mean(GlobalCompetence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = Reading:`Global Competence`, names_to = "Subject", values_to = "Score")

# Step 4: Define groups
greece <- subject_avg %>%
  filter(Country == "Greece") %>%
  mutate(Group = "Greece")

oecd_avg <- subject_avg %>%
  filter(Country %in% oecd_countries, Country != "Greece") %>%
  group_by(Gender, Subject) %>%
  summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Group = "OECD")

global_avg <- subject_avg %>%
  filter(Country != "Greece") %>%
  group_by(Gender, Subject) %>%
  summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Group = "Global")

# Step 5: Combine and control order (Females first, then Males)
plot_data <- bind_rows(greece, oecd_avg, global_avg) %>%
  mutate(
    BarGroup = paste(Group, Gender, sep = "_"),
    BarGroup = factor(BarGroup, levels = c(
      "Greece_Female", "OECD_Female", "Global_Female",
      "Greece_Male", "OECD_Male", "Global_Male"
    ))
  )

# Step 6: Define color palette
fill_colors <- c(
  "Greece_Female" = "#e75480",   # Rose pink
  "OECD_Female"   = "#f2a6c4",   # Orchid pink
  "Global_Female" = "#fbd1df",   # Light pink
  "Greece_Male"   = "#4169e1",   # Royal blue
  "OECD_Male"     = "#7ec8e3",   # Sky blue
  "Global_Male"   = "#b3d9f7"    # Light sky blue
)

# Step 7: Plot
p_gendered <- ggplot(plot_data, aes(x = Subject, y = Score, fill = BarGroup)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0.1), width = 0.7) +
  scale_fill_manual(
    values = fill_colors[c(
      "Greece_Female", "OECD_Female", "Global_Female",
      "Greece_Male", "OECD_Male", "Global_Male"
    )],
    name = "Group",
    labels = c(
      "Greece (F)",
      "OECD (F)",
      "Global (F)",
      "Greece (M)",
      "OECD (M)",
      "Global (M)"
    )
  ) +
  labs(
    title = "PISA 2018: Subject Scores by Gender and Country Group",
    x = NULL,
    y = "Mean Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 11)
  )

# Step 8: Save
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_4_gendered_subject_scores_pink_blue_grouped_gender.png",
       plot = p_gendered, width = 10, height = 6, dpi = 300, bg = "white")
cat("✅ Plot 4 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_4_gendered_subject_scores_pink_blue_grouped_gender.png\n")

# End Plot 4 : ================================================================

# --- PLOT 5:  Greece: Gender Differences in Global Competence vs Academic Performance (PISA 2018)---
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
# --- PLOT 5: Scatter/arrow plots of Greece's Global Competence vs Academic mean by gender ---
# Shows, for each gender, how Greece's mean Global Competence compares to its mean academic score, with a perpendicular distance arrow.
g <- arrangeGrob(
  plot_female + ggtitle("Greece – Female"),
  plot_male + ggtitle("Greece – Male"),
  ncol = 2,
  top = textGrob("Greece's Global Competence vs Academic Scores by Gender", gp = gpar(fontsize = 15, fontface = "bold"))
)
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_5_glcm_vs_academic_gender_greece_dist_legend.png", g, width = 13, height = 6, dpi = 300, bg = "white")
cat("✅ Plot 5 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_5_glcm_vs_academic_gender_greece_dist_legend.png\n")
# End Plot 5 : ================================================================

### Combine Data 2018 with 2022 for comaprson with the latest data
### dependence: `load_pisa_data_2022.R` script

# --- PLOT 6:  ---
# after preprocess data for 2018 
data_2018 <- data
# Select relevant columns
data_2018 <- data_2018 %>%
  select(CNT, ST004D01T, MATH, READ, SCIE)

# Load 2022 data
load("pisa2022.RData")
data_2022 <- pisa2022_imputed

# Filter Greece data
greece_2018 <- data_2018 %>% filter(CNT == "Greece")
greece_2022 <- data_2022 %>% filter(CNT == "Greece")

# Compute mean scores per subject and gender for each year
mean_2018 <- greece_2018 %>%
  group_by(ST004D01T) %>%
  summarise(across(c(MATH, READ, SCIE), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = MATH:SCIE, names_to = "Subject", values_to = "Mean2018")

mean_2022 <- greece_2022 %>%
  group_by(ST004D01T) %>%
  summarise(across(c(MATH, READ, SCIE), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = MATH:SCIE, names_to = "Subject", values_to = "Mean2022")

# Combine and compute score change
score_change <- left_join(mean_2018, mean_2022, by = c("ST004D01T", "Subject")) %>%
  mutate(Change = Mean2022 - Mean2018) %>%
  rename(Gender = ST004D01T)

# Create plot
p_greece_change <- ggplot(score_change, aes(x = Subject, y = Change, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Male" = "#89CFF0", "Female" = "#FFB6C1")) +  # light blue and light pink
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Score Change by Subject and Gender in Greece (2018–2022)",
    x = "Subject",
    y = "Change in Score",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

# Save to figures
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_6_score_change_greece_2018_2022.png", plot = p_greece_change,
       width = 10, height = 6, dpi = 300, bg = "white")
cat("✅ Plot 6 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_6_score_change_greece_2018_2022.png\n")
# End Plot 6: ================================================================

# --- PLOT 7:  Heatmap of all subject Score Change by Country (2018–2022)---
# --- PLOT 7:  Heatmap of all subject Score Change by Country (2018–2022)---
library(dplyr)
library(tidyr)
library(ggplot2)
library(fs)

# Compute mean scores by gender and subject for each year
get_mean_scores <- function(data, year_label) {
  data %>%
    pivot_longer(cols = c(MATH, READ, SCIE), names_to = "Subject", values_to = "Score") %>%
    mutate(Gender = ST004D01T) %>%
    group_by(CNT, Gender, Subject) %>%
    summarise(Mean = mean(Score, na.rm = TRUE), .groups = "drop") %>%
    mutate(Year = year_label)
}

mean_2018 <- get_mean_scores(data_2018, "2018")
mean_2022 <- get_mean_scores(data_2022, "2022")

# Combine and calculate change
score_changes <- bind_rows(mean_2018, mean_2022) %>%
  pivot_wider(names_from = Year, values_from = Mean) %>%
  mutate(Change = `2022` - `2018`) %>%
  drop_na(CNT, Gender, Subject, Change) %>%
  filter(Change != 0) %>%
  mutate(Combo = paste(Gender, Subject, sep = "_"),
         IsGreece = CNT == "Greece")

# Order countries by average change for visual consistency
score_changes <- score_changes %>%
  group_by(CNT) %>%
  mutate(AvgChange = mean(Change, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(CNT = reorder(CNT, AvgChange))

# Create output folder
dir_create("figures")

# Plot with thick blue outline for Greece
p <- ggplot(score_changes, aes(x = Combo, y = CNT, fill = Change)) +
  geom_tile(width = 1, height = 1, color = "grey90") +
  geom_tile(data = filter(score_changes, IsGreece), 
            aes(x = Combo, y = CNT), 
            width = 1, height = 1, color = "blue", linewidth = 1.2, fill = NA) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "darkgreen", midpoint = 0, name = "Score Change") +
  labs(title = "Score Change (2018–2022) by Country, Gender, and Subject", x = "Gender–Subject", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

# Save plot
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_7_all_gender_subject_score_change.png", plot = p, width = 11, height = 13, dpi = 300, bg = "white")
cat("✅ Plot 7 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_7_all_gender_subject_score_change.png\n")

# End Plot 7 : ================================================================

# --- PLOT 8:  
# --- Plot: Greece's Rank vs Global Average (PISA 2018) ---

library(dplyr)
library(tidyr)
library(ggplot2)
library(fs)

# Step 1: Compute average scores per country
country_averages_2018 <- data_2018 %>%
  group_by(CNT) %>%
  summarise(
    Math = mean(MATH, na.rm = TRUE),
    Reading = mean(READ, na.rm = TRUE),
    Science = mean(SCIE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Math), !is.na(Reading), !is.na(Science)) %>%
  mutate(Total = (Math + Reading + Science) / 3)

# Step 2: Rank globally (lower is better)
ranked <- country_averages_2018 %>%
  mutate(
    Math_Rank = min_rank(desc(Math)),
    Reading_Rank = min_rank(desc(Reading)),
    Science_Rank = min_rank(desc(Science)),
    Total_Rank = min_rank(desc(Total))
  )

# Step 3: Extract Greece's rank per subject
greece_ranks <- ranked %>%
  filter(CNT == "Greece") %>%
  select(Math_Rank, Reading_Rank, Science_Rank, Total_Rank) %>%
  pivot_longer(everything(), names_to = "Subject", values_to = "GreeceRank") %>%
  mutate(
    Subject = gsub("_Rank", "", Subject),
    Subject = factor(Subject, levels = c("Math", "Reading", "Science", "Total"))
  )

# Step 4: Build stacked bar data
total_countries <- nrow(ranked)
average_rank <- round(mean(1:total_countries))

plot_data <- greece_ranks %>%
  mutate(Remaining = total_countries - GreeceRank) %>%
  select(Subject, Greece = GreeceRank, `Countries Ranked Lower` = Remaining) %>%
  pivot_longer(cols = c("Greece", "Countries Ranked Lower"),
               names_to = "Category", values_to = "Count") %>%
  mutate(Category = factor(Category, levels = c("Greece", "Countries Ranked Lower")))

# Step 5: Plot
p <- ggplot(plot_data, aes(x = Subject, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = average_rank, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 0.4, y = average_rank + 1.5,
           label = paste0("Global Average Rank (", average_rank, ")"),
           color = "red", size = 4, hjust = 0) +
  scale_fill_manual(
    values = c("Greece" = "#1E90FF", "Countries Ranked Lower" = "gray85"),
    labels = c("Countries Ranked Higher Than Greece", "Countries Ranked Lower Than Greece"),
    name = "Ranking Breakdown"
  ) +
  scale_y_continuous(
    limits = c(0, total_countries),
    breaks = seq(0, total_countries, 10)
  ) +
  labs(
    title = "Greece's Global Rank by Subject (PISA 2018)",
    subtitle = "Blue bar = Countries ranked higher than Greece; Red line = Global average rank",
    x = "Subject",
    y = "Number of Countries"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top"
  )

# Step 6: Save
dir_create("figures")
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_8_greece_rank_vs_avg_annotated_2018.png", plot = p, width = 9, height = 5.5, dpi = 300, bg = "white")
cat("✅ Plot 8 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_8_greece_rank_vs_avg_annotated_2018.png\n")
# End Plot 8 : ================================================================

# --- PLOT 9:  ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(fs)

# Step 1: Compute gender gaps per country and subject + GLCM
gender_gap_df <- newdata %>%
  filter(ST004D01T %in% c("Male", "Female")) %>%
  pivot_longer(cols = c(MATH, READ, SCIE, GLCM), names_to = "Subject", values_to = "Score") %>%
  group_by(CNT, Subject, ST004D01T) %>%
  summarise(MeanScore = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ST004D01T, values_from = MeanScore) %>%
  mutate(GenderGap = Female - Male) %>%
  drop_na(GenderGap)

# Step 2: Format for heatmap
heatmap_data <- gender_gap_df %>%
  mutate(IsGreece = CNT == "Greece") %>%
  select(CNT, Subject, GenderGap, IsGreece)

# Step 3: Order countries by average gender gap
country_order <- heatmap_data %>%
  group_by(CNT) %>%
  summarise(AvgGap = mean(GenderGap, na.rm = TRUE)) %>%
  arrange(desc(AvgGap)) %>%
  pull(CNT)

heatmap_data <- heatmap_data %>%
  mutate(
    CNT = factor(CNT, levels = country_order),
    Subject = factor(Subject, levels = c("MATH", "READ", "SCIE", "GLCM"))
  )

# Step 4: Plot
p <- ggplot(heatmap_data, aes(x = Subject, y = CNT, fill = GenderGap)) +
  geom_tile(color = ifelse(heatmap_data$IsGreece, "blue", NA), linewidth = 1.1) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0,
                       name = "Gap (F − M)") +
  labs(
    title = "Gender Gap in PISA 2018 Scores by Country and Subject (incl. Global Competence)",
    subtitle = "Positive = Girls outperform Boys | Greece outlined in blue",
    x = "Subject", y = "Country"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Step 5: Save
dir_create("figures")
ggsave("/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_9_gender_gap_heatmap_with_glcm.png", plot = p, width = 11, height = 12, dpi = 300, bg = "white")

cat("✅ Plot 9 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/plot_9_gender_gap_heatmap_with_glcm.png\n")
# End Plot 9: ================================================================

# --- PLOT 10:  ---
# Step 1: OECD membership
oecd_members <- c(
  "Albania" = FALSE, "Argentina" = FALSE, "Australia" = TRUE, "Austria" = TRUE,
  "Belgium" = TRUE, "Brazil" = FALSE, "Bulgaria" = FALSE, "Canada" = TRUE,
  "Chile" = TRUE, "Colombia" = TRUE, "Costa Rica" = TRUE, "Croatia" = FALSE,
  "Czech Republic" = TRUE, "Denmark" = TRUE, "Estonia" = TRUE, "Finland" = TRUE,
  "France" = TRUE, "Georgia" = FALSE, "Germany" = TRUE, "Greece" = TRUE,
  "Hong Kong" = FALSE, "Hungary" = TRUE, "Iceland" = TRUE, "India" = FALSE,
  "Indonesia" = FALSE, "Ireland" = TRUE, "Israel" = TRUE, "Italy" = TRUE,
  "Japan" = TRUE, "Jordan" = FALSE, "Kazakhstan" = FALSE, "Korea" = TRUE,
  "Latvia" = TRUE, "Lithuania" = TRUE, "Luxembourg" = TRUE, "Malaysia" = FALSE,
  "Mexico" = TRUE, "Moldova" = FALSE, "Montenegro" = FALSE, "Morocco" = FALSE,
  "Netherlands" = TRUE, "New Zealand" = TRUE, "North Macedonia" = FALSE,
  "Norway" = TRUE, "Peru" = FALSE, "Philippines" = FALSE, "Poland" = TRUE,
  "Portugal" = TRUE, "Qatar" = FALSE, "Romania" = FALSE, "Russian Federation" = FALSE,
  "Saudi Arabia" = FALSE, "Serbia" = FALSE, "Singapore" = FALSE, "Slovak Republic" = TRUE,
  "Slovenia" = TRUE, "Spain" = TRUE, "Sweden" = TRUE, "Switzerland" = TRUE,
  "Thailand" = FALSE, "Tunisia" = FALSE, "Turkey" = TRUE, "Ukraine" = FALSE,
  "United Arab Emirates" = FALSE, "United Kingdom" = TRUE, "United States" = TRUE,
  "Uruguay" = FALSE, "Vietnam" = FALSE, "Chinese Taipei" = FALSE,
  "B-S-J-Z (China)" = FALSE
)
# Step 2: Continent mapping
continent_map <- list(
  "Europe" = c("Austria", "Belgium", "France", "Germany", "Greece", "Italy", "Netherlands", "Spain", "Sweden", "Switzerland", "United Kingdom", "Poland", "Czech Republic", "Portugal", "Slovak Republic", "Hungary", "Finland", "Ireland", "Denmark", "Estonia", "Latvia", "Lithuania", "Slovenia", "Norway", "Iceland", "Luxembourg"),
  "Asia" = c("Japan", "Korea", "Singapore", "Hong Kong", "Chinese Taipei", "Malaysia", "Thailand", "Vietnam", "B-S-J-Z (China)", "Indonesia", "Kazakhstan", "Qatar", "Saudi Arabia", "Jordan", "United Arab Emirates", "Israel"),
  "Americas" = c("United States", "Canada", "Mexico", "Chile", "Colombia", "Costa Rica", "Argentina", "Brazil", "Uruguay", "Peru"),
  "Africa" = c("Morocco", "Tunisia"),
  "Oceania" = c("Australia", "New Zealand")
)
# Step 3: Build dataset
continent_lookup <- unlist(lapply(names(continent_map), function(continent) {
  setNames(rep(continent, length(continent_map[[continent]])), continent_map[[continent]])
}))
all_countries <- names(oecd_members)
plot_df <- data.frame(
  Country = all_countries,
  OECD = unname(unlist(oecd_members)),
  Continent = continent_lookup[all_countries]
)
plot_df <- plot_df[!is.na(plot_df$Continent), ]
plot_df$Count <- 1
summary_df <- aggregate(Count ~ Continent + OECD, data = plot_df, FUN = sum)
# Heights for arrows
europe_total <- sum(summary_df$Count[summary_df$Continent == "Europe"])
asia_non_oecd <- summary_df$Count[summary_df$Continent == "Asia" & summary_df$OECD == FALSE]
# Step 4: Final Plot (complete chain!)
p <- ggplot(summary_df, aes(x = Continent, y = Count, fill = OECD)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("TRUE" = "#1E90FF", "FALSE" = "gray70"),
                    labels = c("Non-OECD", "OECD"), name = "Membership") +
  labs(
    title = "<b>Number of PISA 2018 Countries per Continent by OECD Membership (PISA 2018)</b>",
    subtitle = "Greece (Europe, OECD) and China (Asia, Non-OECD)",
    x = "Continent",
    y = "Number of Countries"
  ) +
  annotate("text", x = 4.5, y = europe_total + 2, label = "Greece", color = "red", size = 5, hjust = 0) +
  annotate("segment", x = 4.4, xend = 4, y = europe_total + 1.5, yend = europe_total - 1,
           arrow = arrow(length = unit(0.25, "cm")), color = "red", linewidth = 1) +
  annotate("text", x = 2.5, y = asia_non_oecd + 2, label = "China", color = "red", size = 5, hjust = 1) +
  annotate("segment", x = 2.6, xend = 3, y = asia_non_oecd + 1.5, yend = asia_non_oecd - 0.5,
           arrow = arrow(length = unit(0.25, "cm")), color = "red", linewidth = 1) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = ggtext::element_markdown(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
# Show and save
print(p)
ggsave(
  filename = "/Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/10_plot_oecd_continent_greece_china_arrow.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)
cat("✅ Plot 10 saved to: /Users/evangeliapanourgia/Desktop/pisa_refactor/vizRTableau/figures/10_plot_oecd_continent_greece_china_arrow.png\n")
# End Plot 10: ================================================================
