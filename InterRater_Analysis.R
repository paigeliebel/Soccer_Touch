
#Inter-Rater Analysis

#Goal is to determine how "similar" raters were
#We have a total of 12 matches that all raters watched

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library(dplyr)
library(irr)
library(fuzzyjoin)
library(knitr)


source("Data_Management.R") #Runs and brings in Matches_final from Data_Management.R script

#Dataframe "Touches_interrater" contains all the touches recorded for this analysis

# Should I exclude
# Situations excluded from core analysis for these hypotheses: Goals For/Against, Substitutions
Exclude_Touch <- c("TA", "CO", "NEG")
Exclude_Situation <- c("GF", "GA", "SUB")

Interrater <- Touches_interrater %>%  #duplicate and filter
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation))

############################ Simple Frequency Check ############################

#Count check per match (simply how many each rater saw)

touch_counts <- Interrater %>%
  group_by(SeasonMatchNumber, Rater) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Plot frequency per match per rater
touch_irr <- ggplot(touch_counts, aes(x = factor(SeasonMatchNumber), y = TouchCount, fill = Rater)) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +  # Remove outlines
  scale_fill_grey(start = 0.3, end = 0.7, name = "Rater") +
  labs(
    title = "Touch Count per Match by Rater",
    x = "Season Match Number",
    y = "Touch Count"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )


###ICC - Intraclass Correlation Coefficient - Interval data:

# reshape your data: rows = matches, columns = raters
icc_data <- Interrater %>%
  group_by(SeasonMatchNumber, Rater) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Rater, values_from = Count)

# Apply ICC (two-way random effects model)
icc_result <- icc(icc_data[,-1], model = "twoway", type = "agreement", unit = "single")

print(icc_result)

############################ Simple Reciprocity Check ############################


Reciprocal_IR <- Touches_interrater %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation)) %>%
  mutate(
    Reciprocity = str_trim(Reciprocal),
    ReciprocityType = case_when(
      Reciprocity %in% c("Y", "G") ~ "Reciprocal",
      Reciprocity == "N" ~ "NonReciprocal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ReciprocityType))

#Count how many touches each rater coded as reciprocal vs non-reciprocal 
Recip_Counts <- Reciprocal_IR %>%
  group_by(SeasonMatchNumber, Rater, ReciprocityType) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ReciprocityType, values_from = Count, values_fill = 0)


#Group by match and rater
Recip_Counts <- Reciprocal_IR %>%
  group_by(SeasonMatchNumber, Rater, ReciprocityType) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = ReciprocityType, values_from = Count, values_fill = 0)

icc_data_recip <- Recip_Counts %>%
  select(SeasonMatchNumber, Rater, Reciprocal) %>%
  pivot_wider(names_from = Rater, values_from = Reciprocal)

icc_result_recip <- icc(icc_data_recip[,-1], model = "twoway", type = "agreement", unit = "single")

print(icc_result_recip)

icc_data_nonrecip <- Recip_Counts %>%
  select(SeasonMatchNumber, Rater, NonReciprocal) %>%
  pivot_wider(names_from = Rater, values_from = NonReciprocal)

icc_result_nonrecip <- icc(icc_data_nonrecip[,-1], model = "twoway", type = "agreement", unit = "single")

print(icc_result_nonrecip)

#Make some charts

# Filter for reciprocal touches
recip_counts_plot <- Reciprocal_IR %>%
  filter(ReciprocityType == "Reciprocal") %>%
  group_by(SeasonMatchNumber, Rater) %>%
  summarise(Count = n(), .groups = "drop")

# Plot
ggplot(recip_counts_plot, aes(x = factor(SeasonMatchNumber), y = Count, fill = Rater)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Reciprocal Touch Count per Match by Rater",
    x = "Season Match Number",
    y = "Reciprocal Touch Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter for non-reciprocal touches
nonrecip_counts_plot <- Reciprocal_IR %>%
  filter(ReciprocityType == "NonReciprocal") %>%
  group_by(SeasonMatchNumber, Rater) %>%
  summarise(Count = n(), .groups = "drop")

# Plot
ggplot(nonrecip_counts_plot, aes(x = factor(SeasonMatchNumber), y = Count, fill = Rater)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Non-Reciprocal Touch Count per Match by Rater",
    x = "Season Match Number",
    y = "Non-Reciprocal Touch Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##########Global
# Total: Keep everything except TA, CO, NEG (but DO allow "IT")
Interrater_Total <- Touches_interrater %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation)) %>%
  group_by(Rater) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(TouchType = "Total")

# Recip / NonRecip: Apply stricter filter (exclude IT)
Interrater_RecipFiltered <- Touches_interrater %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation)) %>%
  mutate(
    Reciprocity = str_trim(Reciprocal),
    TouchType = case_when(
      Reciprocity %in% c("Y", "G") ~ "Reciprocal",
      Reciprocity == "N" ~ "NonReciprocal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(TouchType))

Interrater_Specific <- Interrater_RecipFiltered %>%
  group_by(Rater, TouchType) %>%
  summarise(Count = n(), .groups = "drop")

# Combine all
Global_Touch_Summary <- bind_rows(Interrater_Total, Interrater_Specific) %>%
  mutate(TouchType = factor(TouchType, levels = c("Total", "Reciprocal", "NonReciprocal")))

irr_global <- ggplot(Global_Touch_Summary, aes(x = TouchType, y = Count, fill = Rater)) +
  geom_bar(stat = "identity", position = "dodge", color = NA) +  # Remove outlines
  scale_fill_grey(start = 0.3, end = 0.7, name = "Rater") +
  labs(
    title = "Global Comparison of Touch Counts by Rater",
    x = "Touch Type",
    y = "Total Count"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )


##ICC Table

# Build total count per match/rater
icc_data_total <- Touches_interrater %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation)) %>%  # NOTE: only GF, GA, SUB excluded (not IT)
  group_by(SeasonMatchNumber, Rater) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Rater, values_from = Count)

# Already done earlier:
icc_result_total <- icc(icc_data_total[,-1], model = "twoway", type = "agreement", unit = "single")
icc_result_recip <- icc(icc_data_recip[,-1], model = "twoway", type = "agreement", unit = "single")
icc_result_nonrecip <- icc(icc_data_nonrecip[,-1], model = "twoway", type = "agreement", unit = "single")

# Extract results into a data frame
icc_summary <- tibble(
  TouchType = c("Total", "Reciprocal", "NonReciprocal"),
  ICC = c(icc_result_total$value, icc_result_recip$value, icc_result_nonrecip$value),
  Lower_CI = c(icc_result_total$lbound, icc_result_recip$lbound, icc_result_nonrecip$lbound),
  Upper_CI = c(icc_result_total$ubound, icc_result_recip$ubound, icc_result_nonrecip$ubound)
)

library(knitr)
library(kableExtra)

icc_summary %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  kable(caption = "ICC Values by Touch Type", align = "c") %>%
  kable_styling(full_width = FALSE)


