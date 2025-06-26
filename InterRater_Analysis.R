
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
ggplot(touch_counts, aes(x = factor(SeasonMatchNumber), y = TouchCount, fill = Rater)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Touch Count per Match by Rater",
       x = "Season Match Number",
       y = "Touch Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

# Updated exclusion list
Exclude_Situation_IT <- c("GF", "GA", "SUB", "IT")

Reciprocal_IR <- Touches_interrater %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation_IT)) %>%
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
  filter(!(Situation %in% Exclude_Situation_IT)) %>%
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

ggplot(Global_Touch_Summary, aes(x = TouchType, y = Count, fill = Rater)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Global Comparison of Touch Counts by Rater (Filtered Reciprocities Only)",
    x = "Touch Type",
    y = "Total Count",
    fill = "Rater"
  ) +
  theme_minimal()

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

# ###Pair-wise Difference Per Match | Looks at differences between rater pairs
# 
# # Prepare a compact version of your data
# touch_counts <- Interrater %>%
#   group_by(SeasonMatchNumber, Rater) %>%
#   summarise(TouchCount = n(), .groups = "drop")
# 
# # Self-join to create all rater pairs for the same match
# pairwise_differences <- touch_counts %>%
#   inner_join(touch_counts, by = "SeasonMatchNumber", suffix = c("_R1", "_R2")) %>%
#   filter(Rater_R1 < Rater_R2) %>%  # Avoid duplicate and self-comparisons
#   mutate(
#     AbsoluteDifference = abs(TouchCount_R1 - TouchCount_R2),
#     Difference = TouchCount_R1 - TouchCount_R2
#   )
# 
# ggplot(pairwise_differences, aes(x = SeasonMatchNumber, y = AbsoluteDifference, fill = interaction(Rater_R1, Rater_R2))) +
#   geom_col(position = "dodge") +
#   labs(title = "Pairwise Touch Count Differences by Match",
#        x = "Season Match Number",
#        y = "Absolute Touch Count Difference") +
#   theme_minimal()
# 
# pairwise_differences %>%
#   group_by(Rater_R1, Rater_R2) %>%
#   summarise(
#     MeanAbsDiff = mean(AbsoluteDifference),
#     MaxDiff = max(AbsoluteDifference),
#     .groups = "drop"
#   )
#Simon very different from Paige and Tobi in one single match that throws this all off

############################ Pairing Touches Together ############################

#event-level Interrater Reliability
#Some touches are only seen by one rater
#Some raters will have multiple touches at one time
#Paired Touch = Same time +-2 seconds, Season Match Number, TeamID, Touch Action
#If multiple fall into these buckets, then 'pair' at random. Such that if Rater 1 counts 4 with these criteria
#and Rater 2 has 5 and Rater 3 has 6, then 4 would be paired across 3 raters (1 through 4), 1 across 2 raters (the 5th seen by both Rater 2 and 3), and 1 alone (the last one by Rater 3)


############################ Deeper Dive ############################

#What percent of paired touches are similar?
#As in, within a paired touch, do the raters have the same players, reciprocity, etc?

#This would be for Tobi and Simon
#I feel that what I have already done should be sufficient


