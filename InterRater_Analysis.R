
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

#Should I exclude
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
#Paired Touch = Same time +-2 seconds, Season Match Number, TeamID, Touch Action

# Limit to useful columns and clean first
touches_eventbased <- Interrater %>%
  select(TouchID, SeasonMatchNumber, Team, Time, TouchAction, Rater) %>%
  filter(!is.na(Time))  # just in case

touches_eventbased <- touches_eventbased %>%
  mutate(Time = as.numeric(Time))  # makes Time numeric

# Now do a fuzzy join within ±1 seconds, same match, same team, same action
touch_pairs_eventbased <- fuzzy_inner_join(
  touches_eventbased, touches_eventbased,
  by = c(
    "SeasonMatchNumber" = "SeasonMatchNumber",
    "Team" = "Team",
    "TouchAction" = "TouchAction",
    "Time" = "Time"
  ),
  match_fun = list(`==`, `==`, `==`, function(x, y) abs(x - y) <= 2)
)

# Remove self-matches and ensure we’re only comparing between raters
touch_pairs_filtered_eventbased <- touch_pairs_eventbased %>%
  filter(Rater.x != Rater.y) %>%  # exclude same-rater comparisons
  select(
    SeasonMatchNumber.x,
    Team.x,
    Time.x, TouchID.x, Rater.x,
    Time.y, TouchID.y, Rater.y,
    TouchAction.x
  )

#Count total touches per rater
rater_totals <- Interrater %>%
  filter(Rater %in% c("Rater1", "Rater2", "Rater3")) %>%
  group_by(Rater) %>%
  summarise(TotalTouches = n(), .groups = "drop")

#Count how many of each rater's touches were matched
rater_matched <- touch_pairs_filtered_eventbased %>%
  distinct(TouchID.x, Rater.x) %>%
  group_by(Rater.x) %>%
  summarise(MatchedTouches = n(), .groups = "drop") %>%
  rename(Rater = Rater.x)

#Joins total and matched counts to compute match rate
#how many of those were seen (±2 sec, same team/action/match) by at least one other rater
rater_agreement <- rater_totals %>%
  left_join(rater_matched, by = "Rater") %>%
  mutate(
    MatchedTouches = replace_na(MatchedTouches, 0),
    AgreementRate = MatchedTouches / TotalTouches #the "recall" for that rater, or how well their coded events were corroborated
  )

kable(rater_agreement, digits = 2)

###See how many events were seen by 1, 2, or all 3 raters
# Step 1: Generate unique identifier for matching groups
touch_match_groups <- touch_pairs_filtered_eventbased %>%
  mutate(EventGroupID = paste(SeasonMatchNumber.x, Team.x, TouchAction.x, Time.x, sep = "_")) %>%
  select(EventGroupID, Rater.x, TouchID.x) %>%
  rename(Rater = Rater.x, TouchID = TouchID.x) %>%
  distinct()

# Step 2: Count how many unique raters contributed to each matched event
event_rater_counts <- touch_match_groups %>%
  group_by(EventGroupID) %>%
  summarise(NumRaters = n_distinct(Rater), .groups = "drop")

# Step 3: Tabulate the number of events seen by 1, 2, or all 3 raters
touch_agreement_summary <- event_rater_counts %>%
  count(NumRaters, name = "Count") %>%
  mutate(Percent = round(100 * Count / sum(Count), 1))

# Show the result
kable(touch_agreement_summary)


# Create pie chart
touch_agreement_summary <- touch_agreement_summary %>%
  mutate(AgreementLabel = case_when(
    NumRaters == 1 ~ "Touch event seen by only 1 Rater",
    NumRaters == 2 ~ "Touch event seen by 2 Raters",
    NumRaters == 3 ~ "Touch event seen by all 3 Raters"
  ))

# Define custom colors
custom_colors <- c(
  "Touch event seen by 1 Rater" = "#F4A6A6",  
  "Touch event seen by 2 Raters" = "#A6C8F4", 
  "Touch event seen by 3 Raters" = "#A6F4A6"   
)

# Plot
ggplot(touch_agreement_summary, aes(x = "", y = Count, fill = AgreementLabel)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Touch Agreement by Number of Raters",
       fill = "Rater Agreement") +
  theme_void() +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 4.5)

############################ Deeper Dive ############################

#What percent of paired touches are similar?
#As in, within a paired touch, do the raters have the same players, reciprocity, etc?

