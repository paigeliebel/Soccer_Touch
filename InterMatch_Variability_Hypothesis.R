# Inter-Match Variability Hypothesis
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library (dplyr)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Core_Hypothesis.R") #Runs and brings in data frames from Core_Hypothesis.R script

#note that this is not complete until we get Simon Data (only 2/3s of it so far)
#Ensure to use the correct dfs. Touches_final and Matches_final are correct. They only include assigned rater data, no repeat matches

#Check to make sure data frames are loaded:
if (!exists("Touches_final") | !exists("Matches_final") | !exists("FinalStandings")) {
  stop("Touches_final, Matches_final, or FinalStandings not loaded. Check Data_Management.R.")
}

if (!exists("Touches_final") | !exists("Matches_final") | !exists("FinalStandings") | !exists("Touches_CoreHyp")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded. Check Data_Management.R and Core_Hypothesis.R.")
}

# Step 1: Prosocial touches per team per match
Touches_per_match <- Touches_CoreHyp %>%
  group_by(Team, SeasonMatchNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Step 2: Average touch per team over the season
Team_season_avg <- Touches_per_match %>%
  group_by(Team) %>%
  summarise(SeasonAvgTouch = mean(TouchCount), .groups = "drop")

# Step 3: Merge & calculate scaled deviation from average
Touches_scaled_dev <- Touches_per_match %>%
  left_join(Team_season_avg, by = "Team") %>%
  mutate(ScaledAboveAvg = TouchCount - SeasonAvgTouch)

ggplot(Touch_Goal_Analysis, aes(x = ScaledAboveAvg, y = GoalDiff)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Touch Count Deviation vs Match Goal Differential",
    x = "Touches Above/Below Team Average",
    y = "Goal Differential"
  ) +
  theme_minimal()

model <- lm(GoalDiff ~ ScaledAboveAvg, data = Touch_Goal_Analysis)
summary(model)
