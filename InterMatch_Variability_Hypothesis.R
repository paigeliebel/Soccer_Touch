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
if (!exists("Touches_final") | !exists("Matches_final") | !exists("FinalStandings") | !exists("Touches_CoreHyp")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded. Check Data_Management.R and Core_Hypothesis.R.")
}

############################ Inter-Match Variability Hypothesis ############################ 

# Basically asking: "When a team is more (or less) touchy than usual, do they score more or less goals than their opponent?"
# Note that this is still using same CoreHyp touches (therefore only prosocial touches and not including GF, GA, Subs etc)

#Clean Match column data for use
Matches_final_cleaned <- Matches_final %>%
  mutate(
    GoalsFor = as.numeric(str_trim(GoalsFor)),
    GoalsAgainst = as.numeric(str_trim(GoalsAgainst))
  )

#Get TeamID into Matches_final
Matches_finalID <- Matches_final_cleaned %>%
  mutate(
    MatchID = str_pad(MatchID, width = 4, pad = "0"),  # in case it was shortened
    TeamID = str_sub(MatchID, 1, 2),                     # preserve leading zeros
    GoalDiff = GoalsFor - GoalsAgainst
  )

# Prosocial touches per team per match
Touches_per_match <- Touches_CoreHyp %>%
  group_by(Team, SeasonMatchNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Average touch per team over the season
Team_season_avg <- Touches_per_match %>%
  group_by(Team) %>%
  summarise(SeasonAvgTouch = mean(TouchCount), .groups = "drop")

# Merge & calculate scaled deviation from average
Touches_scaled_dev <- Touches_per_match %>%
  left_join(Team_season_avg, by = "Team") %>%
  mutate(ScaledAboveAvg = TouchCount - SeasonAvgTouch) # positive = more touchy than average, neg = less touchy than average

# Joins dataframes (goal differetials to the touches scaled)
Touch_GoalDiff_Analysis <- Touches_scaled_dev %>%
  left_join(
    Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
    by = c("SeasonMatchNumber", "Team" = "TeamID")
  )

# Visualize 
ggplot(Touch_GoalDiff_Analysis, aes(x = ScaledAboveAvg, y = GoalDiff)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Touch Count Deviation vs Match Goal Differential",
    x = "Touches Above/Below Team Average",
    y = "Goal Differential"
  ) +
  theme_minimal()

# Pearson
cor.test(Touch_GoalDiff_Analysis$ScaledAboveAvg, Touch_GoalDiff_Analysis$GoalDiff)

############################ Wilcoxon: Inter-Match Variability Hypothesis ############################ 

# Wilcoxon: Perhaps not nicely distributed: “Do teams tend to have higher goal differentials when they are more touchy than their season average?”

# Create AboveAvgTouch flag
Touch_AboveBelow_Analysis <- Touch_GoalDiff_Analysis %>%
  mutate(AboveAvgTouch = ScaledAboveAvg > 0)

# Summary of goal differentials by touch group
Touch_AboveBelow_Analysis %>%
  group_by(AboveAvgTouch) %>%
  summarise(
    n = n(),
    mean_GD = mean(GoalDiff, na.rm = TRUE),
    median_GD = median(GoalDiff, na.rm = TRUE),
    sd_GD = sd(GoalDiff, na.rm = TRUE)
  )

# Wilcoxon rank-sum test
wilcox.test(GoalDiff ~ AboveAvgTouch, data = Touch_AboveBelow_Analysis)

# Visualize Wilcoxon
ggplot(Touch_AboveBelow_Analysis, aes(x = AboveAvgTouch, y = GoalDiff)) +
  geom_boxplot(fill = "lightblue") +
  scale_x_discrete(labels = c("FALSE" = "Below Avg Touch", "TRUE" = "Above Avg Touch")) +
  labs(
    title = "Goal Differential by Above/Below Avg Touch",
    x = "Above Team's Avg Touch?",
    y = "Goal Differential"
  ) +
  theme_minimal()
