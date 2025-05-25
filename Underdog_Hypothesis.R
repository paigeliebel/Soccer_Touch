# Underdog Hypothesis
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
source("InterMatch_Variability_Hypothesis.R") #Runs and brings in data frames from InterMatch_Variability_Hypothesis.R script

#note that this is not complete until we get Simon Data (only 2/3s of it so far)
#Ensure to use the correct dfs. Touches_final and Matches_final are correct. They only include assigned rater data, no repeat matches

#Check to make sure data frames are loaded:

if (!exists("Touches_final") | !exists("Touches_scaled") | !exists("Matches_finalID") | !exists("FinalStandings") | !exists("Touches_CoreHyp")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded. Check Data_Management.R and Core_Hypothesis.R.")
}

############################ Underdog Hypothesis ############################

# Clean Match column data for use
Matches_final_cleaned_CurrentStandings <- Matches_final %>%
  mutate(
    GoalsFor = as.numeric(str_trim(GoalsFor)),
    GoalsAgainst = as.numeric(str_trim(GoalsAgainst)),
    CurrentStanding = as.numeric(str_trim(CurrentStanding)),
    MatchID = str_pad(MatchID, width = 4, pad = "0"),  # ensure 4-digit MatchID
    TeamID = str_sub(MatchID, 1, 2)                    # extract TeamID from MatchID
  )

# Get the spread into teh info for each team
Matches_final_Spread <- Matches_final_cleaned_CurrentStandings %>%
  rename_with(~ paste0(.x, "_self")) %>%
  inner_join(
    Matches_final_cleaned_CurrentStandings,
    by = c("SeasonMatchNumber_self" = "SeasonMatchNumber")
  ) %>%
  filter(TeamID_self != TeamID) %>%  # Make sure we’re not joining a row to itself
  mutate(
    GoalDiff = GoalsFor_self - GoalsAgainst_self,
    Spread = CurrentStanding_self - CurrentStanding  # positive = better ranked than opponent, neg = underdog
  ) %>%
  select(
    SeasonMatchNumber = SeasonMatchNumber_self,
    MatchID = MatchID_self,
    TeamID = TeamID_self,
    GoalDiff,
    Spread,
    CurrentStanding = CurrentStanding_self,
    OpponentTeamID = TeamID,
    OpponentStanding = CurrentStanding
  )

# Data frame creation for Underdog analysis
Underdog_Analysis <- Matches_final_Spread %>%
  left_join(
    Touches_scaled,
    by = c("SeasonMatchNumber", "TeamID" = "Team")
  )

# Visualize
ggplot(Underdog_Analysis, aes(x = Spread, y = ScaledTouch)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  labs(
    title = "Scaled Touches per Match vs Ranking Spread",
    x = "Spread in Current Standing (Opponent - Team)",
    y = "Touches in Match"
  ) +
  theme_minimal()


#Now add third dimension of goal differential. See if teams that lose less if they touch more against harder teams. 






