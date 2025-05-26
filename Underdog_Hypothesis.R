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
library(plotly)
library(mgcv)


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

#NOTE: First 13 matches of season are excluded. Not all teams play the first weekend of the season, 
#therefore, the current standings only have a 'comprehensive picture' going into the 3rd weekend
#In other words, after match 13, at least every team has completed 1 game

# Clean Match column data for use
Matches_final_cleaned_CurrentStandings <- Matches_final %>%
  mutate(
    GoalsFor = as.numeric(str_trim(GoalsFor)),
    GoalsAgainst = as.numeric(str_trim(GoalsAgainst)),
    CurrentStanding = as.numeric(str_trim(CurrentStanding)),
    MatchID = str_pad(MatchID, width = 4, pad = "0"),  # ensure 4-digit MatchID
    TeamID = str_sub(MatchID, 1, 2),                   # extract TeamID from MatchID
    SeasonMatchNumber = as.numeric(SeasonMatchNumber)  # ensure it can be compared numerically
  ) %>%
  filter(SeasonMatchNumber > 13)  # exclude first 13 matches

# Get the spread into the info for each team
Matches_final_Spread <- Matches_final_cleaned_CurrentStandings %>%
  rename_with(~ paste0(.x, "_self")) %>% #renames every column so that you know which row refers to the self team of analysis
  inner_join(
    Matches_final_cleaned_CurrentStandings,
    by = c("SeasonMatchNumber_self" = "SeasonMatchNumber") # joins data frame to itself, matching each game via seasonmatchnumber (_self is of interst) (wihtout is opponent)
  ) %>%
  filter(TeamID_self != TeamID) %>%  # Make sure we’re not joining a row to itself
  mutate(
    GoalDiff = GoalsFor_self - GoalsAgainst_self,
    Spread = CurrentStanding - CurrentStanding_self  # positive = better ranked than opponent, negative = underdog
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

#Correcting data types
Touches_scaled_numeric <- Touches_scaled %>%
  mutate(
    SeasonMatchNumber = as.numeric(SeasonMatchNumber),
    Team = as.character(Team)  # just to ensure consistency
  )

# Data frame creation for Underdog analysis
Underdog_Analysis <- Matches_final_Spread %>%
  left_join(
    Touches_scaled_numeric,
    by = c("SeasonMatchNumber", "TeamID" = "Team")
  )

############################ GAM Model | Underdog Hypothesis ############################

# Fit GAM model to allow for nonlinear effects
# Changing k = 15 creates completely flate slope
gam_model <- gam(
  GoalDiff ~ s(Spread, ScaledTouch, k = 16, bs = "tp"),  # increase k for smoother fit
  data = Underdog_Analysis
)

# Create grid for predictions
spread_seq <- seq(min(Underdog_Analysis$Spread, na.rm = TRUE),
                  max(Underdog_Analysis$Spread, na.rm = TRUE), length.out = 50)
touch_seq <- seq(min(Underdog_Analysis$ScaledTouch, na.rm = TRUE),
                 max(Underdog_Analysis$ScaledTouch, na.rm = TRUE), length.out = 50)

grid <- expand.grid(Spread = spread_seq, ScaledTouch = touch_seq)
grid$GoalDiff <- predict(gam_model, newdata = grid)

# Convert to matrix for surface
z_matrix <- matrix(grid$GoalDiff, nrow = length(spread_seq), ncol = length(touch_seq))

# 3D Plot
plot_ly() %>%
  add_surface(
    x = ~spread_seq,
    y = ~touch_seq,
    z = ~z_matrix,
    colorscale = list(
      c(0, "red"),  # red for losses
      c(1, "green")   # green for wins
    ),
    cmin = min(Underdog_Analysis$GoalDiff, na.rm = TRUE),
    cmax = max(Underdog_Analysis$GoalDiff, na.rm = TRUE),
    opacity = 0.7,
    showscale = TRUE
  ) %>%
  add_markers(
    data = Underdog_Analysis,
    x = ~Spread,
    y = ~ScaledTouch,
    z = ~GoalDiff,
    marker = list(
      size = 3,
      color = ~GoalDiff,
      colorscale = list(c(0, "#ff0000"), c(1, "#00ff00")),  # flipped: red = low, green = high
      cmin = min(Underdog_Analysis$GoalDiff, na.rm = TRUE),
      cmax = max(Underdog_Analysis$GoalDiff, na.rm = TRUE)
    ),
    name = "Observed"
  ) %>%
  layout(
    title = "Underdog Hypothesis: Spread x Touch Deviation x Goal Differential",
    scene = list(
      xaxis = list(title = "Spread (Opponent Rank - Team Rank)"),
      yaxis = list(title = "Scaled Touch Deviation"),
      zaxis = list(title = "Goal Differential")
    )
  )


############################ Interpret GAM Model ############################



# Underdog_Analysis %>%
#   mutate(
#     UnderdogGroup = case_when(
#       Spread < -5 ~ "Major Underdog",
#       Spread < 0 ~ "Mild Underdog",
#       Spread == 0 ~ "Equal Rank",
#       Spread > 0 ~ "Favored"
#     ),
#     HighTouch = ScaledTouch > 0
#   ) %>%
#   group_by(UnderdogGroup, HighTouch) %>%
#   summarise(
#     MeanGD = mean(GoalDiff, na.rm = TRUE),
#     n = n()
#   )

