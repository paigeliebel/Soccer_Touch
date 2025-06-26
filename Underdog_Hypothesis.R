# Underdog Hypothesis
# For more information on these data frames please look at the README.md file

################################################################################
# MODELING STRATEGY FOR UNDERDOG HYPOTHESIS | GAMs, Penalization, and Surfaces #
################################################################################

# OBJECTIVE:
# To test whether the impact of prosocial touch on Goal Differential depends 
# on a team's underdog status (Spread = Opponent Rank - Team Rank).
# 
# Specifically, we examined whether there is a *synergistic interaction* between 
# touch frequency and underdog status, rather than just additive effects.

# MODELING APPROACH:
# We used Generalized Additive Models (GAMs) with tensor product smooths 
# to allow for non-linear interactions between:
#     (1) Spread (ranking difference) and 
#     (2) ScaledTouch (how touchy the team was compared to their average)

# -------------------------
# MODEL 1: Penalized GAM
# -------------------------
# - Fit using `method = "REML"` to estimate the optimal amount of wiggliness.
# - Penalization discourages overfitting and results in smoother surfaces.
# - Formula: GoalDiff ~ s(Spread, ScaledTouch, k = 100, bs = "tp")
# - Interpretation: Model explained ~23% of deviance with ~2 EDF.
# - Result: Smooth surface resembled a near-plane, suggesting additive (not interactive) effects.

# -------------------------
# MODEL 2: Unpenalized GAM
# -------------------------
# - Fit with `fx = TRUE`, which **removes penalization** and forces full wiggliness.
# - Formula: GoalDiff ~ s(Spread, ScaledTouch, k = 40, bs = "tp", fx = TRUE)
# - Interpretation: Higher edf (~39), more complex surface, deviance explained slightly higher (~29.7%)
# - However, AIC increased → indicating overfitting likely, and added complexity not justified.
# - Useful as a diagnostic contrast to visualize flexibility.

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
library(ggplot2)
library(forcats)


source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Core_Hypothesis.R") #Runs and brings in data frames from Core_Hypothesis.R script
source("InterMatch_Variability_Hypothesis.R") #Runs and brings in data frames from InterMatch_Variability_Hypothesis.R script

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

############################ Observed Data Table Summary | Underdog Hypothesis ############################

spread_cutoff <- 7 #arbitrary spread number: I like 7 because it separates the table in half (1st rank team playing against bottom half of table)

# Categorize real match data into underdog/favored + touch level
Underdog_Observed_Summary <- Underdog_Analysis %>%
  filter(!is.na(GoalDiff) & !is.na(ScaledTouch) & !is.na(Spread)) %>%  # ensure clean data
  mutate(
    SpreadGroup = case_when(
      Spread <= -spread_cutoff ~ "Major Underdog",
      Spread > -spread_cutoff & Spread < 0 ~ "Mild Underdog",
      Spread == 0 ~ "Even",
      Spread > 0 & Spread < spread_cutoff ~ "Mild Favorite",
      Spread >= spread_cutoff ~ "Major Favorite"
    ),
    TouchGroup = case_when(
      ScaledTouch >= 1 ~ "High Touch",
      ScaledTouch <= -1 ~ "Low Touch",
      TRUE ~ "Average Touch"
    )
  ) %>%
  group_by(SpreadGroup, TouchGroup) %>%
  summarise(
    MeanObservedGD = mean(GoalDiff, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(SpreadGroup, TouchGroup)

#Bar chart for this data: I think easier to understand than the cool looking 3D chart generated below.
# Set factor levels for order
spread_levels <- c("Major Underdog", "Mild Underdog", "Even", "Mild Favorite", "Major Favorite")
touch_levels <- c("Low Touch", "Average Touch", "High Touch")

# Make sure SpreadGroup and TouchGroup are ordered
Underdog_Observed_Summary <- Underdog_Observed_Summary %>%
  mutate(
    SpreadGroup = factor(SpreadGroup, levels = spread_levels),
    TouchGroup = factor(TouchGroup, levels = touch_levels)
  )

# Plot observed data
ggplot(Underdog_Observed_Summary, aes(x = SpreadGroup, y = MeanObservedGD, fill = TouchGroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0("n=", n)),
    position = position_dodge(width = 0.8),
    vjust = ifelse(Underdog_Observed_Summary$MeanObservedGD >= 0, -0.5, 1.2),
    size = 3.5
  ) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Observed Goal Differential by Underdog/Favorite Status and Touch Level",
    x = "Underdog/Favorite Status (Spread Group)",
    y = "Mean Goal Differential",
    fill = "Touch Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Statistically compare goal differentials across spreadgroup and touchgroup
# Via a two-way ANOVa 
# Make sure grouping variables are factors
Underdog_Observed_ANOVA <- Underdog_Analysis %>%
  filter(!is.na(GoalDiff) & !is.na(ScaledTouch) & !is.na(Spread)) %>%
  mutate(
    SpreadGroup = case_when(
      Spread <= -spread_cutoff ~ "Major Underdog",
      Spread > -spread_cutoff & Spread < 0 ~ "Mild Underdog",
      Spread == 0 ~ "Even",
      Spread > 0 & Spread < spread_cutoff ~ "Mild Favorite",
      Spread >= spread_cutoff ~ "Major Favorite"
    ),
    TouchGroup = case_when(
      ScaledTouch >= 1 ~ "High Touch",
      ScaledTouch <= -1 ~ "Low Touch",
      TRUE ~ "Average Touch"
    ),
    SpreadGroup = factor(SpreadGroup, levels = spread_levels),
    TouchGroup = factor(TouchGroup, levels = touch_levels)
  )

# Run Two-Way ANOVA
anova_result <- aov(GoalDiff ~ SpreadGroup * TouchGroup, data = Underdog_Observed_ANOVA)
summary(anova_result)
TukeyHSD(anova_result)

#Summary Table of Means and SDs per Group
Underdog_Observed_ANOVA %>%
  group_by(SpreadGroup, TouchGroup) %>%
  summarise(
    Mean_GD = mean(GoalDiff, na.rm = TRUE),
    SD_GD = sd(GoalDiff, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(SpreadGroup, TouchGroup)

#ANOVA categorically dos not say that spread AND touch together affects goal differential
#It does say that underdogs are more likely to lose (duh) and that more touch is better (duh)
#Look at GAM Model to see other stuff, there it creates a a relationship between the three values

# 3D scatter of actual points only
plot_ly(data = Underdog_Analysis,
        x = ~Spread, y = ~ScaledTouch, z = ~GoalDiff,
        type = "scatter3d", mode = "markers",
        marker = list(size = 3, color = ~GoalDiff,
                      colorscale = "RdYlGn", showscale = TRUE))

############################ GAM Model | Underdog Hypothesis ############################

#This is asking: If a team is more/less touchy than usual, and they are underdog/overdog, how does this impact the goal differenetial?

# Fit GAM model to allow for nonlinear effects

# Changing k = 15 creates completely flatens slope
gam_model <- gam(
  GoalDiff ~ s(Spread, ScaledTouch, k = 100, bs = "tp"),  # increase k for smoother fit
  method = "REML", #changing penalty 
  data = Underdog_Analysis
)

#Forced Wigliness - removed penalities
gam_model_unpenalized <- gam(
  GoalDiff ~ s(Spread, ScaledTouch, k = 100, bs = "tp", fx = TRUE),
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
#Note: Each dot represents one match outcome for a team, therefore 2 dots for each match
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


## completely not penalized one (allow for overfitting)
#Forced Wigliness - removed penalities
gam_model_unpenalized <- gam(
  GoalDiff ~ s(Spread, ScaledTouch, k = 40, bs = "tp", fx = TRUE),
  data = Underdog_Analysis
)

# Step 1: Use same grid
grid$GoalDiff_unpenalized <- predict(gam_model_unpenalized, newdata = grid)

# Step 2: Convert to matrix for plotly
z_matrix_unpenalized <- matrix(grid$GoalDiff_unpenalized, 
                               nrow = length(spread_seq), 
                               ncol = length(touch_seq))

plot_ly() %>%
  add_surface(
    x = ~spread_seq,
    y = ~touch_seq,
    z = ~z_matrix_unpenalized,
    colorscale = list(
      c(0, "red"),
      c(1, "green")
    ),
    cmin = min(Underdog_Analysis$GoalDiff, na.rm = TRUE),
    cmax = max(Underdog_Analysis$GoalDiff, na.rm = TRUE),
    opacity = 0.7,
    showscale = TRUE,
    name = "Unpenalized Surface"
  ) %>%
  add_markers(
    data = Underdog_Analysis,
    x = ~Spread,
    y = ~ScaledTouch,
    z = ~GoalDiff,
    marker = list(
      size = 3,
      color = ~GoalDiff,
      colorscale = list(c(0, "#ff0000"), c(1, "#00ff00")),
      cmin = min(Underdog_Analysis$GoalDiff, na.rm = TRUE),
      cmax = max(Underdog_Analysis$GoalDiff, na.rm = TRUE)
    ),
    name = "Observed"
  ) %>%
  layout(
    title = "Unpenalized GAM: Spread x Touch Deviation x Goal Differential",
    scene = list(
      xaxis = list(title = "Spread"),
      yaxis = list(title = "Scaled Touch"),
      zaxis = list(title = "Goal Differential")
    )
  )

############################ GAM Model CATEGORICAL Table Summary (Interpretation of GAM) | Underdog Hypothesis ############################

# Evaluates how a team's goal differential is predicated by a model across a spectrum of two predictors:
# Predictor One = Ranking Spread
# Predictor Two = Scaled Touch Deviation (how much more or less physical touch a team used compared to their norm)

# Backpedals to the observed table CATEGORICAL idea. 

# Define a grid of Spread and ScaledTouch values
# Create a sequence of 100 evenly spaced values from smallest to largest spread
spread_vals <- seq(min(Underdog_Analysis$Spread, na.rm = TRUE),
                   max(Underdog_Analysis$Spread, na.rm = TRUE),
                   length.out = 100)

# Create a sequence of 100 evenly spaced values from smallest to largest touch
touch_vals <- seq(min(Underdog_Analysis$ScaledTouch, na.rm = TRUE),
                  max(Underdog_Analysis$ScaledTouch, na.rm = TRUE),
                  length.out = 100)

# Create a ten thousand row data frame by combining all hundred by hundred values above
grid <- expand.grid(Spread = spread_vals, ScaledTouch = touch_vals)

# Predict GoalDiff across the grid on each fake game (ten thousand of them)
grid$PredictedGoalDiff <- predict(gam_model, newdata = grid)

#Note choice of 7 as the cutoff is somewhat arbitrary
grid_summary <- grid %>%
  mutate(
    SpreadGroup = case_when(
      Spread <= -spread_cutoff ~ "Major Underdog",
      Spread > -spread_cutoff & Spread < 0 ~ "Mild Underdog",
      Spread == 0 ~ "Even",
      Spread > 0 & Spread < spread_cutoff ~ "Mild Favorite",
      Spread >= spread_cutoff ~ "Major Favorite"
    ),
    TouchGroup = case_when(
      ScaledTouch >= 1 ~ "High Touch",
      ScaledTouch <= -1 ~ "Low Touch",
      TRUE ~ "Average Touch"
    )
  ) %>%
  group_by(SpreadGroup, TouchGroup) %>%
  summarise(
    MeanPredGD = mean(PredictedGoalDiff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SpreadGroup, TouchGroup)

#Bar graph to see the GAM model major/mild underdogs versus tables
# Set same factor levels for consistency
grid_summary <- grid_summary %>%
  mutate(
    SpreadGroup = factor(SpreadGroup, levels = spread_levels),
    TouchGroup = factor(TouchGroup, levels = touch_levels)
  )

# Plot GAM model data
ggplot(grid_summary, aes(x = SpreadGroup, y = MeanPredGD, fill = TouchGroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "GAM Model Predicted Goal Differential by Underdog/Favorite Status and Touch Level",
    x = "Underdog/Favorite Status (Spread Group)",
    y = "Predicted Goal Differential",
    fill = "Touch Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

