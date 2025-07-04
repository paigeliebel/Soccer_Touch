# Inter-Match Variability Hypothesis
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library(dplyr)
library(ggh4x)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Overview_summary_Data.R") #Runs and brings in data frames from Core_Hypothesis.R script

############################ Inter-Match Variability Hypothesis ############################ 

# Asking: "When a team is touchy, do they score more or less goals than their opponent?"
# Note that this is still using same CoreData touches (therefore only prosocial touches and not including GF, GA, Subs etc)

# Look at overall touch frequency (no scaled) first, then do individual team scaling

# Joins dataframes (goal differentials to the touches unscaled)
Touch_Unscaled_GoalDiff_Analysis <- Touches_per_match_team %>%
  left_join(
    Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
    by = c("SeasonMatchNumber", "Team" = "TeamID")
  )

# Visualize 
Touch_Unscaled_GoalDiff_Plot <- ggplot(Touch_Unscaled_GoalDiff_Analysis, aes(x = TouchCount, y = GoalDiff)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Touch Count vs Match Goal Differential",
    x = "Touch Frequency per team per match",
    y = "Goal Differential",
    caption = "Note: Each dot represents one match outcome for a team, therefore 2 dots for each match"
  ) +
  theme_minimal()

# Note that this data is not actually continuous. a team can't win by 2.1 or 4.3 goals.

Touch_Unscaled_GoalDiff_cor_test <- cor.test(Touch_Unscaled_GoalDiff_Analysis$TouchCount,
                     Touch_Unscaled_GoalDiff_Analysis$GoalDiff,
                     method = "spearman",
                     exact = FALSE)  # exact=FALSE for larger datasets

# This alone is statistically significant. More touch associated with more goals.

######## Scaled version per team ####

# Joins dataframes (goal differentials to the touches scaled)
Touch_GoalDiff_scaled_Analysis <- Touches_per_match_scaled %>%
  left_join(
    Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
    by = c("SeasonMatchNumber", "Team" = "TeamID")
  )

# Visualize 
Touch_GoalDiff_scaled_plot <- ggplot(Touch_GoalDiff_scaled_Analysis, aes(x = ScaledTouch, y = GoalDiff)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Touch Count Deviation vs Match Goal Differential",
    x = "Scaled Touch by Team Profile",
    y = "Goal Differential",
    caption = "Note: Each dot represents one match outcome for a team, therefore 2 dots for each match"
  ) +
  theme_minimal()

# Spearman:
Touch_Scaled_GoalDiff_cor_test <- cor.test(
  Touch_GoalDiff_scaled_Analysis$ScaledTouch,
  Touch_GoalDiff_scaled_Analysis$GoalDiff,
  method = "spearman",
  exact = FALSE  # use FALSE for larger datasets
)

# Both Scaled and Unscaled are quite similar! Both are statistically significant.

Touch_GoalDiff_scaled_Analysis_plot <- ggplot(Touch_GoalDiff_scaled_Analysis, aes(x = ScaledTouch, y = GoalDiff)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) +
  ggh4x::facet_wrap2(~ Team, ncol = 7, strip.position = "top", axes = "all") +
  coord_cartesian(xlim = c(-4, 4)) +
  labs(
    title = "Touch Count Deviation vs Match Goal Differential by Team",
    x = "Scaled Touch by Team Profile",
    y = "Goal Differential",
    caption = "Each dot represents one match outcome for a team"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "gray30", size = 0.8),
    panel.spacing = unit(1, "lines")
  )
