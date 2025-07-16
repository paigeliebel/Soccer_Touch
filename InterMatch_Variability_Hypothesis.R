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
library(gtsummary)


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


##################################################
######## Half by Half Analysis ########
##################################################

#Separate touches by half

Touches_CoreData_Half <- Touches_CoreData %>%
  mutate(Half = case_when(
    str_starts(as.character(Time), "1") ~ "First Half",
    str_starts(as.character(Time), "2") ~ "Second Half",
    TRUE ~ "Unknown"
  ))

# two new data sets
Touches_FirstHalf <- Touches_CoreData_Half %>%
  filter(Half == "First Half") %>%
  group_by(Team, SeasonMatchNumber) %>%
  summarise(TouchCount_FirstHalf = n(), .groups = "drop")

Touches_SecondHalf <- Touches_CoreData_Half %>%
  filter(Half == "Second Half") %>%
  group_by(Team, SeasonMatchNumber) %>%
  summarise(TouchCount_SecondHalf = n(), .groups = "drop")

# First Half Plot
Touch_FirstHalf_GoalDiff_Analysis <- Touches_FirstHalf %>%
  left_join(
    Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
    by = c("SeasonMatchNumber", "Team" = "TeamID")
  )

Touch_FirstHalf_GoalDiff_Plot <- ggplot(Touch_FirstHalf_GoalDiff_Analysis, aes(x = TouchCount_FirstHalf, y = GoalDiff)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "First Half Touch Count vs Match Goal Differential",
    x = "First Half Touches per Team per Match",
    y = "Goal Differential"
  ) +
  theme_minimal()

# Second Half Plot
Touch_SecondHalf_GoalDiff_Analysis <- Touches_SecondHalf %>%
  left_join(
    Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
    by = c("SeasonMatchNumber", "Team" = "TeamID")
  )

Touch_SecondHalf_GoalDiff_Plot <- ggplot(Touch_SecondHalf_GoalDiff_Analysis, aes(x = TouchCount_SecondHalf, y = GoalDiff)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Second Half Touch Count vs Match Goal Differential",
    x = "Second Half Touches per Team per Match",
    y = "Goal Differential"
  ) +
  theme_minimal()

# Overlayed

#Add "Half" label and unify column for plotting
Touch_FirstHalf_GoalDiff_Analysis <- Touch_FirstHalf_GoalDiff_Analysis %>%
  mutate(Half = "First", TouchCount = TouchCount_FirstHalf) %>%
  select(SeasonMatchNumber, Team, GoalDiff, Half, TouchCount)

Touch_SecondHalf_GoalDiff_Analysis <- Touch_SecondHalf_GoalDiff_Analysis %>%
  mutate(Half = "Second", TouchCount = TouchCount_SecondHalf) %>%
  select(SeasonMatchNumber, Team, GoalDiff, Half, TouchCount)

#Combine into long format
Touch_HalfComparison_Analysis <- bind_rows(
  Touch_FirstHalf_GoalDiff_Analysis,
  Touch_SecondHalf_GoalDiff_Analysis
)


Touch_HalfComparison_Plot <- ggplot(Touch_HalfComparison_Analysis, aes(x = TouchCount, y = GoalDiff, color = Half)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_manual(
    values = c("First" = "steelblue", "Second" = "firebrick"),
    labels = c("First Half", "Second Half")
  ) +
  labs(
    title = "Touch Count vs Goal Differential by Match Half",
    x = "Touches per Team per Half",
    y = "Goal Differential",
    color = "Match Half",
    caption = "Dots represent match outcomes for each team. Trend lines show linear fits for First and Second Halves."
  ) +
  theme_minimal()

#Half Analysis
Touch_HalfComparison_Analysis <- Touch_HalfComparison_Analysis %>%
  mutate(Half = factor(Half, levels = c("First", "Second")))

model_interaction <- lm(GoalDiff ~ TouchCount * Half, data = Touch_HalfComparison_Analysis)
summary(model_interaction)

model_interaction %>% tbl_regression()

