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
Touch_Unscaled_GoalDiff_Plot <- ggplot(Touch_Unscaled_GoalDiff_Analysis, aes(x = TouchCount, y = GoalDiff, color = Rank)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linewidth = 1) +
  scale_color_gradient(
    low = "green3",  # Best-ranked teams
    high = "firebrick3",  # Worst-ranked teams
    name = "Team's Final \n Season Rank",
    guide = guide_colorbar(reverse = TRUE)  # So R1 is green
  ) +
  labs(
    title = "Match-Level: Goal Differential vs Touch Frequency",
    x = "Touches per Match",
    y = "Goal Differential",
    caption = "One dot represents match outcome for one team (2 dots per match). \nColor indicates team’s final season rank (R1 = green, R14 = red)."
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

# Note that this data is not actually continuous. a team can't win by 2.1 or 4.3 goals.

# Normality checks
shapiro.test(Touch_Unscaled_GoalDiff_Analysis$TouchCount)
shapiro.test(Touch_Unscaled_GoalDiff_Analysis$GoalDiff)

Touch_Unscaled_GoalDiff_cor_test <- cor.test(Touch_Unscaled_GoalDiff_Analysis$TouchCount,
                     Touch_Unscaled_GoalDiff_Analysis$GoalDiff,
                     method = "spearman",
                     exact = FALSE)  # exact=FALSE for larger datasets

GoalDiff_lm <- lm(GoalDiff ~ TouchCount, data = Touch_Unscaled_GoalDiff_Analysis)

# This alone is statistically significant. More touch associated with more goals.


######## Scaled version per team ####

# Joins dataframes (goal differentials to the touches scaled)
Touch_GoalDiff_scaled_Analysis <- Touches_per_match_scaled %>%
  left_join(
    Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
    by = c("SeasonMatchNumber", "Team" = "TeamID")
  ) %>%
  left_join(
    FinalStandings %>% select(TeamID, Rank),
    by = c("Team" = "TeamID")
  )


# Visualize 
Touch_GoalDiff_scaled_plot <- ggplot(Touch_GoalDiff_scaled_Analysis, aes(x = ScaledTouch, y = GoalDiff, color = Rank)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linewidth = 1) +
  scale_color_gradient(
    low = "green3", 
    high = "firebrick3", 
    name = "Team's Final \n Season Rank",
    guide = guide_colorbar(reverse = TRUE)
  ) +
  labs(
    title = "Match-Level: Goal Differential vs SCALED Touch Frequency",
    x = "Within-Team Scaled Touch Frequency (Median/IQR Normalized)",
    y = "Goal Differential",
    caption = "One dot represents match outcome for one team (2 dots per match). \nColor indicates team’s final season rank (R1 = green, R14 = red).",
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )


# Spearman:
Touch_Scaled_GoalDiff_cor_test <- cor.test(
  Touch_GoalDiff_scaled_Analysis$ScaledTouch,
  Touch_GoalDiff_scaled_Analysis$GoalDiff,
  method = "spearman",
  exact = FALSE  # use FALSE for larger datasets
)

Touch_Scaled_lm <- lm(GoalDiff ~ ScaledTouch, data = Touch_GoalDiff_scaled_Analysis)

Touch_GoalDiff_scaled_Analysis <- Touch_GoalDiff_scaled_Analysis %>%
  mutate(RankNumber = paste0("R", Rank))

Touch_GoalDiff_scaled_Analysis <- Touch_GoalDiff_scaled_Analysis %>%
  mutate(
    RankNumber = paste0("R", Rank),  # Creates R1, R2, ..., R14
    RankNumber = factor(RankNumber, levels = paste0("R", 1:14))  # Ensures proper order
  )


Touch_GoalDiff_scaled_fourteenteam_plot <- ggplot(Touch_GoalDiff_scaled_Analysis, aes(x = ScaledTouch, y = GoalDiff)) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 0.8) +
  ggh4x::facet_wrap2(~ RankNumber, ncol = 7, strip.position = "top", axes = "all") +
  coord_cartesian(xlim = c(-2, 4)) +
  labs(
    title = "Match Goal Differential by Team vs Touch Count Deviation",
    x = "Within-Team Scaled Touch Frequency (Median/IQR Normalized)",
    y = "Goal Differential",
    caption = "Each dot represents one match outcome for a team (26 matches per team)"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black"),
    panel.border = element_blank(),
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

######## Scaled Half Analysis #######

Touches_per_firsthalf_Scaled <- Touch_FirstHalf_GoalDiff_Analysis %>%
  group_by(Team) %>%
  mutate(
    median_touch = median(TouchCount, na.rm = TRUE),
    iqr_touch = IQR(TouchCount, na.rm = TRUE),
    ScaledTouch = (TouchCount - median_touch) / iqr_touch
  ) %>%
  ungroup()

Touches_per_secondhalf_Scaled <- Touch_SecondHalf_GoalDiff_Analysis %>%
  group_by(Team) %>%
  mutate(
    median_touch = median(TouchCount, na.rm = TRUE),
    iqr_touch = IQR(TouchCount, na.rm = TRUE),
    ScaledTouch = (TouchCount - median_touch) / iqr_touch
  ) %>%
  ungroup()

Touches_per_firsthalf_Scaled <- Touches_per_firsthalf_Scaled %>%
  mutate(Half = "First")

Touches_per_secondhalf_Scaled <- Touches_per_secondhalf_Scaled %>%
  mutate(Half = "Second")

Touch_HalfComparison_Scaled <- bind_rows(
  Touches_per_firsthalf_Scaled,
  Touches_per_secondhalf_Scaled
) %>%
  mutate(Half = factor(Half, levels = c("First", "Second")))

Touch_HalfComparison_ScaledPlot <- ggplot(Touch_HalfComparison_Scaled, aes(x = ScaledTouch, y = GoalDiff, color = Half)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_manual(
    values = c("First" = "steelblue", "Second" = "firebrick"),
    labels = c("First Half", "Second Half")
  ) +
  labs(
    title = "Scaled Touch Count vs Goal Differential by Match Half",
    x = "Scaled Touch Count (per team IQR)",
    y = "Goal Differential",
    color = "Match Half"
  ) +
  theme_minimal()

model_scaled_interaction <- lm(GoalDiff ~ ScaledTouch * Half, data = Touch_HalfComparison_Scaled)
summary(model_scaled_interaction)

