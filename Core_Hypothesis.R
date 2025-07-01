# Core_Hypothesis
# Added Outlier Analysis as well
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

#Ensure to use the correct dfs. Touches_final and Matches_final are correct. They only include assigned rater data, no repeat matches

#Check to make sure data frames are loaded:
if (!exists("Touches_final") | !exists("Matches_final") | !exists("FinalStandings")) {
  stop("Touches_final, Matches_final, or FinalStandings not loaded. Check Data_Management.R.")
}

############################ Create Data Set for this Hypothesis ############################ 

# Prosocial touches are defined as all haptic rituals recorded excluding: 
#   Tactical Adjustments, Collisions, and Negative Touch
# 
# Situations excluded from analysis for this hypothesis: Goals For/Against, Substitutions

Exclude_Touch <- c("TA", "CO", "NEG")
Exclude_Situation <- c("GF", "GA", "SUB") 

#Creates data set for core hypothesis analysis
Touches_CoreHyp <- Touches_final %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation))

#Count of frequency of touches per team
Touches_by_team <- Touches_CoreHyp %>%
  mutate(Team = str_trim(as.character(Team))) %>%
  count(Team, name = "TotalTouches")

# Make sure TeamID is padded to match
FinalStandings <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0"))

#Join touch counts with final standings
Team_Touches_Standings <- FinalStandings %>%
  left_join(Touches_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(TotalTouches))

#Plot with regression line : final rankings to frequency of touch
TouchFreq_vs_FinalStandings <- ggplot(Team_Touches_Standings, aes(x = Rank, y = TotalTouches)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  scale_x_reverse() +
  labs(
    title = "Final Rank vs Overall Touch Frequency",
    x = "Final Season Rank",
    y = "Total Touches (Filtered)"
  ) +
  theme_minimal()

TouchFreq_vs_FinalStandings_Stats <- cor_result <- cor.test(Team_Touches_Standings$TotalTouches, Team_Touches_Standings$Rank)

#Bar graph of Total Touches per Team over the season
Touches_per_Team <- ggplot(Team_Touches_Standings, aes(x = reorder(TeamID, -TotalTouches), y = TotalTouches)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Total Touches per Team (Season)",
    x = "Team",
    y = "Total Touches"
  ) +
  theme_minimal()

############################ Investigating outliers ############################ 

# Check distribution and identify potential outlier
Team_Touches_Standings %>%
  arrange(desc(TotalTouches))

library(ggrepel)

outliers <- ggplot(Team_Touches_Standings, aes(x = Rank, y = TotalTouches, label = TeamID)) +
  geom_point(size = 3) +
  geom_text_repel() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_reverse() +
  labs(
    title = "Final Rank vs Touch Frequency (w/ Labels)",
    x = "Final Season Rank",
    y = "Total Touches"
  ) +
  theme_minimal()

Touches_per_game <- Touches_CoreHyp %>%
  group_by(Team, SeasonMatchNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Prep: Join team rank for ordering
Touches_per_game_ranked <- Touches_per_game %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  filter(!is.na(Rank))

ggplot(Touches_per_game, aes(x = TouchCount)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Touches per Game (All Teams)",
    x = "Touches per Game",
    y = "Number of Matches"
  ) +
  theme_minimal()

# Replace "xx" with the team you're investigating
team_focus <- "13"

Touches_per_game %>%
  mutate(IsTarget = ifelse(Team == team_focus, "Target Team", "Others")) %>%
  ggplot(aes(x = TouchCount, fill = IsTarget)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7, color = "white") +
  scale_fill_manual(values = c("Target Team" = "red", "Others" = "gray")) +
  labs(
    title = paste("Touches per Game Distribution — Highlighting Team", team_focus),
    x = "Touches per Game",
    y = "Number of Matches",
    fill = "Team"
  ) +
  theme_minimal()

#Histogram per team
ggplot(Touches_per_game, aes(x = TouchCount)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  facet_wrap(~ Team, ncol = 4) +
  labs(
    title = "Touches per Game Distribution by Team",
    x = "Touches per Game",
    y = "Number of Matches"
  ) +
  theme_minimal()

#Density plot by team
ggplot(Touches_per_game, aes(x = TouchCount, color = Team)) +
  geom_density(size = 1, alpha = 0.7) +
  labs(
    title = "Touches per Game: Density by Team",
    x = "Touches per Game",
    y = "Density",
    color = "Team"
  ) +
  theme_minimal()

#RidgePlot
# Requires ggridges
library(ggridges)

ggplot(Touches_per_game, aes(x = TouchCount, y = reorder(Team, TouchCount, median), fill = Team)) +
  geom_density_ridges(scale = 2, alpha = 0.8, color = "white") +
  labs(
    title = "Touches per Game Distribution by Team",
    x = "Touches per Game",
    y = "Team (Sorted by Median Touches)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
############################ Within-Team Variability in Touch Frequency ############################ 

# Looks at the variability a team has across matches throughout the season

# Count touches per team per game from CoreHyp data frame
Touches_per_game <- Touches_CoreHyp %>%
  group_by(Team, SeasonMatchNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Computing Within-Team Variability
Team_touch_variability <- Touches_per_game %>%
  group_by(Team) %>%
  summarise(
    MeanTouches = mean(TouchCount),
    SDTouches = sd(TouchCount),
    MinTouches = min(TouchCount),
    MaxTouches = max(TouchCount),
    NumGames = n(),
    .groups = "drop"
  )

# Join season rank to each team for ordering in the plot
Touches_per_game_ranked <- Touches_per_game %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  filter(!is.na(Rank))  # make sure we only include ranked teams

# Visualize 
TouchesPerGame_vs_rank <- ggplot(Touches_per_game_ranked, aes(x = Rank, y = TouchCount, group = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_reverse(breaks = 1:14) +  # clean 1–14 axis
  labs(
    title = "Variation of Within-Team Touch Frequency per Game",
    x = "Team (Ordered by Final Rank)",
    y = "Touches per Game"
  ) +
  theme_minimal()

# Scale touch based on distance from mean (MAD-based z-score)
# How extreme a touch count is compared to team's norm
Touches_scaled <- Touches_per_game %>%
  group_by(Team) %>%
  mutate(
    MedianTouch = median(TouchCount),
    MAD = mad(TouchCount),  # median absolute deviation
    ScaledTouch = (TouchCount - MedianTouch) / MAD
  ) %>%
  ungroup()

# Join Touches_scaled with ranks
Touches_scaled_ranked <- Touches_scaled %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  filter(!is.na(Rank))

# Plot of MAD
MAD_TouchesPerGame_vs_rank <- ggplot(Touches_scaled_ranked, aes(x = Rank, y = ScaledTouch, group = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  scale_x_reverse(breaks = 1:14) +  # clean 1–14 axis
  labs(
    title = "Scaled Touch Deviation from Team Median",
    subtitle = "Boxplot of (TouchCount - Median) / MAD per Team",
    x = "Team (Ordered by Final Rank)",
    y = "Scaled Touch Value (MAD Units)"
  ) +
  theme_minimal()

############################ Within-Team Variability in Touch Frequency vs Ranking ############################ 

# Join variability data to final standings
Variability_vs_Rank <- Team_touch_variability %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  filter(!is.na(Rank))

# Plot SDTouches vs Rank
Within_Variability_vs_Rank <- ggplot(Variability_vs_Rank, aes(x = Rank, y = SDTouches)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  scale_x_reverse(breaks = 1:14) +
  labs(
    title = "Team Variability in Touch vs Final Rank",
    x = "Final Season Rank",
    y = "Touch Frequency Variability (SD)"
  ) +
  theme_minimal()

Within_Variability_vs_Rank_Stats <- cor.test(Variability_vs_Rank$SDTouches, Variability_vs_Rank$Rank)

#In regards to within-tea variability including MAD normalization

Team_scaled_variability <- Touches_scaled %>%
  group_by(Team) %>%
  summarise(
    SD_ScaledTouch = sd(ScaledTouch),
    NumGames = n(),
    .groups = "drop"
  )

Team_scaled_variability_ranked <- Team_scaled_variability %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  filter(!is.na(Rank))

Team_scaled_variability_ranked_plot <- ggplot(Team_scaled_variability_ranked, aes(x = Rank, y = SD_ScaledTouch)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  scale_x_reverse(breaks = 1:14) +
  labs(
    title = "Team Variability (SD of Scaled Touch) vs Final Rank",
    x = "Final Season Rank",
    y = "SD of Scaled Touch (MAD units)"
  ) +
  theme_minimal()

ScaledTouch_Variability_vs_Rank_Stats <- cor.test(
  Team_scaled_variability_ranked$SD_ScaledTouch,
  Team_scaled_variability_ranked$Rank
)

############################ Investigating data after removing outliers ############################ 

Touches_filteredoutliers <- Touches_scaled %>%
  filter(abs(ScaledTouch) <= 2)

Touches_outliers_removed <- Touches_scaled %>%
  filter(abs(ScaledTouch) > 2)

#Yes, the filtering included both high and low outliers — anything more than 2 MAD units away from the team median, in either direction (too high or too low), was removed.
library(patchwork)

# Full data
p1 <- ggplot(Touches_scaled, aes(x = TouchCount)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Touches per Game (All Games)",
    x = "Touches per Game",
    y = "Number of Matches"
  ) +
  theme_minimal()

# Filtered data
p2 <- ggplot(Touches_filteredoutliers, aes(x = TouchCount)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "white") +
  labs(
    title = "Touches per Game (Outliers Removed)",
    x = "Touches per Game",
    y = "Number of Matches"
  ) +
  theme_minimal()

# Display side-by-side
p1 + p2

############################ Removal of Outliers | Core Hypothesis ############################ 

Touches_filteredoutliers  # contains Team, SeasonMatchNumber, TouchCount

# Get only the core hypothesis touches from the non-outlier matches
Touches_CoreHyp_Clean <- Touches_CoreHyp %>%
  semi_join(Touches_filteredoutliers, by = c("Team", "SeasonMatchNumber"))

Touches_by_team_clean <- Touches_CoreHyp_Clean %>%
  mutate(Team = str_trim(as.character(Team))) %>%
  count(Team, name = "TotalTouches")

Team_Touches_Standings_clean <- FinalStandings %>%
  left_join(Touches_by_team_clean, by = c("TeamID" = "Team")) %>%
  filter(!is.na(TotalTouches))

TouchFreq_vs_FinalStandings_clean <- ggplot(Team_Touches_Standings_clean, aes(x = Rank, y = TotalTouches)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", linewidth = 1) +
  scale_x_reverse() +
  labs(
    title = "Final Rank vs Touch Frequency (Cleaned, Outliers Removed)",
    x = "Final Season Rank",
    y = "Total Touches (No Outlier Matches)"
  ) +
  theme_minimal()

TouchFreq_vs_FinalStandings_clean_Stats <- cor.test(
  Team_Touches_Standings_clean$TotalTouches,
  Team_Touches_Standings_clean$Rank
)

original_plot <- ggplot(Team_Touches_Standings, aes(x = Rank, y = TotalTouches)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 1) +
  scale_x_reverse() +
  labs(
    title = "Original: Rank vs Touch Frequency (All Matches)",
    x = "Final Season Rank",
    y = "Total Touches"
  ) +
  theme_minimal()



original_plot + TouchFreq_vs_FinalStandings_clean


############################ Deep Dive Outlier Analysis ############################ 

Matches_foroutliers <- Matches_final %>%
  mutate(TeamID = substr(as.character(MatchID), 1, 2))

Touches_outliers_removed <- Touches_outliers_removed %>%
  mutate(OutlierStatus = "Outlier")

Touches_filteredoutliers <- Touches_filteredoutliers %>%
  mutate(OutlierStatus = "Normal")

# Combine both into one data frame
TouchMatch_Comparison <- bind_rows(Touches_outliers_removed, Touches_filteredoutliers) %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(Matches_foroutliers, by = c("Team" = "TeamID", "SeasonMatchNumber"))

TouchMatch_Comparison <- TouchMatch_Comparison %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0"))

TouchMatch_Comparison <- TouchMatch_Comparison %>%
  mutate(
    GoalsFor = case_when(
      GoalsFor %in% c("X", "XX") ~ "0",
      TRUE ~ GoalsFor
    ),
    GoalsFor = as.numeric(GoalsFor)
  )

# Compute sample size per group
n_labels <- TouchMatch_Comparison %>%
  group_by(OutlierStatus) %>%
  summarise(
    n = n(),
    y_pos = max(GoalsFor, na.rm = TRUE) + 0.5  # position just above max value
  )

#goals scored
ggplot(TouchMatch_Comparison, aes(x = OutlierStatus, y = GoalsFor)) +
  geom_boxplot(fill = "gray", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, color = "darkred") +
  geom_text(data = n_labels, aes(x = OutlierStatus, y = y_pos, label = paste0("n = ", n)), vjust = 0) +
  labs(
    title = "Goals Scored in Outlier vs Normal Matches",
    x = "Match Type",
    y = "Goals Scored"
  ) +
  theme_minimal()


#proportion of wins
TouchMatch_Comparison %>%
  group_by(OutlierStatus, Outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(OutlierStatus) %>%
  mutate(
    prop = n / sum(n),
    label_y = prop + 0.03  # slightly above the bar
  ) %>%
  ggplot(aes(x = OutlierStatus, y = prop, fill = Outcome)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0("n = ", n), y = label_y), 
            position = position_dodge(width = 0.9), 
            vjust = 0, size = 3.5) +
  labs(
    title = "Match Outcomes in Outlier vs Normal Matches",
    x = "Match Type",
    y = "Proportion of Matches"
  ) +
  theme_minimal()


# New n_labels specifically for Outcome x OutlierStatus
n_labels_touch <- TouchMatch_Comparison %>%
  group_by(Outcome, OutlierStatus) %>%
  summarise(
    n = n(),
    y_pos = max(TouchCount, na.rm = TRUE) + 2,
    .groups = "drop"
  )

#TouchCount vs Match Outcome
ggplot(TouchMatch_Comparison, aes(x = Outcome, y = TouchCount, fill = OutlierStatus)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  geom_text(data = n_labels_touch,
            aes(x = Outcome, y = y_pos, label = paste0("n = ", n), group = OutlierStatus),
            position = position_dodge(width = 0.75),
            vjust = 0, size = 3.5) +
  labs(
    title = "Touch Count by Match Result and Outlier Status",
    x = "Match Result",
    y = "Touches",
    fill = "Match Type"
  ) +
  theme_minimal()


#do outlier matches have significantly higher goals? 
t.test(GoalsFor ~ OutlierStatus, data = TouchMatch_Comparison)

#  Outlier matches (those with abnormally high/low touch rates) are associated with higher average goals scored than normal matches — by ~0.55 goals on average.

# This supports the idea that "touchy" or "chaotic" matches may coincide with more offensive action (more goals).
wilcox.test(GoalsFor ~ OutlierStatus, data = TouchMatch_Comparison)

#There is a statistically significant difference in the distribution of goals scored between outlier and normal matches (p = 0.0175).
#This confirms the earlier t-test finding — but with fewer assumptions (no need for normality or equal variances).

#statistically more likely to have wins?
# Create a contingency table of outcomes by outlier status
table_outcomes <- TouchMatch_Comparison %>%
  count(OutlierStatus, Outcome) %>%
  pivot_wider(names_from = Outcome, values_from = n, values_fill = 0) %>%
  column_to_rownames("OutlierStatus") %>%
  as.matrix()

# Chi-squared test (good for larger samples)
chisq.test(table_outcomes)

# Optional: Fisher's Exact Test (more accurate with small samples)
fisher.test(table_outcomes)

#ya it is stats sig

# Create a contingency table of outcomes by outlier status
table_outcomes <- table(TouchMatch_Comparison$OutlierStatus, TouchMatch_Comparison$Outcome)

# View the raw counts
print(table_outcomes)

# View the proportions per match type
prop.table(table_outcomes, margin = 1)  # margin = 1 → row-wise proportions (within each match type)


chisq.test(table_outcomes)     # For general large-sample significance
fisher.test(table_outcomes)    # Better for small sample sizes

#Outlier matches — defined as games with unusually high (MAD > 2) prosocial touch counts — are strongly associated with higher win rates. This supports the idea that elevated team touch behavior may correlate with better team performance.


############################ Outlier Summary: Which Teams, How Many, Avg Touches ############################ 

# Step 1: Compute per-team summary for Normal and Outlier matches
Outlier_Summary <- bind_rows(Touches_outliers_removed, Touches_filteredoutliers) %>%
  mutate(
    Team = str_pad(as.character(Team), width = 2, pad = "0"),
    OutlierStatus = factor(OutlierStatus, levels = c("Normal", "Outlier"))
  ) %>%
  group_by(Team, OutlierStatus) %>%
  summarise(
    n_matches = n(),
    avg_touches = mean(TouchCount, na.rm = TRUE),
    min_touches = min(TouchCount, na.rm = TRUE),
    max_touches = max(TouchCount, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2 (Optional): Wide format for side-by-side comparison
Outlier_Summary_wide <- Outlier_Summary %>%
  pivot_wider(
    names_from = OutlierStatus,
    values_from = c(n_matches, avg_touches, min_touches, max_touches),
    names_glue = "{.value}_{OutlierStatus}"
  )

# Step 3: Plot number of outlier vs normal matches per team
ggplot(Outlier_Summary, aes(x = Team, y = n_matches, fill = OutlierStatus)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of Outlier vs Normal Matches per Team",
    x = "Team",
    y = "Number of Matches",
    fill = "Match Type"
  ) +
  theme_minimal()

# Step 4 (Optional): View summary table
View(Outlier_Summary_wide)

library(ggplot2)

# Filter just the outlier matches
Touches_outliers_removed %>%
  ggplot(aes(x = Team, y = TouchCount, fill = ScaledTouch > 0)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.7, aes(color = ScaledTouch)) +
  geom_hline(yintercept = median(Touches_outliers_removed$TouchCount), linetype = "dashed", color = "black") +
  scale_color_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, name = "Scaled Touch") +
  labs(
    title = "Touch Frequencies in Outlier Matches",
    subtitle = "Color shows direction of deviation: blue = low, red = high",
    x = "Team",
    y = "Touches in Outlier Match"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Add a flag for whether a match is an outlier
Touches_scaled <- Touches_scaled %>%
  mutate(OutlierFlag = ifelse(abs(ScaledTouch) > 2, "Outlier", "Normal"))

# Plot all matches with MAD-scaled values, color by outlier status
ggplot(Touches_scaled, aes(x = Team, y = ScaledTouch, color = OutlierFlag)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
  scale_color_manual(values = c("Normal" = "gray", "Outlier" = "red")) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "black") +
  labs(
    title = "MAD-Scaled Touch Frequency per Match by Team",
    subtitle = "Outliers (|ScaledTouch| > 2) shown in red",
    x = "Team",
    y = "Scaled Touch (MAD Units)",
    color = "Match Type"
  ) +
  theme_minimal()


##################Parametric analysis

shapiro.test(Touches_scaled$TouchCount)         # Raw touches per match
shapiro.test(Team_Touches_Standings$TotalTouches)  # Total touches per season
shapiro.test(Touches_scaled$ScaledTouch)
shapiro.test(Team_touch_variability$MeanTouches)
shapiro.test(TouchMatch_Comparison$GoalsFor)
shapiro.test(TouchMatch_Comparison$TouchCount)
shapiro.test(Team_Touches_Standings$TotalTouches)


# Histogram
ggplot(Touches_scaled, aes(x = TouchCount)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Touch Count", x = "Touch Count", y = "Frequency") +
  theme_minimal()

# Q-Q Plot
qqnorm(Touches_scaled$TouchCount)
qqline(Touches_scaled$TouchCount, col = "red")

mad_val <- mad(Team_Touches_Standings$TotalTouches)
sd_val <- sd(Team_Touches_Standings$TotalTouches)
mad_to_sd_ratio <- mad_val / sd_val

library(ggplot2)

ggplot(Team_Touches_Standings, aes(x = TotalTouches)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(aes(xintercept = median(TotalTouches)), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = median(TotalTouches) + mad(TotalTouches)), color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = median(TotalTouches) - mad(TotalTouches)), color = "red", linetype = "dotted") +
  labs(title = "Density Plot with MAD Bands", x = "Total Touches", y = "Density") +
  theme_minimal()
