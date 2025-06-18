# Core_Hypothesis
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
#Exclude_Visibility <- c("P")

#Creates data set for core hypothesis analysis
Touches_CoreHyp <- Touches_final %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(!(Situation %in% Exclude_Situation)) #%>% 
  #filter(!(Visibility %in% Exclude_Visibility))

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



#Following looks at the number of subs each team has over the season and number of corners etc (looking at match data)
# #Number of subs:
# # Step 1: Create a working copy with TeamID and SubCount calculated
# Matches_Subs <- Matches_final %>%
#   mutate(
#     TeamID = substr(MatchID, 1, 2),
#     SubCount = if_else(
#       is.na(Substitutes) | Substitutes == "",
#       0,
#       str_count(Substitutes, ",") + 1
#     )
#   )
# 
# # Step 2: Aggregate total substitutions per team
# Subs_per_team <- Matches_Subs %>%
#   group_by(TeamID) %>%
#   summarise(TotalSubs = sum(SubCount), .groups = "drop")
# 
# # Step 1: Create a working version with TeamID and TotalCorners
# Matches_with_corners <- Matches_final %>%
#   mutate(
#     TeamID = substr(MatchID, 1, 2),
#     CornersFor = as.numeric(CornersFor),
#     CornersAgainst = as.numeric(CornersAgainst),
#     TotalCorners = CornersFor + CornersAgainst
#   )
# 
# # Step 2: Aggregate total corners per team
# Corners_per_team <- Matches_with_corners %>%
#   group_by(TeamID) %>%
#   summarise(TotalCorners = sum(TotalCorners, na.rm = TRUE), .groups = "drop")

