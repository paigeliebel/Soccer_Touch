# Core_Hypothesis
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script

#note that this is not complete until we get Simon Data (only 2/3s of it so far)
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
ggplot(Team_Touches_Standings, aes(x = Rank, y = TotalTouches)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  scale_x_reverse() +
  labs(
    title = "Final Rank vs Overall Touch Frequency",
    x = "Final Season Rank",
    y = "Total Touches (Filtered)"
  ) +
  theme_minimal()

cor_result <- cor.test(Team_Touches_Standings$TotalTouches, Team_Touches_Standings$Rank)

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

