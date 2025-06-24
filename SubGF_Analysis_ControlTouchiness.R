# Sub_Analysis and GF
# For more information on these data frames please look at the README.md file

#Goal of this script is to compare amount of SUB touches (controlled by number of SUBs), and separately total Goals For, to the overall touch amount in the Core Hyp to see if teams are consistent

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

############################ SUB | Create Data Set for this Hypothesis ############################ 

# Prosocial touches are defined as all haptic rituals recorded excluding: 
#   Tactical Adjustments, Collisions, and Negative Touch
# 
# Situations excluded from analysis for this hypothesis: Goals For/Against, Substitutions

# Define exclusions for non-prosocial touches
Exclude_Touch <- c("TA", "CO", "NEG")

# Create data frame with ONLY touches during SUB situation
Touches_SUB <- Touches_final %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(Situation == "SUB") %>%
  mutate(Team = str_trim(as.character(Team)))  # Clean Team column

# Count SUB touches per team
SUB_Touches_by_team <- Touches_SUB %>%
  count(Team, name = "Total_SUB_Touches")

#Number of subs:
# Create a working copy with TeamID and SubCount calculated
Matches_Subs <- Matches_final %>%
  mutate(
    TeamID = substr(MatchID, 1, 2),
    SubCount = if_else(
      is.na(Substitutes) | Substitutes == "",
      0,
      str_count(Substitutes, ",") + 1
    )
  )

# Aggregate total substitutions per team
Subs_per_team <- Matches_Subs %>%
  group_by(TeamID) %>%
  summarise(TotalSubs = sum(SubCount), .groups = "drop")

# Join SUB touch counts with sub counts
Touches_per_Sub <- Subs_per_team %>%
  left_join(SUB_Touches_by_team, by = c("TeamID" = "Team")) %>%
  mutate(
    Total_SUB_Touches = if_else(is.na(Total_SUB_Touches), 0, Total_SUB_Touches),  # fill missing with 0
    Touches_per_Sub = Total_SUB_Touches / TotalSubs
  )

# Join together
Compare_Touchiness <- Team_Touches_Standings %>%
  select(TeamID, TotalTouches) %>%
  left_join(Touches_per_Sub, by = "TeamID") %>%
  filter(!is.na(Touches_per_Sub), TotalSubs > 0)

# Scatter plot
Touchiness_Comparison_Plot <- ggplot(Compare_Touchiness, aes(x = TotalTouches, y = Touches_per_Sub)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Are Touchy Teams Touchy during Subs?",
    x = "Total Prosocial Touches (Core Hyp)",
    y = "Touches per Substitution Event"
  ) +
  theme_minimal()

# Correlation test
Touchiness_Correlation_Stats <- cor.test(Compare_Touchiness$TotalTouches, Compare_Touchiness$Touches_per_Sub)

############################ GoalsFor | Create Data Set for this Hypothesis ############################ 

# Filter only GF situations
Touches_GF <- Touches_final %>%
  filter(!(HapticRitual %in% Exclude_Touch)) %>%
  filter(Situation == "GF") %>%
  mutate(Team = str_trim(as.character(Team)))  # Clean Team column

# Count GF touches per team
GF_Touches_by_team <- Touches_GF %>%
  count(Team, name = "Total_GF_Touches")

# Join with standings
Touches_per_Goal <- Team_Touches_Standings %>%
  select(TeamID, TotalTouches, SeasonGoalsFor) %>%
  left_join(GF_Touches_by_team, by = c("TeamID" = "Team")) %>%
  mutate(
    Total_GF_Touches = if_else(is.na(Total_GF_Touches), 0, Total_GF_Touches),  # fill NAs
    Touches_per_Goal = if_else(SeasonGoalsFor > 0, Total_GF_Touches / SeasonGoalsFor, NA_real_)  # avoid divide by 0
  )

# Plot: Are touchy teams more touchy per goal celebration?
Touchiness_vs_GF_Plot <- ggplot(Touches_per_Goal, aes(x = TotalTouches, y = Touches_per_Goal)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "orange", linewidth = 1) +
  labs(
    title = "Are Touchy Teams More Touchy per Goal?",
    x = "Total Prosocial Touches (Core Hyp)",
    y = "Touches per Goal Celebration (GF)"
  ) +
  theme_minimal()


# Correlation test
Touchiness_vs_GF_Stats <- cor.test(Touches_per_Goal$TotalTouches, Touches_per_Goal$Touches_per_Goal)

