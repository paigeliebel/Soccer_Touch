# Social Network Strength Hypothesis
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
library(ggplot2)
library(forcats)
library(ggridges)
library(DescTools)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Overview_summary_Data.R") #Runs and brings in data frames from Core_Hypothesis.R script
source("MatchPerformance_Stats_PK.R") #Runs abd brings in data frames from MatchPerformance and stats script

#Check to make sure data frames are loaded:
if (!exists("Touches_final") | !exists("Touches_scaled") | !exists("Matches_finalID") | !exists("FinalStandings") | !exists("Touches_CoreData")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded.")
}

############################ Social Network Strength ############################

#Creates Dataframe that Flags values that don't match with requirements of strings, jersey numbers, G, SU, ??
#Cleans common typos (which were many)
Touches_players_flagged <- Touches_final %>%
  mutate(
    ToucherNumber = as.character(ToucherNumber),
    ToucheeNumber = as.character(ToucheeNumber),
    PlayersInvolved = as.character(PlayersInvolved),
    PlayersInvolved = PlayersInvolved %>%
      str_replace_all("\\.\\s+", ",") %>%           # fix "10. 12" → "10,12"
      str_remove_all("[\"'`:;.`]") %>%              # Remove unwanted punctuation
      str_replace_all("\\s*,\\s*", ",") %>%         # Normalize commas and spacing
      str_replace_all("\\s+", "") %>%               # Remove stray spaces
      str_replace_all("(?<=\\d{2})(?=\\d{2}$)", ",") %>%       # Insert comma in "1210" → "12,10"
      str_replace(",+$", "") %>%                    # Remove trailing commas
      str_trim(),                                   # Clean up leading/trailing space 
    
    
    # Valid if it's a number, "G", "SU", or "??"
    ToucherNumber_Valid = str_detect(ToucherNumber, "^\\d{1,2}$|^G$|^SU$|^\\?\\?$"),
    ToucheeNumber_Valid = str_detect(ToucheeNumber, "^\\d{1,2}$|^G$|^SU$|^\\?\\?$"),
    
    # Valid PlayersInvolved: at least two elements (numbers or ?? or SU), comma-separated
    PlayersInvolved_List = str_split(PlayersInvolved, ",\\s*"),
    PlayersInvolved_Valid = map_lgl(PlayersInvolved_List, function(players) {
      cleaned <- str_trim(players)
      all_valid <- all(str_detect(cleaned, "^\\d+$|^\\?\\?$|^SU$"))
      has_multiple <- length(cleaned) >= 2
      all_valid && has_multiple
    })
  )
#Splits "12,04,09" back into "12, 04, 09"
Touches_players_flagged <- Touches_players_flagged %>%
  mutate(PlayersInvolved = str_split(PlayersInvolved, ","))

#Ignore those flagged values for now (fix them before final paper)
Touches_players_final <- Touches_players_flagged %>%
  filter(
    ToucherNumber_Valid,
    ToucheeNumber_Valid,
    PlayersInvolved_Valid
  )

############################ Player Concentration | Social Network Strength ############################

#########Histograms for Toucher by team (creates 14 histograms for team by team analysis)

# Count number of times each player was the Toucher within each team
toucher_counts <- Touches_players_final %>%
  filter(!is.na(ToucherNumber) & ToucherNumber != "G") %>%
  group_by(Team, ToucherNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

ggplot(toucher_counts, aes(x = ToucherNumber, y = TouchCount)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Team, scales = "free_x") +
  labs(
    title = "Toucher Frequency by Player Jersey Number",
    x = "Player (ToucherNumber)",
    y = "Number of Touches"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(face = "bold")
  )

#########Histograms for Touchee by team (creates 14 histograms for team by team analysis)

# Count number of times each player was the Touchee within each team
touchee_counts <- Touches_players_final %>%
  filter(!is.na(ToucheeNumber) & ToucheeNumber != "G") %>%
  group_by(Team, ToucheeNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

ggplot(touchee_counts, aes(x = ToucheeNumber, y = TouchCount)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Team, scales = "free_x") +
  labs(
    title = "Touchee Frequency by Player Jersey Number",
    x = "Player (ToucheeNumber)",
    y = "Number of Touches"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(face = "bold")
  )

#########Histograms for players involved in any touch event, as toucher, touchee or group (creates 14 histograms for team by team analysis)

# Properly split and unnest
player_counts <- Touches_players_final %>%
  select(Team, PlayersInvolved) %>%
  separate_rows(PlayersInvolved, sep = ",\\s*") %>%
  mutate(PlayersInvolved = str_trim(PlayersInvolved)) %>%
  filter(PlayersInvolved != "") %>%
  group_by(Team, PlayersInvolved) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  arrange(Team, desc(TouchCount))

ggplot(player_counts, aes(x = fct_reorder(PlayersInvolved, -TouchCount), y = TouchCount)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Team, scales = "free_x") +
  labs(
    title = "Player Touch Involvement (PlayersInvolved Column)",
    subtitle = "Each bar shows one player's total involvement across the season",
    x = "Player Jersey / Code",
    y = "Touch Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
    strip.text = element_text(face = "bold")
  )

#Using the shrinkage factor
# player_touch_shrink_clean <- player_touch_shrink %>%
#   filter(!is.na(Player), !is.na(AdjustedTouches))
# 
# ggplot(player_touch_shrink_clean, aes(x = fct_reorder(Player, -AdjustedTouches), y = AdjustedTouches)) +
#   geom_col(fill = "steelblue") +
#   facet_wrap(~ Team, scales = "free_x") +
#   labs(
#     title = "Shrinkage-Weighted Player Touch Involvement",
#     subtitle = "Touch involvement scaled by log1p(Minutes Played)",
#     x = "Player Jersey",
#     y = "Adjusted Touch Involvement"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
#     strip.text = element_text(face = "bold")
#   )

#########Histograms for players involved in group events by team (creates 14 histograms for team by team analysis)

#Filters for just rows that contain the "G"
group_touches <- Touches_players_final %>%
  filter(ToucherNumber == "G")

player_counts_groupsanalysis <- group_touches %>%
  select(Team, PlayersInvolved, ToucherNumber) %>%
  separate_rows(PlayersInvolved, sep = ",\\s*") %>%
  mutate(PlayersInvolved = str_trim(PlayersInvolved)) %>%
  filter(PlayersInvolved != "") %>%
  group_by(Team, PlayersInvolved) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  arrange(Team, desc(TouchCount))

#Plots
ggplot(player_counts_groupsanalysis, aes(x = PlayersInvolved, y = TouchCount)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Team, scales = "free_x") +
  labs(
    title = "Player Involvement in Group Events (Touches_final)",
    subtitle = "Each bar shows a player's total involvement in group touches",
    x = "Player Jersey",
    y = "Group Touch Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#########Ridge plot that shows touchiest players stacked on top of each other by team (one plot for all 14 teams)

# Build long-format player count data (from cleaned PlayersInvolved)
player_ridgeplot_counts <- Touches_players_final %>%
  select(Team, PlayersInvolved) %>%
  separate_rows(PlayersInvolved, sep = ",\\s*") %>%
  mutate(PlayersInvolved = str_trim(PlayersInvolved)) %>%
  filter(PlayersInvolved != "") %>%
  group_by(Team, PlayersInvolved) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Assign within-team ranks (1 = most touchy)
player_ridgeplot_counts <- player_ridgeplot_counts %>%
  group_by(Team) %>%
  arrange(desc(TouchCount)) %>%
  mutate(
    TouchiestRank = row_number()  # Numeric: 1 = most touchy
  ) %>%
  ungroup()

# Calculate standard deviation of TouchCount per team
team_SD <- player_ridgeplot_counts %>%
  group_by(Team) %>%
  summarise(TouchSD = sd(TouchCount), .groups = "drop")

# Step 2: Reorder Team factor based on flatness (ascending)
player_ridgeplot_counts <- player_ridgeplot_counts %>%
  left_join(team_SD, by = "Team") %>%
  mutate(Team = fct_reorder(Team, TouchSD, .desc = FALSE))  # flattest first

# Plot ridge plot using numeric ranks
ggplot(player_ridgeplot_counts, aes(x = TouchiestRank, y = fct_rev(Team), height = TouchCount, group = Team)) +
  geom_ridgeline(stat = "identity", fill = "steelblue", color = "white", alpha = 0.8, scale = 0.9) +
  scale_x_continuous(breaks = 1:50, expand = c(0.01, 0)) +
  labs(
    title = "Player Touch Involvement Ridge Plot by Team",
    subtitle = "Teams ordered front-to-back from flattest to most peaked touch distributions",
    x = "Player Touch Rank within Team (1 = Most Involved)",
    y = "Team"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

#Unstacked Ridge plot
ggplot(player_ridgeplot_counts, aes(x = TouchiestRank, y = Team, height = TouchCount, group = Team)) +
  geom_density_ridges(stat = "identity", scale = 1.5, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Player Touch Involvement by Team",
    subtitle = "Teams ordered from flattest to most peaked distribution",
    x = "Player Touchiness Rank (1 = Most Involved)",
    y = "Team"
  ) +
  theme_minimal()

############################ Player Concentration | Social Network Strength to Final Standings ############################

# Merge flatness, player touchiness, and season rank into player data
player_touchiness_rank <- player_ridgeplot_counts %>%
  mutate(Team = as.numeric(as.character(Team))) %>%
  left_join(
    FinalStandings %>%
      mutate(TeamID = as.numeric(TeamID)) %>%
      select(TeamID, Rank),
    by = c("Team" = "TeamID")
  )

player_touchiness_rank <- player_touchiness_rank %>%
  mutate(
    Team = factor(Team),
    Team = fct_reorder(Team, Rank, .desc = FALSE)  # Rank 1 at top/front
  )

#filter to only top 18 players of touchiness... tails too long
player_touchiness_rank_top <- player_touchiness_rank %>%
  filter(TouchiestRank <= 18)

# Ridge plot (non-stacked), teams ordered by final season standings
ggplot(player_touchiness_rank_top, aes(x = TouchiestRank, y = Team, height = TouchCount, group = Team)) +
  geom_density_ridges(stat = "identity", scale = 1.5, fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Distribution of Player Touch Involvement by Team",
    subtitle = "Teams ordered by final season standings (Bottom of y-axis is 1st ranked team)",
    x = "Player Touchiness Rank (1 = Most Involved)",
    y = "Team"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

#plot team flatness SD to final season rank in a simple scatter plot
#low standard deviation = flatter distribution (single players do not dominate touch interactions)
team_SD_rank <- team_SD %>%
  mutate(Team = as.numeric(as.character(Team))) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID"))

team_cv <- player_touchiness_rank_top %>%
  mutate(Team = as.numeric(as.character(Team))) %>%
  group_by(Team) %>%
  summarise(
    MeanTouches = mean(TouchCount),
    SDTouches = sd(TouchCount),
    CV = SDTouches / MeanTouches,
    .groups = "drop"
  )

team_cv_rank <- team_cv %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID"))

#Look at CV (Coefficient of Variation): CV = SD / Mean -- normalized level of spread
# Plot scatter
ggplot(team_cv_rank, aes(x = Rank, y = CV)) +
  geom_point(size = 3, color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_x_reverse(breaks = 1:max(team_SD_rank$Rank)) +  # Lower rank = better
  labs(
    title = "Touch Concentration vs Final Season Rank",
    subtitle = "Higher SD = touches concentrated in fewer players",
    x = "Final Season Rank (1 = Best)",
    y = "Touch Count Standard Deviation (Per Team)"
  ) +
  theme_minimal()

cor.test(team_cv_rank$CV, team_cv_rank$Rank)

######Gini Coefficient################
team_gini <- player_touchiness_rank %>%
  group_by(Team) %>%
  summarise(
    Gini = Gini(TouchCount),
    .groups = "drop"
  )
team_gini_rank <- team_gini %>%
  mutate(Team = as.numeric(as.character(Team))) %>% 
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID"))

ggplot(team_gini_rank, aes(x = Rank, y = Gini)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_x_reverse(breaks = 1:max(team_gini_rank$Rank)) +  # Rank 1 = best, so reverse axis
  labs(
    title = "Gini Coefficient: Touch Inequality vs Final Season Rank",
    subtitle = "Higher Gini = More touches concentrated in fewer players",
    x = "Final Season Rank (1 = Best)",
    y = "Gini Coefficient of Touch Distribution"
  ) +
  theme_minimal()

summary(lm(Gini ~ Rank, data = team_gini_rank))


#Percentage of touches from top 3 touchiest players
# Step 1: Sum total touches per team
team_total_touches <- player_touchiness_rank %>%
  group_by(Team) %>%
  summarise(TotalTouches = sum(TouchCount), .groups = "drop")

# Step 2: Get top 3 players per team and their touch count
top3_touches <- player_touchiness_rank %>%
  group_by(Team) %>%
  arrange(desc(TouchCount)) %>%
  slice_head(n = 3) %>%  # top 3 players
  summarise(Top3Touches = sum(TouchCount), .groups = "drop")

# Step 3: Merge and calculate proportion
touch_concentration <- top3_touches %>%
  left_join(team_total_touches, by = "Team") %>%
  mutate(Top3_Proportion = Top3Touches / TotalTouches)

# Step 4 (Optional): Join with final standings to analyze relationship
touch_concentration <- touch_concentration %>%
  mutate(Team = as.numeric(as.character(Team))) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID"))

ggplot(touch_concentration, aes(x = Rank, y = Top3_Proportion)) +
  geom_point(size = 3, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_x_reverse(breaks = 1:max(touch_concentration$Rank)) +
  labs(
    title = "Touch Concentration in Top 3 Players vs Final Season Rank",
    subtitle = "Higher values = more concentrated touch behavior in fewer players",
    x = "Final Season Rank (1 = Best)",
    y = "Proportion of Team Touches by Top 3 Players"
  ) +
  theme_minimal()

cor.test(touch_concentration$Top3_Proportion, touch_concentration$Rank)

summary(lm(Top3_Proportion ~ Rank, data = touch_concentration))

