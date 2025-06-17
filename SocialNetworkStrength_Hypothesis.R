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
source("Core_Hypothesis.R") #Runs and brings in data frames from Core_Hypothesis.R script
source("InterMatch_Variability_Hypothesis.R") #Runs and brings in data frames from InterMatch_Variability_Hypothesis.R script

#note that this is not complete until we get Simon Data (only 2/3s of it so far)
#Ensure to use the correct dfs. Touches_final and Matches_final are correct. They only include assigned rater data, no repeat matches

#Check to make sure data frames are loaded:

if (!exists("Touches_final") | !exists("Touches_scaled") | !exists("Matches_finalID") | !exists("FinalStandings") | !exists("Touches_CoreHyp")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded. Check Data_Management.R and Core_Hypothesis.R.")
}

############################ Social Network Strength ############################

#Clean data to grab what we need: reciprocity, toucher numbers, touchee numbers
#Group event also count towards reciprocal, so long as it is not GF, SUB etc 

Touches_Reciprocal <- Touches_final %>% 
  mutate(
    Team = as.character(Team),
    Reciprocity = str_trim(Reciprocal) #cleans up white spaces in case
  )


############################ Reciprocal/Non-Reciprocal | Social Network Strength ############################

#Reciprocal vs non-reciprocal touch | simple final standings to ratio of reciprocal/non-reciprocal

# Count touches by team and reciprocity type
Reciprocity_by_Team <- Touches_Reciprocal %>%
  filter(!is.na(Reciprocity)) %>%
  mutate(
    ReciprocityType = case_when(
      Reciprocity %in% c("Y", "G") ~ "Reciprocal", #Reciprocal includes group events
      Reciprocity == "N" ~ "NonReciprocal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ReciprocityType)) %>%
  group_by(Team, ReciprocityType) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = ReciprocityType, values_from = TouchCount, values_fill = 0) %>%
  mutate(
    Reciprocal_To_NonRecip_Ratio = Reciprocal / NonReciprocal,
    DataFlag = if_else(Reciprocal + NonReciprocal == 0, "No Touch Data", "Data Available")
  )

#Join with Final Standings
Reciprocal_vs_Rank <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0")) %>%
  left_join(Reciprocity_by_Team, by = c("TeamID" = "Team"))

#Plot
ggplot(Reciprocal_vs_Rank, aes(x = Rank, y = Reciprocal_To_NonRecip_Ratio)) +
  geom_point(size = 3, color = "steelblue") +  # all teams shown in one color
  geom_smooth(
    data = filter(Reciprocal_vs_Rank, !is.na(Reciprocal_To_NonRecip_Ratio)),
    aes(x = Rank, y = Reciprocal_To_NonRecip_Ratio),
    method = "lm", se = FALSE, color = "black"
  ) +
  scale_x_reverse(breaks = 1:max(Reciprocal_vs_Rank$Rank)) +
  labs(
    title = "Reciprocal Touch Ratio vs Final Season Rank per Team",
    subtitle = "Higher ratios may reflect stronger intra-team cohesion",
    caption = "Note: Each dot represents one team",
    x = "Team Final Season Rank",
    y = "Team Touch Ratio (Reciprocal : Non-Reciprocal)",
  ) +
  theme_minimal()

#Stats Sum
reciprocal_lm <- lm(Rank ~ Reciprocal_To_NonRecip_Ratio, data = Reciprocal_vs_Rank)


############################ Player Concentration | Social Network Strength ############################


#I feel like if we are looking at distribution we definitely have to scale for time-played.
#Players who touch less but only played 20 minutes across the season needs to have less weight
#But, then I also need to make sure a player wo only played 20 minutes and touched a bunch doesn't throw it all ooff
#I want to weigh both touch per minute and amount of minutes played?
#I want to scale for playing time, but not let tiny-sample outliers (e.g., someone who touched the ball 12 times in a 20-minute cameo) dominate the metric.

#Options include a shrinkage factor: AdjustedTouchesPer90 = (Touches / MinutesPlayed) * 90 * log1p(MinutesPlayed)
#Touches / MinutesPlayed * 90 = standard touches per 90
#log1p(MinutesPlayed) = soft weigh




#Everything below needs to get looked at again with the above concern addressed!

#concentration of touch within players on a team could be a metric for overall team cohesion | ridge plot
#include injured-touch because we are looking at how touchy some players are

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
  left_join(
    FinalStandings %>% select(TeamID, Rank),
    by = c("Team" = "TeamID")  # Match Team to TeamID
  )

player_touchiness_rank <- player_touchiness_rank %>%
  mutate(
    Team = factor(Team),
    Team = fct_reorder(Team, Rank, .desc = FALSE)  # Rank 1 at top/front
  )

#filter to only top 20 players of touchiness... tails too long
player_touchiness_rank_top <- player_touchiness_rank %>%
  filter(TouchiestRank <= 20)

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
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID"))

team_cv <- player_touchiness_rank %>%
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

#Gini Coefficient
team_gini <- player_touchiness_rank %>%
  group_by(Team) %>%
  summarise(
    Gini = Gini(TouchCount),
    .groups = "drop"
  )

#Percentage of touches from top 3 touchiest players
# Step 1: Sum total touches per team
team_total_touches <- player_touchiness_rank %>%
  group_by(Team) %>%
  summarise(TotalTouches = sum(TouchCount), .groups = "drop")

# Step 2: Get top 3 players per team and their touch count
top3_touches <- player_touchiness_rank %>%
  group_by(Team) %>%
  arrange(desc(TouchCount)) %>%
  slice_head(n = 11) %>%  # top 3 players
  summarise(Top3Touches = sum(TouchCount), .groups = "drop")

# Step 3: Merge and calculate proportion
touch_concentration <- top3_touches %>%
  left_join(team_total_touches, by = "Team") %>%
  mutate(Top3_Proportion = Top3Touches / TotalTouches)

# Step 4 (Optional): Join with final standings to analyze relationship
touch_concentration <- touch_concentration %>%
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
