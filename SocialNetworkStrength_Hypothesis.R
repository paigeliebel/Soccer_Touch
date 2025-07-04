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

############################ Social Network Strength ############################

#Creates Dataframe that Flags values that don't match with requirements of strings, jersey numbers, G, SU, ??
#Cleans common typos (which were many)
Touches_players_flagged <- Touches_ProSocial %>%
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
  filter(!is.na(ToucherNumber) & ToucherNumber != "G" & Reciprocal == "N") %>%
  group_by(Team, ToucherNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

toucher_histo <- ggplot(toucher_counts, aes(x = ToucherNumber, y = TouchCount)) +
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

toucher_team_gini <- toucher_counts %>%
  group_by(Team) %>%
  summarise(Gini = Gini(TouchCount)) %>%
  arrange(desc(Gini))

# Use Gini order for Team factor levels
toucher_counts <- toucher_counts %>%
  mutate(Team = factor(Team, levels = toucher_team_gini$Team))

# Plot with teams ordered by Gini (highest concentration at top)
Toucher_plot_gini_ordered <- ggplot(toucher_counts, aes(x = TouchCount, y = Team, fill = Team)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  labs(
    title = "Distribution of Toucher Counts per Player by Team (Ordered by Gini Coefficient)",
    x = "Number of Touches",
    y = "Team (High to Low Concentration)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#########Histograms for Touchee by team (creates 14 histograms for team by team analysis)

# Count number of times each player was the Touchee within each team
touchee_counts <- Touches_players_final %>%
  filter(!is.na(ToucheeNumber) & ToucheeNumber != "G" & Reciprocal == "N") %>%
  group_by(Team, ToucheeNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

Touchee_histo <- ggplot(touchee_counts, aes(x = ToucheeNumber, y = TouchCount)) +
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

# Calculate Gini coefficient for touchee counts per team
touchee_team_gini <- touchee_counts %>%
  group_by(Team) %>%
  summarise(Gini = Gini(TouchCount)) %>%
  arrange(desc(Gini))

# Order Team factor by touchee Gini (descending)
touchee_counts <- touchee_counts %>%
  mutate(Team = factor(Team, levels = touchee_team_gini$Team))

# Ridge plot for touchee counts ordered by Gini
Touchee_plot_gini_ordered <- ggplot(touchee_counts, aes(x = TouchCount, y = Team, fill = Team)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  labs(
    title = "Distribution of Touchee Counts per Player by Team (Ordered by Gini Coefficient)",
    x = "Number of Touches",
    y = "Team (High to Low Concentration)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

############# Touchee vs Toucher involvement league wide ##################333

# Combine toucher and touchee counts per player and team
combined_counts <- toucher_counts %>%
  rename(Player = ToucherNumber, ToucherCount = TouchCount) %>%
  full_join(
    touchee_counts %>% rename(Player = ToucheeNumber, ToucheeCount = TouchCount),
    by = c("Team", "Player")
  ) %>%
  mutate(
    ToucherCount = replace_na(ToucherCount, 0),
    ToucheeCount = replace_na(ToucheeCount, 0),
    TotalTouches = ToucherCount + ToucheeCount
  ) %>%
  filter(TotalTouches >= 10)  # filter players with 10 or more combined touches

# Scatter plot league-wide
Toucher_v_Touchee_plot_global <- ggplot(combined_counts, aes(x = ToucheeCount, y = ToucherCount)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "League-wide Player Touch Counts: Touchee vs. Toucher",
    x = "Touchee Count (Touches Received)",
    y = "Toucher Count (Touches Initiated)"
  ) +
  theme_minimal()

combined_counts_team <- toucher_counts %>%
  rename(Player = ToucherNumber, ToucherCount = TouchCount) %>%
  full_join(
    touchee_counts %>% rename(Player = ToucheeNumber, ToucheeCount = TouchCount),
    by = c("Team", "Player")
  ) %>%
  mutate(
    ToucherCount = replace_na(ToucherCount, 0),
    ToucheeCount = replace_na(ToucheeCount, 0),
    TotalTouches = ToucherCount + ToucheeCount
  ) %>%
  filter(TotalTouches >= 10)  # keep only active players

Toucher_v_Touchee_plot_team <- ggplot(combined_counts_team, aes(x = ToucheeCount, y = ToucherCount)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ Team, scales = "free") +
  labs(
    title = "Player Touch Counts: Touchee vs. Toucher by Team (≥10 touches)",
    x = "Touchee Count (Touches Received)",
    y = "Toucher Count (Touches Initiated)"
  ) +
  theme_minimal()

team_correlations_touchervtouchee <- combined_counts %>%
  group_by(Team) %>%
  summarise(
    cor = cor(ToucheeCount, ToucherCount, method = "pearson")
  ) %>%
  arrange(desc(cor))

combined_counts_ratio <- toucher_counts %>%
  rename(Player = ToucherNumber, ToucherCount = TouchCount) %>%
  full_join(
    touchee_counts %>% rename(Player = ToucheeNumber, ToucheeCount = TouchCount),
    by = c("Team", "Player")
  ) %>%
  mutate(
    ToucherCount = replace_na(ToucherCount, 0),
    ToucheeCount = replace_na(ToucheeCount, 0),
    TotalTouches = ToucherCount + ToucheeCount,
    RoleRatio = (ToucherCount + 1) / (ToucheeCount + 1)
  ) %>%
  filter(TotalTouches >= 10)  # focus on active players

combined_ratios_plot <- ggplot(combined_counts_ratio, aes(x = Team, y = RoleRatio)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Player Role Ratios by Team",
    x = "Team",
    y = "Toucher / Touchee Ratio"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")  # line at balanced ratio

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

player_gini <- player_counts %>%
  group_by(Team) %>%
  summarise(Gini = Gini(TouchCount)) %>%
  arrange(desc(Gini))

player_counts <- player_counts %>%
  mutate(Team = factor(Team, levels = player_gini$Team))

player_involved_histo <- ggplot(player_counts, aes(x = fct_reorder(PlayersInvolved, -TouchCount), y = TouchCount)) +
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

player_involved_ridgeplot <- ggplot(player_counts, aes(x = TouchCount, y = Team, fill = Team)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  labs(
    title = "Distribution of Player Touch Involvement by Team (Ordered by Gini)",
    x = "Touch Count (Player Involvement)",
    y = "Team"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#What Anne asked for:
# 1. Calculate team mean touch count
player_counts_mean <- player_counts %>%
  group_by(Team) %>%
  mutate(TeamMeanTouch = mean(TouchCount)) %>%
  ungroup()

# Calculate relative touch count per player (touches minus team mean)
player_counts_mean <- player_counts_mean %>%
  mutate(TouchCount_relative = TouchCount - TeamMeanTouch)

# Optional: reorder teams by Gini or other metric
player_gini <- player_counts_mean %>%
  group_by(Team) %>%
  summarise(Gini = Gini(TouchCount)) %>%
  arrange(desc(Gini))

player_counts_mean <- player_counts_mean %>%
  mutate(Team = factor(Team, levels = player_gini$Team))

# Plot ridge plot with TouchCount_relative on x-axis, Team on y-axis
player_counts_mean_plot <- ggplot(player_counts_mean, aes(x = TouchCount_relative, y = Team, fill = Team)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # line at team mean
  labs(
    title = "Player Touch Involvement Relative to Team Mean",
    subtitle = "Negative = below team mean, Positive = above team mean",
    x = "Touch Count Difference from Team Mean",
    y = "Team"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



#Compare gini coeeficient to final season standings. 

player_gini <- player_gini %>%
  mutate(Team = as.numeric(Team),
         Team = str_trim(Team))

FinalStandings <- FinalStandings %>%
  mutate(TeamID = as.numeric(TeamID),
         TeamID = str_trim(TeamID))

# Join player involvement Gini with final standings
gini_with_rank <- player_gini %>%
  inner_join(FinalStandings, by = c("Team" = "TeamID"))

# Scatter plot: Rank vs. Gini coefficient
gini_rank_plot <- ggplot(gini_with_rank, aes(x = Rank, y = Gini)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_x_reverse(breaks = sort(unique(gini_with_rank$Rank))) +  # Best team on left
  labs(
    title = "Player Touch Involvement Concentration vs. Final Season Rank",
    x = "Final Season Rank (1 = Best)",
    y = "Gini Coefficient of Player Touch Involvement"
  ) +
  theme_minimal()

# Calculate Spearman correlation between rank and Gini
spearman_corr <- cor(gini_with_rank$Rank, gini_with_rank$Gini, method = "spearman")

# What if we do this by game? Again, with goal differential? Where a more evenly spread game (gini coefficient) could produce a better goal differential?