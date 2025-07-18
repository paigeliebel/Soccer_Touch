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

# Step 1: Join TotalSecondsPlayed
combined_counts_with_playtime <- combined_counts %>%
  left_join(
    player_season_totals %>% select(TeamID, Player, TotalSecondsPlayed),
    by = c("Team" = "TeamID", "Player")
  ) %>%
  filter(!is.na(TotalSecondsPlayed))  # remove if no playtime data

# Scatter plot league-wide
Toucher_v_Touchee_plot_global <- ggplot(combined_counts_with_playtime, 
                                        aes(x = ToucheeCount, 
                                            y = ToucherCount, 
                                            color = TotalSecondsPlayed)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_gradient(low = "gray80", high = "black", name = "Seconds Played") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  annotate("text", x = 75, y = 72, 
           label = "x = y", angle = 45, hjust = -0.1, size = 4, family = "Times New Roman") +
  coord_fixed() +  # ensures square aspect ratio
  xlim(0, 100) +
  ylim(0, 100) +
  labs(
    title = "League-wide Player Touch Profile",
    subtitle = "Dot shade reflects seconds played",
    x = "Touchee Count (Touches Received)",
    y = "Toucher Count (Touches Initiated)"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10)
  )




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

# Step 1: Join and count player involvement
player_counts <- Touches_players_final %>%
  select(Team, PlayersInvolved) %>%
  separate_rows(PlayersInvolved, sep = ",\\s*") %>%
  mutate(PlayersInvolved = str_trim(PlayersInvolved)) %>%
  filter(PlayersInvolved != "") %>%
  group_by(Team, PlayersInvolved) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Step 2: Join with RankLabels
player_counts <- player_counts %>%
  left_join(FinalStandings %>% select(TeamID, Rank, RankLabel), by = c("Team" = "TeamID"))

# Step 3: Count number of players per team
player_counts_summary <- player_counts %>%
  group_by(RankLabel) %>%
  summarise(n_players = n_distinct(PlayersInvolved), .groups = "drop")

# Step 4: Add player count info for facet labels
player_counts <- player_counts %>%
  left_join(player_counts_summary, by = "RankLabel") %>%
  mutate(FacetLabel = paste0(RankLabel, " (n = ", n_players, ")"))

# Step 5: Create a sorting order for players within each team
player_counts <- player_counts %>%
  group_by(FacetLabel) %>%
  arrange(desc(TouchCount), .by_group = TRUE) %>%
  mutate(PlayerOrder = row_number()) %>%
  ungroup()

# Step 6: Create a unique ID for plotting x-axis, but we’ll remove labels
player_counts <- player_counts %>%
  mutate(TeamPlayerID = paste0(FacetLabel, "_", PlayersInvolved))

# Force FacetLabel to order by ascending rank (R1 to R14)
player_counts <- player_counts %>%
  mutate(FacetLabel = factor(FacetLabel, 
                             levels = player_counts %>%
                               distinct(Rank, FacetLabel) %>%
                               arrange(Rank) %>%
                               pull(FacetLabel)))

# Plot with custom layout and smaller strip text
player_involved_plot <- ggplot(player_counts, aes(x = reorder(TeamPlayerID, -PlayerOrder), y = TouchCount)) +
  geom_col(fill = "gray40") +
  facet_wrap(~ FacetLabel, scales = "free_x", nrow = 2, ncol = 7) +
  labs(
    title = "Player Pro-Social Touch Involvement by Team",
    subtitle = "Bars ordered from highest to lowest involvement within each team; n = # of players per team",
    x = "Each Bar is One Player's Touch Involvement",
    y = "Touch Count"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 10),        # Smaller facet strip labels
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    text = element_text(size = 12)
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
library(dplyr)
library(ggplot2)
library(ggridges)

# Step 1: Keep top 18 players by TotalSecondsPlayed per team
top_players_by_minutes <- player_season_totals %>%
  arrange(TeamID, desc(TotalSecondsPlayed)) %>%
  group_by(TeamID) %>%
  slice_head(n = 18) %>%
  ungroup() %>%
  select(TeamID, Player)

# Step 2: Join with player_counts to keep only those top 18 players per team
player_counts_top18 <- player_counts %>%
  inner_join(top_players_by_minutes, by = c("Team" = "TeamID", "PlayersInvolved" = "Player"))

# Step 3: Calculate team means and relative values
player_counts_mean_top18 <- player_counts_top18 %>%
  group_by(Team) %>%
  mutate(TeamMeanTouch = mean(TouchCount)) %>%
  ungroup() %>%
  mutate(TouchCount_relative = TouchCount - TeamMeanTouch)

# Optional: Reorder teams by average touch for pretty plot ordering
team_order <- player_counts_mean_top18 %>%
  group_by(Team) %>%
  summarise(Average = mean(TouchCount_relative)) %>%
  arrange(desc(Average)) %>%
  pull(Team)

player_counts_mean_top18$Team <- factor(player_counts_mean_top18$Team, levels = team_order)


# Order RankLabel factor levels from R1 to R14
player_counts_mean_top18 <- player_counts_mean_top18 %>%
  mutate(RankLabel = factor(RankLabel, levels = paste0("R", 1:14)))

# Plot
density_top18_mean <- ggplot(player_counts_mean_top18, aes(x = TouchCount_relative, color = RankLabel)) +
  geom_density(size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Player Touch Involvement Relative to Team Mean",
    subtitle = "Includes Only Top 18 players by minutes played per team",
    x = "Touch Count Difference from Team Mean",
    y = "Density",
    color = "Final Rank"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    legend.position = "right",
    legend.key = element_blank(),  # Removes box background
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    text = element_text(size = 12)
  )


#Compare gini coeeficient to final season standings. 

# Compute Gini coefficient for top 18 players per team
player_gini_top18 <- player_counts_top18 %>%
  group_by(Team) %>%
  summarise(Gini = Gini(TouchCount)) %>%
  ungroup()

# Join Gini with final standings
FinalStandings <- FinalStandings %>%
  mutate(TeamID = str_trim(as.character(TeamID)))

player_gini_top18 <- player_gini_top18 %>%
  mutate(Team = str_trim(as.character(Team)))

gini_with_rank_top18 <- player_gini_top18 %>%
  inner_join(FinalStandings, by = c("Team" = "TeamID"))

# Plot Gini vs Final Rank
gini_rank_plot_top18 <- ggplot(gini_with_rank_top18, aes(x = Rank, y = Gini)) +
  geom_point(size = 3, color = "black") +
  geom_text_repel(aes(label = RankLabel), size = 3, family = "Times New Roman", color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linewidth = 1.2) +
  scale_x_reverse(breaks = sort(unique(gini_with_rank_top18$Rank))) +
  labs(
    title = "Season-Level: Team Touch Concentration vs Final Rank",
    subtitle = "Gini calculated using top 18 players by minutes played",
    x = "Final Season Rank (1 = Best)",
    y = "Gini Coefficient"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    text = element_text(size = 12)
  )

# Shapiro-Wilk normality tests
shap_gini <- shapiro.test(gini_with_rank_top18$Gini)

# Pearson correlation
pearson_gini_rank <- cor.test(
  gini_with_rank_top18$Gini,
  gini_with_rank_top18$Rank,
  method = "pearson"
)

# Linear regression
gini_rank_lm <- lm(Rank ~ Gini, data = gini_with_rank_top18)
summary(gini_rank_lm)

