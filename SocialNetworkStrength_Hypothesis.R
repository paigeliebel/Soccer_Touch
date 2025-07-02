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


############################ Reciprocal/Non-Reciprocal | Social Network Strength ############################

#Reciprocal vs non-reciprocal touch | simple final standings to ratio of reciprocal/non-reciprocal

Reciprocity_by_Team <- Touches_ReciprocalNonRecip %>%
  filter(!is.na(Reciprocity_Group)) %>%               # Filter out NA if any
  group_by(Team, Reciprocity_Group) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = Reciprocity_Group, values_from = TouchCount, values_fill = 0) %>%
  mutate(
    Reciprocal_To_NonRecip_Ratio = Reciprocal / NonReciprocal,
    DataFlag = if_else(Reciprocal + NonReciprocal == 0, "No Touch Data", "Data Available")
  )

#Join with Final Standings
Reciprocal_vs_Rank <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0")) %>%
  left_join(Reciprocity_by_Team, by = c("TeamID" = "Team"))

#Visualize the reciprocity ratio by team
ratio_histo <- ggplot(Reciprocity_by_Team, aes(x = Reciprocal_To_NonRecip_Ratio)) +
  geom_histogram(bins = 30, fill = "gray", color = "black") +
  labs(title = "Distribution of Reciprocal to NonReciprocal Ratio")

#Plot
Reciprocal_vs_Rank_Plot <- ggplot(Reciprocal_vs_Rank, aes(x = Rank, y = Reciprocal_To_NonRecip_Ratio)) +
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

# check normality --> parametric
ratio_parametric <- shapiro.test(Reciprocity_by_Team$Reciprocal_To_NonRecip_Ratio)

#Stats Sum
ratio_recip_pearson_result <- cor.test(
  Reciprocal_vs_Rank$Reciprocal_To_NonRecip_Ratio,
  Reciprocal_vs_Rank$Rank,
  method = "pearson"
)

#Statistical Significance! Note the use of a ratio, therefore I didn't feel the need to filter for run of play etc

######### What about in Run of Play? When I look at GF, GA, and SUBs excluded? ##########

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Define situations to exclude
exclude_situations <- c("GF", "GA", "SUB")

# Filter and calculate touches by reciprocity group per team (excluding specified situations)
Reciprocity_by_Team_filtered <- Touches_ReciprocalNonRecip %>%
  filter(!is.na(Reciprocity_Group)) %>%
  filter(!Situation %in% exclude_situations) %>%   # exclude GF, GA, SUB
  group_by(Team, Reciprocity_Group) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = Reciprocity_Group, values_from = TouchCount, values_fill = 0) %>%
  mutate(
    Reciprocal_To_NonRecip_Ratio = Reciprocal / NonReciprocal,
    DataFlag = if_else(Reciprocal + NonReciprocal == 0, "No Touch Data", "Data Available")
  )

# Join with Final Standings to get rank info
Reciprocal_vs_Rank_filtered <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0")) %>%
  left_join(Reciprocity_by_Team_filtered, by = c("TeamID" = "Team")) %>%
  filter(DataFlag == "Data Available")  # Optional: exclude teams with no data

# Plot histogram of reciprocal to nonreciprocal ratio
ratio_histo_filtered <- ggplot(Reciprocal_vs_Rank_filtered, aes(x = Reciprocal_To_NonRecip_Ratio)) +
  geom_histogram(bins = 30, fill = "gray70", color = "black") +
  labs(title = "Distribution of Reciprocal to NonReciprocal Ratio (Filtered Situations)",
       x = "Reciprocal to NonReciprocal Ratio",
       y = "Count") +
  theme_minimal()

# Plot reciprocal ratio vs final season rank
Reciprocal_vs_Rank_Plot_filtered <- ggplot(Reciprocal_vs_Rank_filtered, aes(x = Rank, y = Reciprocal_To_NonRecip_Ratio)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  scale_x_reverse(breaks = 1:max(Reciprocal_vs_Rank_filtered$Rank)) +
  labs(
    title = "Reciprocal Touch Ratio vs Final Season Rank per Team (Filtered Situations)",
    subtitle = "Higher ratios may reflect stronger intra-team cohesion",
    caption = "Note: Each dot represents one team",
    x = "Team Final Season Rank (1 = Best)",
    y = "Reciprocal to NonReciprocal Touch Ratio"
  ) +
  theme_minimal()

# Check normality of the ratio
ratio_parametric_filtered <- shapiro.test(Reciprocal_vs_Rank_filtered$Reciprocal_To_NonRecip_Ratio)

# Perform Pearson correlation (parametric)
ratio_recip_pearson_result_filtered <- cor.test(
  Reciprocal_vs_Rank_filtered$Reciprocal_To_NonRecip_Ratio,
  Reciprocal_vs_Rank_filtered$Rank,
  method = "pearson"
)

# No longer statistically significant, suggesting that the reciprocal/nonreciprocal ratio is linked
# to the GF, SUBs, and GA situations. 
# What is going on here? Perhaps GF, GA, and SUB all have high reciprocal touch 
# Step 1 & 2: Summarize counts and proportions by Situation and Reciprocity_Group

library(scales)

# Categorize situations into GF, GA, SUB, or Other
Touches_ReciprocalNonRecip <- Touches_ReciprocalNonRecip %>%
  mutate(
    Situation_Bucket = case_when(
      Situation %in% c("GF", "GA", "SUB") ~ Situation,
      TRUE ~ "Other"
    )
  )

# Summarize counts per Reciprocity_Group and Situation_Bucket
situation_summary_reciprocity <- Touches_ReciprocalNonRecip %>%
  filter(!is.na(Reciprocity_Group)) %>%
  group_by(Reciprocity_Group, Situation_Bucket) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Reciprocity_Group) %>%
  mutate(
    Percent = Count / sum(Count)
  ) %>%
  ungroup()

# Plot grouped bar chart with percent on y-axis
situation_summary_reciprocity_plot <- ggplot(situation_summary_reciprocity, aes(x = Reciprocity_Group, y = Percent, fill = Situation_Bucket)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +  # side-by-side bars
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportion of Situations by Reciprocity Group",
    x = "",
    y = "Percentage of Touches",
    fill = "Situation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#### Okay ya, so the ratio of a teams reciprocity is significantly different when accounting for GF and SUBS.

######## Lets look at reciprocal and non recriprocal totally separately ####
# Sum total touches by team for reciprocal
Recip_by_team <- Reciprocal_Touches %>%
  group_by(Team) %>%
  summarise(Reciprocal = n(), .groups = "drop")

# Sum total touches by team for nonreciprocal
Nonrecip_by_team <- NonReciprocal_Touches %>%
  group_by(Team) %>%
  summarise(NonReciprocal = n(), .groups = "drop")

# Join with FinalStandings (make sure TeamID format matches 'Team')
FinalStandings <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0"))

# Join reciprocal touches with standings
Recip_vs_rank <- FinalStandings %>%
  left_join(Recip_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(Reciprocal)) %>%
  mutate(Rank = as.numeric(Rank))

# Join nonreciprocal touches with standings
NonRecip_vs_rank <- FinalStandings %>%
  left_join(Nonrecip_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(NonReciprocal)) %>%
  mutate(Rank = as.numeric(Rank))

# Correlation: Reciprocal vs Rank
cor_recip <- cor.test(Recip_vs_rank$Reciprocal, Recip_vs_rank$Rank, method = "spearman")
print(cor_recip)

# Correlation: Nonreciprocal vs Rank
cor_nonrecip <- cor.test(NonRecip_vs_rank$NonReciprocal, NonRecip_vs_rank$Rank, method = "spearman")
print(cor_nonrecip)

# Plot Reciprocal touches vs Rank
recip_touch_vs_rank_plot <- ggplot(Recip_vs_rank, aes(x = Rank, y = Reciprocal)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  scale_x_reverse(breaks = 1:max(Recip_vs_rank$Rank)) +
  labs(title = "Reciprocal Touches vs Final Season Rank",
       x = "Final Season Rank (1 = Best)",
       y = "Total Reciprocal Touches") +
  theme_minimal()

# Plot NonReciprocal touches vs Rank
nonrecip_touch_vs_rank_plot <- ggplot(NonRecip_vs_rank, aes(x = Rank, y = NonReciprocal)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_x_reverse(breaks = 1:max(NonRecip_vs_rank$Rank)) +
  labs(title = "NonReciprocal Touches vs Final Season Rank",
       x = "Final Season Rank (1 = Best)",
       y = "Total NonReciprocal Touches") +
  theme_minimal()

#### Reciprocal touches seem meaningfully associated with better performance, while nonrecip touches do not

# GA has such few touches, going to exclude from the scaling
# Scale for "goals for" and subs

# Calculate scaling factors for each team as their goals/subs relative to the league average,
# to normalize for differences in team performance and allow fair comparison of touch metrics.

# Count total substitutions per team
subs_count <- subs_exploded %>%
  group_by(TeamID) %>%
  summarise(TotalSubs = n(), .groups = "drop")

# Extract season goals scored per team
goals_per_team <- FinalStandings %>%
  select(TeamID, SeasonGoalsFor) %>%
  mutate(TeamID = as.character(TeamID))  # Ensure consistent type for join

# Combine substitutions count with goals scored
team_summary_GF_SUB <- subs_count %>%
  full_join(goals_per_team, by = "TeamID") %>%
  mutate(TotalSubs = if_else(is.na(TotalSubs), 0L, TotalSubs))

# Calculate league averages for GoalsFor and Subs
league_averages <- team_summary_GF_SUB %>%
  summarise(
    avg_goals_for = mean(SeasonGoalsFor, na.rm = TRUE),
    avg_subs = mean(TotalSubs, na.rm = TRUE)
  )

# Add scaling factors to each team: team value divided by league average
team_scaled_GF_SUB <- team_summary_GF_SUB %>%
  mutate(
    Scale_GoalsFor = SeasonGoalsFor / league_averages$avg_goals_for,
    Scale_Subs = TotalSubs / league_averages$avg_subs
  )

#creating scaling denominator
team_scaled_GF_SUB <- team_scaled_GF_SUB %>%
  mutate(
    ScalingDenominator = Scale_GoalsFor + Scale_Subs + 1
  )

########################

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

