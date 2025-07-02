#Reciprocity
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
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Overview_summary_Data.R") #Runs and brings in data frames from Core_Hypothesis.R script
source("MatchPerformance_Stats_PK.R") #Runs abd brings in data frames from MatchPerformance and stats script

#Check to make sure data frames are loaded:
if (!exists("Touches_final") | !exists("Touches_scaled") | !exists("Matches_finalID") | !exists("FinalStandings") | !exists("Touches_CoreData")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded.")
}

############################ Reciprocal/Non-Reciprocal ############################
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

lollipop_season_recip_byteam_plot <- ggplot(Reciprocity_by_Team_filtered %>% filter(DataFlag == "Data Available"), 
       aes(x = reorder(Team, Reciprocal_To_NonRecip_Ratio), y = Reciprocal_To_NonRecip_Ratio)) +
  geom_segment(aes(xend = Team, y = 0, yend = Reciprocal_To_NonRecip_Ratio), color = "gray70") +
  geom_point(color = "steelblue", size = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Reciprocal to Nonreciprocal Touch Ratio by Team (Season Aggregate)",
    x = "Team",
    y = "Reciprocal / Nonreciprocal Touch Ratio"
  ) +
  theme_minimal()

# Join with Final Standings to get rank info
Reciprocal_vs_Rank_filtered <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0")) %>%
  left_join(Reciprocity_by_Team_filtered, by = c("TeamID" = "Team")) %>%
  filter(DataFlag == "Data Available")  # Optional: exclude teams with no data

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

# Not statistically significant, suggesting that the reciprocal/nonreciprocal ratio is linked
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

# ratio

library(dplyr)
library(ggplot2)

# Filter data to exclude GF, GA, SUB situations
filtered_touches_recip_nonrecip <- Touches_ReciprocalNonRecip %>%
  filter(!Situation %in% c("GF", "GA", "SUB")) %>%
  filter(!is.na(Reciprocity_Group))

# Calculate reciprocal and nonreciprocal counts per team per match
touches_per_match_ratio <- filtered_touches_recip_nonrecip %>%
  group_by(Team, SeasonMatchNumber, Reciprocity_Group) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = Reciprocity_Group, values_from = TouchCount, values_fill = 0) %>%
  mutate(Recip_NonRecip_Ratio = Reciprocal / NonReciprocal)

# Join rank info to your per-match data
touches_per_match_ranked <- touches_per_match_ratio %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID"))

# Convert Team to factor ordered by Rank
touches_per_match_ranked <- touches_per_match_ranked %>%
  mutate(Team = factor(Team, levels = FinalStandings %>% arrange(Rank) %>% pull(TeamID)))

# Create factor levels ordered by rank ascending, then reverse them
touches_per_match_ranked <- touches_per_match_ranked %>%
  mutate(Team = factor(Team, levels = rev(FinalStandings %>% arrange(Rank) %>% pull(TeamID))))

# Now plot using this ordered factor
ggplot(touches_per_match_ranked, aes(x = Team, y = Recip_NonRecip_Ratio)) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Per Match Spread of Reciprocal to Nonreciprocal Touch Ratios by Team (Ordered by End of Season Rank)",
    x = "Team (Ordered by Rank)",
    y = "Reciprocal / Nonreciprocal Touch Ratio"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Join with Matches_finalID to get goal differential
touches_ratio_goaldiff <- touches_per_match_ratio %>%
  left_join(Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
            by = c("SeasonMatchNumber", "Team" = "TeamID")) %>%
  filter(!is.na(Recip_NonRecip_Ratio))  # remove NA ratios if any (e.g. NonReciprocal = 0)

# Plot reciprocal:nonreciprocal ratio vs goal differential
ratio_vs_goaldiff_plot <- ggplot(touches_ratio_goaldiff, aes(x = Recip_NonRecip_Ratio, y = GoalDiff)) +
  geom_point(alpha = 0.7, color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Reciprocal to NonReciprocal Touch Ratio vs Goal Differential",
    x = "Reciprocal : NonReciprocal Touch Ratio (Filtered Situations)",
    y = "Goal Differential",
    caption = "Each point = one team in one match"
  ) +
  theme_minimal()

# Spearman correlation test between ratio and goal differential
spearman_ratio_goaldiff <- cor.test(
  touches_ratio_goaldiff$Recip_NonRecip_Ratio,
  touches_ratio_goaldiff$GoalDiff,
  method = "spearman"
)


###With a team profile profile (difference from normal)####

# Season level ratio per team (filtered situations)
season_ratio_by_team <- Touches_ReciprocalNonRecip %>%
  filter(!is.na(Reciprocity_Group)) %>%
  filter(!Situation %in% c("GF", "GA", "SUB")) %>%
  group_by(Team, Reciprocity_Group) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = Reciprocity_Group, values_from = TouchCount, values_fill = 0) %>%
  mutate(Season_Recip_NonRecip_Ratio = Reciprocal / NonReciprocal)

match_ratio_by_team <- Touches_ReciprocalNonRecip %>%
  filter(!Situation %in% c("GF", "GA", "SUB")) %>%
  filter(!is.na(Reciprocity_Group)) %>%
  group_by(Team, SeasonMatchNumber, Reciprocity_Group) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = Reciprocity_Group, values_from = TouchCount, values_fill = 0) %>%
  mutate(Match_Recip_NonRecip_Ratio = Reciprocal / NonReciprocal)

match_vs_season_ratio <- match_ratio_by_team %>%
  left_join(season_ratio_by_team %>% select(Team, Season_Recip_NonRecip_Ratio), by = "Team")

match_vs_season_ratio <- match_vs_season_ratio %>%
  mutate(
    Ratio_Deviation = Match_Recip_NonRecip_Ratio - Season_Recip_NonRecip_Ratio
  )

match_vs_season_ratio <- match_vs_season_ratio %>%
  left_join(Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
            by = c("SeasonMatchNumber", "Team" = "TeamID"))

# Scatter plot with smooth line
ggplot(match_vs_season_ratio, aes(x = Ratio_Deviation, y = GoalDiff)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Deviation of Match vs Season Reciprocal Touch Ratio vs Goal Differential",
    x = "Match-to-Season Reciprocal:NonReciprocal Ratio Deviation",
    y = "Goal Differential"
  ) +
  theme_minimal()

# Spearman correlation test for significance
cor.test(match_vs_season_ratio$Ratio_Deviation, match_vs_season_ratio$GoalDiff, method = "spearman")
