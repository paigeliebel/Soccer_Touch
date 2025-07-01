# Data Summary and Overview
# Gives Simple overall counts, tables, data make-up etc

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library (dplyr)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("MatchPerformance_Stats_PK.R") 


############################ Complete data summary | No filtering ############################ 

#Create Tables that summarizes complete data of "Touches_final" and "Matches_final"

Touches_Summary <- Touches_final
Matches_Summary <- Matches_final

Total_Touch_Instance_count <- nrow(Touches_Summary) #count total number of touches recorded

Total_Match_Count <- n_distinct(Touches_Summary$SeasonMatchNumber) #total number of matches watched

Total_Teams <- n_distinct(Touches_Summary$Team) #teams recorded

Total_Matches_perTeam <- 26 #Matches each team played (verified below)

Summary_Table_A <- tibble(
  Variable = c("Total_Touch_Instance_count", "Total_Match_Count", "Total_Teams", "Total_Matches_perTeam"),
  Value = c(
    nrow(Touches_Summary),
    n_distinct(Touches_Summary$SeasonMatchNumber),
    n_distinct(Touches_Summary$Team),
    26  # Manually verified
  )
)

############################ FlowChart | Filtering ############################ 

# Prosocial vs Nonsocial

# Prosocial touches are defined as all haptic rituals recorded excluding: 
# Tactical Adjustments, Collisions, and Negative Touch
Exclude_Touch <- c("TA", "CO", "NEG")

#Only Prosocial Touches
Touches_ProSocial <- Touches_final %>%
  filter(!(HapticRitual %in% Exclude_Touch))

#Non-social touches table (only those in Exclude_Touch)
Touches_NonSocial <- Touches_final %>%
  filter(HapticRitual %in% Exclude_Touch)

# Count n for social and nonsocial:
social_count <- nrow(Touches_ProSocial)
nonsocial_count <- nrow(Touches_NonSocial)

cat("Number of Prosocial (Social) Touches:", social_count, "\n")
cat("Number of NonSocial Touches:", nonsocial_count, "\n")

#Excluding goal for/against, substitutions (starting with prosocial set)
Exclude_Situation <- c("GF", "GA", "SUB") 

#Only Prosocial Touches
Touches_CoreData <- Touches_ProSocial %>%
  filter(!(Situation %in% Exclude_Situation))

#Count for other GF/GA/SUB
Touches_GoalsSubs <- Touches_ProSocial %>%
  filter(Situation %in% Exclude_Situation)

# Count n for GF,GA,SUB and Run-of-play:
coredata_count <- nrow(Touches_CoreData)
GoalsSubs_count <- nrow(Touches_GoalsSubs)

#Filter out IT and look at Reciprocity
Exclude_IT <- c("IT")
Touches_ReciprocalNonRecip <- Touches_CoreData %>%
  filter(!(Situation %in% Exclude_IT))

Touches_ReciprocalNonRecip <- Touches_ReciprocalNonRecip %>% 
  mutate(
    Reciprocity = str_trim(Reciprocal) #cleans up white spaces in case
  )

#Define Reciprocity
Touches_ReciprocalNonRecip <- Touches_ReciprocalNonRecip %>%
  mutate(
    Reciprocity_Group = case_when(
      Reciprocity %in% c("Y", "G") ~ "Reciprocal",
      Reciprocity == "N" ~ "NonReciprocal",
      TRUE ~ "Other"   # just in case of other values
    )
  )

# Reciprocal touches table
Reciprocal_Touches <- Touches_ReciprocalNonRecip %>%
  filter(Reciprocity_Group == "Reciprocal")

# Nonreciprocal touches table
NonReciprocal_Touches <- Touches_ReciprocalNonRecip %>%
  filter(Reciprocity_Group == "NonReciprocal")

#Look at IT touches:
# Keep only IT touches
Touches_IT <- Touches_CoreData %>%
  filter(Situation %in% "IT") %>%   # keep only IT
  mutate(
    Reciprocity = str_trim(Reciprocal) # clean white spaces
  ) %>%
  mutate(
    Reciprocity_Group = case_when(
      Reciprocity %in% c("Y", "G") ~ "Reciprocal",
      Reciprocity == "N" ~ "NonReciprocal",
      TRUE ~ "Other"
    )
  )

# Reciprocal touches within IT
Reciprocal_IT_Touches <- Touches_IT %>%
  filter(Reciprocity_Group == "Reciprocal")

# Nonreciprocal touches within IT
NonReciprocal_IT_Touches <- Touches_IT %>%
  filter(Reciprocity_Group == "NonReciprocal")

# Counts
reciprocal_count <- nrow(Reciprocal_IT_Touches)
nonreciprocal_count <- nrow(NonReciprocal_IT_Touches)

#Create a table for all of this: 
library(tibble)

# Summary Table
touch_summary_fowchart <- tibble(
  Step = c(
    "Total Touches",
    "Prosocial Touches",
    "NonSocial Touches",
    "Prosocial: Run of Play",
    "Prosocial: Goals For, Goals Against, Sub",
    "Prosocial, Run of Play, excluding Injury Touch: Reciprocal",
    "Prosocial, Run of Play, excluding Injury Touch: NonReciprocal",
    "Prosocial, Run of Play, only Injury Touch: Reciprocal",
    "Prosocial, Run of Play, only Injury Touch: NonReciprocal"
  ),
  Count = c(
    nrow(Touches_final),
    social_count,
    nonsocial_count,
    coredata_count,
    GoalsSubs_count,
    nrow(Reciprocal_Touches),
    nrow(NonReciprocal_Touches),
    reciprocal_count,
    nonreciprocal_count
  )
)

############### Touches Per Team (Season) #####################

#Cleans up Team Name mispellings
Matches_Summary <- Matches_Summary %>%
  mutate(TeamName = case_when(
    TeamName %in% c("Racing Louisville", "Racing louisville FC", "Louisville Racing") ~ "Racing Louisville FC",
    TeamName %in% c("NC Courage", "Carolina Courage") ~ "North Carolina Courage",
    TeamName %in% c("KC Current") ~ "Kansas City Current",
    TeamName %in% c("Chicago Redstar FC") ~ "Chicago Red Stars",
    TeamName %in% c("Portland Thorns") ~ "Portland Thorns FC",
    TeamName %in% c("Gotham FC") ~ "NJ/NY Gotham FC",
    TeamName %in% c("San Diego Wave") ~ "San Diego Wave FC",
    TeamName %in% c("Seattle Reign") ~ "Seattle Reign FC",
    TRUE ~ TeamName
  ))

TeamMatchCounts <- Matches_Summary %>%
  count(TeamName, name = "MatchesPlayed")

TeamMatchCounts <- TeamMatchCounts %>%
  left_join(Team_IDs, 
            by = c("TeamName" = "Team Name 2024 Season"))

ByTeam_Total_Touch_Instance_count <- Touches_CoreData %>% 
  count(Team, name = "Total Season Touches") %>% 
  rename(TeamID = Team)

TeamMatchCounts <- TeamMatchCounts %>%
  left_join(ByTeam_Total_Touch_Instance_count, 
            by = c("TeamID" = "TeamID"))

######################## Total Season #######################################
Matches_Summary <- Matches_Summary %>%
  mutate(TeamName = case_when(
    TeamName %in% c("Racing Louisville", "Racing louisville FC", "Louisville Racing") ~ "Racing Louisville FC",
    TeamName %in% c("NC Courage", "Carolina Courage") ~ "North Carolina Courage",
    TeamName %in% c("KC Current") ~ "Kansas City Current",
    TeamName %in% c("Chicago Redstar FC") ~ "Chicago Red Stars",
    TeamName %in% c("Portland Thorns") ~ "Portland Thorns FC",
    TeamName %in% c("Gotham FC") ~ "NJ/NY Gotham FC",
    TeamName %in% c("San Diego Wave") ~ "San Diego Wave FC",
    TeamName %in% c("Seattle Reign") ~ "Seattle Reign FC",
    TRUE ~ TeamName
  ))

TeamMatchCounts <- Matches_Summary %>%
  count(TeamName, name = "MatchesPlayed")

TeamMatchCounts <- TeamMatchCounts %>%
  left_join(Team_IDs, 
            by = c("TeamName" = "Team Name 2024 Season"))

# Total, Unfiltered, data 
ByTeam_Total_Touch_Instance_Unfiltered <- Touches_Summary %>% 
  count(Team, name = "Total Season Touches") %>% 
  rename(TeamID = Team)

TeamMatchCounts_Unfiltered <- TeamMatchCounts %>%
  left_join(ByTeam_Total_Touch_Instance_unfiltered, 
            by = c("TeamID" = "TeamID"))

unfiltered <- TeamMatchCounts_Unfiltered %>%
  mutate(Median = median(`Total Season Touches`, na.rm = TRUE),
         AboveMedian = `Total Season Touches` > Median)

unfiltered <- unfiltered %>%
  arrange(`Total Season Touches`) %>%
  mutate(TeamID = factor(TeamID, levels = TeamID))  # preserve the new order

unfiltered_graph <- ggplot(unfiltered, aes(x = TeamID, y = `Total Season Touches`)) +
  geom_col(fill = "gray60") +
  labs(title = "Total Season Touches by Team (Unfiltered)",
       x = "Team (Ordered by Touch Count)",
       y = "Total Season Touches") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

unfiltered_densityplot <- ggplot(unfiltered, aes(x = `Total Season Touches`)) +
  geom_density(fill = "gray60", alpha = 0.7) +
  geom_vline(xintercept = median(unfiltered$`Total Season Touches`, na.rm = TRUE),
             linetype = "dashed", color = "black") +
  labs(title = "Density of Total Season Touches by Team (Unfiltered)",
       x = "Total Season Touches",
       y = "Density") +
  theme_minimal()


# Filtered for Touches_CoreData (Prosocial Touches by team)
ByTeam_Total_Touch_Instance_count <- Touches_CoreData %>% 
  count(Team, name = "Total Season Touches") %>% 
  rename(TeamID = Team)

TeamMatchCounts <- TeamMatchCounts %>%
  left_join(ByTeam_Total_Touch_Instance_count, 
            by = c("TeamID" = "TeamID"))

filtered <- ByTeam_Total_Touch_Instance_count %>%
  arrange(`Total Season Touches`) %>%
  mutate(TeamID = factor(TeamID, levels = TeamID))

filtered_plot <- ggplot(filtered, aes(x = TeamID, y = `Total Season Touches`)) +
  geom_col(fill = "gray60") +
  labs(title = "Total Season Touches by Team (Filtered : Core Data)",
       x = "Team (Ordered by Touch Count)",
       y = "Total Season Touches") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


filtered_densityplot <- ggplot(filtered, aes(x = `Total Season Touches`)) +
  geom_density(fill = "gray60", alpha = 0.7) +
  labs(title = "Density of Total Season Touches by Team (Filtered: Core Data)",
       x = "Total Season Touches",
       y = "Density") +
  theme_minimal()

# Check if filtered data is parametric:

totalseason_parametric <- shapiro.test(filtered$`Total Season Touches`)

# Data is normal! Therefore, parametric.

#Boxplot with both
touch_box_df <- bind_rows(
  unfiltered %>%
    mutate(Type = "Unfiltered") %>%
    select(Type, `Total Season Touches`),
  
  filtered %>%
    mutate(Type = "Filtered") %>%
    select(Type, `Total Season Touches`)
) %>%
  mutate(Type = factor(Type, levels = c("Unfiltered", "Filtered")))


totalseason_boxplot <- ggplot(touch_box_df, aes(x = Type, y = `Total Season Touches`)) +
  geom_boxplot(fill = "gray70", outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, size = 2, color = "gray30", alpha = 0.8) +
  labs(title = "Comparison of Total Season Touches by Team",
       x = "Data Type",
       y = "Total Season Touches") +
  theme_minimal()

#Fantastic.

############### Touches Per Match | Breakdown by Match #############################

#First just Histogram of touch frequency per match across all teams

# Count touches per team per game from CoreHyp data frame
Touches_per_match <- Touches_CoreData %>%
  group_by(Team, SeasonMatchNumber) %>%
  summarise(TouchCount = n(), .groups = "drop")

touches_permatch_plot <- ggplot(Touches_per_match, aes(x = TouchCount)) +
  geom_histogram(binwidth = 5, fill = "gray60", color = "white") +
  labs(
    title = "Distribution of Touches per Game Across All Teams",
    x = "Touches Per Game",
    y = "Number of Matches"
  ) +
  theme_minimal()

#Check normalacy

touches_per_match_parametric <- shapiro.test(Touches_per_match$TouchCount)

#NON-PARAMETIC! Do not use linear regression etc. uuse non-parametric alternatives.

#By team explosion of histograms
FinalStandings <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0")) %>%
  rename(TeamName = Team) 

Touches_per_match_team <- Touches_per_match %>%
  left_join(FinalStandings %>% select(TeamID, TeamName), 
            by = c("Team" = "TeamID")) %>%
  mutate(TeamLabel = paste0(Team, ": ", TeamName))

touches_permatch_plot_teamlevel <- ggplot(Touches_per_match_team, aes(x = TouchCount)) +
  geom_histogram(binwidth = 5, fill = "gray60", color = "white") +
  facet_wrap(~ TeamLabel, scales = "fixed") +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = c(0, 5, 10)  # Only these tick marks shown
  ) +
  labs(
    title = "Touches Per Game by Team (each team played 26 matches)",
    x = "Touches Per Game",
    y = "Number of Matches"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10))

#Ridgeline Plot
#Stacked Density plot for each team
#height of curve is relative frequency of touch
library(ggridges)

touch_permatch_ridgelineplot <- ggplot(Touches_per_match, aes(x = TouchCount, y = reorder(Team, TouchCount, median), fill = Team)) +
  geom_density_ridges(scale = 2, alpha = 0.8, color = "white") +
  labs(
    title = "Touches per Game Distribution by Team",
    x = "Touches per Game",
    y = "Team (Sorted by Median Touches)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Same thing but ordered by final rank
# Visualize with boxplots 
TouchesPerGame_vs_rank <- ggplot(Touches_per_game_ranked, aes(x = Rank, y = TouchCount, group = Rank)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_reverse(breaks = 1:14) +  # clean 1–14 axis
  labs(
    title = "Variation of Within-Team Touch Frequency per Game",
    x = "Team (Ordered by Final Rank)",
    y = "Touches per Game"
  ) +
  theme_minimal()

############### Scaling by team #####################################

# recognizing each team has their own touch culture

#Scaled touch = (touchCount - MedianTouch of team) / IQR of team

Touches_per_match_scaled <- Touches_per_match %>%
  group_by(Team) %>%
  mutate(
    median_touch = median(TouchCount, na.rm = TRUE),
    iqr_touch = IQR(TouchCount, na.rm = TRUE),
    ScaledTouch = (TouchCount - median_touch) / iqr_touch
  ) %>%
  ungroup()

Touches_per_match_scaled_ranked <- Touches_per_match_scaled %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  mutate(
    Rank = as.numeric(Rank),
    Team = factor(Team, levels = FinalStandings$TeamID[order(FinalStandings$Rank)])
  )

touches_permatch_scaled_plot <- ggplot(Touches_per_match_scaled_ranked, aes(x = Team, y = ScaledTouch, group = Team)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  scale_x_discrete(labels = FinalStandings$TeamID[order(FinalStandings$Rank)]) +  # Optional: show TeamIDs on x-axis
  labs(
    title = "Variation of Scaled Touches per Match by Team (Ordered by Final Rank)",
    x = "Team (Ordered by Final Rank)",
    y = "Scaled Touches (Centered and Scaled by IQR)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############### Outliers #####################################

# Check distribution and identify potential outlier

# Prep: Join team rank for ordering
Touches_per_game_ranked <- Touches_per_match %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  filter(!is.na(Rank))



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
Touches_CoreData_Clean <- Touches_CoreData %>%
  semi_join(Touches_filteredoutliers, by = c("Team", "SeasonMatchNumber"))

Touches_by_team_clean <- Touches_CoreData_Clean %>%
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
