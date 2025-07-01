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

#outlier is 1.5 IQR

Touches_per_match_outliers <- Touches_per_match %>%
  group_by(Team) %>%
  mutate(
    Q1 = quantile(TouchCount, 0.25, na.rm = TRUE),
    Q3 = quantile(TouchCount, 0.75, na.rm = TRUE),
    IQR_val = Q3 - Q1,
    lower_fence = Q1 - 1.5 * IQR_val,
    upper_fence = Q3 + 1.5 * IQR_val,
    OutlierStatus = if_else(TouchCount < lower_fence | TouchCount > upper_fence, "Outlier", "Normal")
  ) %>%
  ungroup()

# Extract TeamID from MatchID in Matches_final
Matches_for_outliers <- Matches_final %>%
  mutate(Team = str_sub(MatchID, 1, 2))  # first two characters as TeamID

# Make sure Team codes are padded the same way
Touches_per_match_outliers <- Touches_per_match_outliers %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0"))

Matches_for_outliers <- Matches_for_outliers %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0"))

# Now join using SeasonMatchNumber and Team
Touches_per_match_outliers <- Touches_per_match_outliers %>%
  left_join(Matches_for_outliers %>% select(SeasonMatchNumber, Team, Outcome, GoalsFor),
            by = c("SeasonMatchNumber", "Team"))

outcome_summary <- Touches_per_match_outliers %>%
  group_by(OutlierStatus, Outcome) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(OutlierStatus) %>%
  mutate(proportion = count / sum(count))

match_outcomes_outliers <- ggplot(outcome_summary, aes(x = OutlierStatus, y = proportion, fill = Outcome)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), position = position_dodge(0.9), vjust = -0.25) +
  labs(
    title = "Match Outcomes by Outlier Status",
    x = "Match Type",
    y = "Proportion"
  ) +
  theme_minimal()

table_outcomes <- table(Touches_per_match_outliers$OutlierStatus, Touches_per_match_outliers$Outcome)

fisher_res <- fisher.test(table_outcomes)

#Extreme touch events seem to propose a different match outcome. Dive into match level analysis.
