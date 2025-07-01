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

cat("Number of Reciprocal IT touches:", reciprocal_count, "\n")
cat("Number of Nonreciprocal IT touches:", nonreciprocal_count, "\n")

###############################################################
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

ByTeam_Total_Touch_Instance_count <- Touches_Summary %>% 
  count(Team, name = "Total Season Touches") %>% 
  rename(TeamID = Team)

TeamMatchCounts <- TeamMatchCounts %>%
  left_join(ByTeam_Total_Touch_Instance_count, 
            by = c("TeamID" = "TeamID"))

############################ Core Hyp data summary | Includes filtering ############################ 

Filtered_Touch_Summary <- Touches_CoreHyp

Filtered_Touch_Instance_count <- nrow(Filtered_Touch_Summary)

#Count of frequency of touches per team
Filtered_Touches_by_team <- Touches_by_team

Filtered_TeamMatchCounts <- TeamMatchCounts %>%
  left_join(Filtered_Touches_by_team, 
            by = c("TeamID" = "Team"))



