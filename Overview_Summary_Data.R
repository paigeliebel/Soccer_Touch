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
source("Core_Hypothesis.R") #Runs and brings in data frames from Core_Hypothesis.R script

############################ Complete data summary | No filtering ############################ 

#Create Tables that summarizes complete data of "Touches_final" and "Matches_final"

Touches_Summary <- Touches_final
Matches_Summary <- Matches_final

Total_Touch_Instance_count <- nrow(Touches_Summary) #count total number of touches recorded

Total_Match_Count <- n_distinct(Touches_Summary$SeasonMatchNumber) #total number of matches watched

Total_Teams <- n_distinct(Touches_Summary$Team) #teams recorded

Total_Matches_perTeam <- 26 #Matches each team played (verified below)

Summary_Table <- tibble(
  Variable = c("Total_Touch_Instance_count", "Total_Match_Count", "Total_Teams", "Total_Matches_perTeam"),
  Value = c(
    nrow(Touches_Summary),
    n_distinct(Touches_Summary$SeasonMatchNumber),
    n_distinct(Touches_Summary$Team),
    26  # Manually verified
  )
)

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

Summary_Table <- tibble(
  Variable = c("Total_Touch_Instance_count", "Filtered_Touch_Instance_count", "Total_Match_Count", "Total_Teams", "Total_Matches_perTeam"),
  Value = c(
    Total_Touch_Instance_count,
    Filtered_Touch_Instance_count,
    Total_Match_Count,
    Total_Teams,
    Total_Matches_perTeam
  )
)

