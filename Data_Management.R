# Data_Management
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)

#################################################################
# raters and corresponding sheets
# Rater1 = Paige, Rater2 = Tobi, Rater3 = Simon

Touch_Rater1 <- read_csv("Touch_Paige.csv")
Touch_Rater2 <- read_csv("Touch_Tobi.csv")
#Touch_Rater3 <- read_csv("Touch_Simon.csv")
Touch_Dataframes_List <- list(Touch_Rater1, Touch_Rater2) #list of touch dataframes wanted to be passed into Touches

Touches <- bindrows(Touch_Dataframes_List) #Total touch df

Match_Rater1 <- read_csv("Match_Paige.csv")
Match_Rater2 <- read_csv("Match_Tobi.csv")
#Match_Rater3 <- read_csv("Match_Simon.csv")
Match_Dataframes_List <- list(Math_Rater1, Match_Rater2) #list of match dfs wanted to be passed into Matches

Matches <- bindrows(Match_Dataframes_List) #Total match df

#################################################################

Primary_SeasonOverview <- read_csv("Primary_SeasonOverview.csv")

Team_IDs <- read_csv("TeamIDs.csv")

Stadiums <- read_csv("Stadiums.csv")

MatchAssignments_WatchOrder <- read_csv("MatchAssignments_WatchOrder.csv")

StandingsByWeek <- read_csv("StandingsByWeek_Clean.csv")

FinalStandings <- read_csv("FinalSeasonStandings.csv")

#################################################################

