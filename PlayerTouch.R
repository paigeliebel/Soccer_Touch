# Player Touch
# For more information on these data frames please look at the README.md file

# We hypothesize that pro-social touch may be associated with individual player success. Specifically, we tested whether players demonstrated a 
# higher rate of pro-social touch per minute of play in the period leading up to their first scoring contribution (goal or assist), compared to 
# their own touch rates in matches where they did not contribute to scoring. By comparing players within themselves across matches, this approach 
# helps control for stable player-level factors (e.g., personality, role).

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library (dplyr)

source("Data_Management.R") 
source("Core_Hypothesis.R")
source("MatchPerformance_Stats_PK.R")

# Adjust data frame from the MatchPerformance_Stats_PK.R sheet to include the match by match seconds played
# Below shows seconds played by each player paired with seasonmatchnumber
player_match_seconds <- match_player_entries %>%
  group_by(SeasonMatchNumber, TeamID, Player) %>%
  summarise(
    MatchSecondsPlayed = sum(MatchSecondsPlayed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SeasonMatchNumber, TeamID, Player)

#Filters for players who only played in 10 or more matches throughout the season for the team
players_with_10_matches <- player_match_seconds %>%
  group_by(TeamID, Player) %>%
  summarise(
    NumMatchesPlayed = n_distinct(SeasonMatchNumber),  # count unique matches
    .groups = "drop"
  ) %>%
  filter(NumMatchesPlayed >= 10)  # keep only players with ≥10 matches

#Get when the goals were scored and by who:
goals_exploded <- Matches_final %>%
  mutate(
    MatchID = str_pad(MatchID, width = 4, pad = "0"),
    TeamID = str_sub(MatchID, 1, 2)
  ) %>%
  select(SeasonMatchNumber, MatchID, TeamID, GoalsInMatchFor, FirstHalfLength) %>%
  filter(!is.na(GoalsInMatchFor) & GoalsInMatchFor != "" & !(GoalsInMatchFor %in% c("X", "XX"))) %>%
  mutate(
    GoalsInMatchFor = str_split(GoalsInMatchFor, ",\\s*")
  ) %>%
  unnest(GoalsInMatchFor) %>%
  mutate(
    GoalsInMatchFor = str_trim(GoalsInMatchFor),  # trim spaces!
    StringLength = nchar(GoalsInMatchFor)  # check length
  ) %>%
  filter(StringLength == 10) %>%  # only keep valid strings
  group_by(SeasonMatchNumber, TeamID) %>%
  mutate(
    GoalCount = row_number(),
    Scorer = str_sub(GoalsInMatchFor, 1, 2),
    Assistor = str_sub(GoalsInMatchFor, 3, 4),
    Half = as.numeric(str_sub(GoalsInMatchFor, 5, 5)),
    MinuteShown = as.numeric(str_sub(GoalsInMatchFor, 6, 8)),
    Second = as.numeric(str_sub(GoalsInMatchFor, 9, 10))
  ) %>%
  ungroup() %>%
  mutate(
    TotalSecondsElapsed = if_else(
      Half == 1,
      (MinuteShown * 60) + Second,
      convert_mmss_to_seconds(as.numeric(FirstHalfLength)) + (MinuteShown * 60) + Second
    )
  ) %>%
  filter(Scorer != "OG") %>%
  select(SeasonMatchNumber, TeamID, GoalCount, Scorer, Assistor, TotalSecondsElapsed)


bad_strings <- Matches_final %>%
  mutate(
    GoalsInMatchFor = str_split(GoalsInMatchFor, ",\\s*")
  ) %>%
  unnest(GoalsInMatchFor) %>%
  mutate(
    GoalsInMatchFor = str_trim(GoalsInMatchFor),
    StringLength = nchar(GoalsInMatchFor),
    
    # Break string into visible parts — even if too short
    Scorer   = str_sub(GoalsInMatchFor, 1, 2),
    Assistor = str_sub(GoalsInMatchFor, 3, 4),
    Half     = str_sub(GoalsInMatchFor, 5, 5),
    Minute   = str_sub(GoalsInMatchFor, 6, 8),
    Second   = str_sub(GoalsInMatchFor, 9, 10)
  ) %>%
  filter(!GoalsInMatchFor %in% c("X", "XX"), StringLength != 10) %>%
  select(SeasonMatchNumber, GoalsInMatchFor, StringLength,
         Scorer, Assistor, Half, Minute, Second)

bad_strings %>% count(StringLength)
