#Cleaning up and Extracting Info from the Match_Performance Sheet
#Goal to get:
#Minutes Played for each player
#Minutes played by each team
#Penalty Kick Info

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
library(tidyr)
library(purrr)


source("Data_Management.R") #Runs and brings in Matches_final from Data_Management.R script

############################ Overall Team Data ############################
#Info Team by Team
Matches_ID <- Matches_final %>%
  mutate(
    MatchID = str_pad(MatchID, width = 4, pad = "0"),  # in case it was shortened
    TeamID = str_sub(MatchID, 1, 2),
    Substitutes = str_replace_all(Substitutes, "\\.", ",")
  )

team_minutesplayed <- Matches_ID %>% 
  mutate(
    MatchLength = as.numeric(MatchLength)  # Convert to numeric
  ) %>%
  group_by(TeamID) %>%
    summarise(
      TotalMinutesPlayed = sum(MatchLength, na.rm = TRUE),
      MatchCount = n()
    ) %>%
    arrange(desc(TotalMinutesPlayed))

#Formations teams played
formation_counts <- Matches_ID %>%
  group_by(TeamID, Formation) %>%
  summarise(FormationCount = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Formation,
    values_from = FormationCount,
    values_fill = 0  # Fill in 0 if a team never used a certain formation
  )

############################ Overall League Data ############################

#Most Popular Formations
popular_formations <- Matches_ID %>%
  group_by(Formation) %>%
  summarise(TimesUsed = n(), .groups = "drop") %>%
  arrange(desc(TimesUsed))

############################ Player Data ############################

#Determine how many minutes each player played

#First determine the starters across the season for each team
starter_players <- Matches_ID %>%
  mutate(StarterList = str_split(Starters, ",\\s*")) %>%   # split by comma, trim spaces
  rowwise() %>%
  mutate(
    StarterCount = length(StarterList),
    StarterFlag = ifelse(StarterCount != 11, TRUE, FALSE)  # flag if not 11
  ) %>%
  ungroup() %>%
  select(TeamID, StarterList, StarterFlag, SeasonMatchNumber, FirstHalfLength, SecondHalfLength) %>%
  unnest(StarterList) %>%
  mutate(StarterList = str_trim(StarterList))  # clean whitespace

unique_team_players <- starter_players %>%
  mutate(StarterList = str_trim(StarterList)) %>%
  distinct(TeamID, StarterList) %>%
  rename(Player = StarterList)

flagged_starter_matches <- Matches_ID %>%
  mutate(
    StarterList = str_split(Starters, ",\\s*"),
    StarterCount = lengths(StarterList)
  ) %>%
  filter(StarterCount != 11) %>%
  select(SeasonMatchNumber, FirstHalfLength, SecondHalfLength, TeamID, StarterCount, Starters)

#Determine the Subs and add them to the starters
subs_exploded <- Matches_ID %>%
  select(SeasonMatchNumber, FirstHalfLength, SecondHalfLength, TeamID, Substitutes) %>%
  filter(!is.na(Substitutes) & Substitutes != "") %>%
  mutate(
    Substitutes = str_split(Substitutes, ",\\s*")) %>%
  unnest(Substitutes)

#Parse the substitution strings into their parts
subs_parsed <- subs_exploded %>%
  filter(str_detect(Substitutes, "^\\d{8}$")) %>%  # Keep only well-formed subs
  mutate(
    SubIn   = str_sub(Substitutes, 1, 2),
    SubOut  = str_sub(Substitutes, 3, 4),
    Half    = str_sub(Substitutes, 5, 5),
    Minute  = as.numeric(str_sub(Substitutes, 6, 8))
  ) %>%
  select(SeasonMatchNumber, FirstHalfLength, SecondHalfLength, TeamID, Substitutes, SubIn, SubOut, Half, Minute)

#Sub issues to flag
sub_issues <- Matches_ID %>%
  mutate(SeasonMatchNumber = as.character(SeasonMatchNumber)) %>%
  separate_rows(Substitutes, sep = ",\\s*") %>%
  filter(Substitutes != "") %>%
  mutate(Flag = !str_detect(Substitutes, "^\\d{8}$")) %>%  # Expect 8 digits exactly
  filter(Flag) %>%
  select(SeasonMatchNumber, Substitutes)

# Now filter out bad sub entries (those not 8 digits)
subs_parsed_clean <- subs_parsed %>%
  anti_join(sub_issues, by = c("SeasonMatchNumber", "Substitutes"))

# Get all distinct subbed-in players per team
subbed_in_players <- subs_parsed_clean %>%
  select(TeamID, SubIn) %>%
  distinct() %>%
  rename(Player = SubIn)

unique_sub_players <- subs_parsed_clean %>%
  mutate(SubIn = str_trim(SubIn)) %>%                 # <- trim here too
  distinct(TeamID, SubIn) %>%
  rename(Player = SubIn)

# Combine starters and subbed-in players
all_unique_team_players <- bind_rows(unique_team_players, unique_sub_players) %>%
  mutate(
    Player = str_trim(Player),
    PlayerCharLength = nchar(Player)
  ) %>%
  distinct(TeamID, Player, PlayerCharLength)

#Give a complete list of each jersey number that shows up for a team
# Group players by team and nest them into a list column
team_player_lists <- all_unique_team_players %>%
  mutate(Player = as.character(str_trim(Player))) %>%  # Clean and ensure characters
  group_by(TeamID) %>%
  summarise(
    PlayerList = paste(sort(unique(Player)), collapse = ", "),
    .groups = "drop"
  )

#Now Let's figure out minutes played for starters who were not subbed out in a match

# From starter_players: select and rename StarterList to Player
starter_players_long <- starter_players %>%
  select(SeasonMatchNumber, TeamID, StarterList) %>%
  rename(Player = StarterList)

# From subs_parsed_clean: select and rename SubIn to Player
subins_long <- subs_parsed_clean %>%
  select(SeasonMatchNumber, TeamID, SubIn) %>%
  rename(Player = SubIn)

# Combine both into one dataframe, keep distinct values
match_player_entries <- bind_rows(starter_players_long, subins_long) %>%
  mutate(Player = str_trim(Player)) %>%   # Clean whitespace just in case
  distinct()

#pure iteration row by row (inefficient) to see fi player was subbed in/out
match_player_entries <- match_player_entries %>% 
  mutate(
    WasPlayerSubbedIn = pmap_chr(
      list(SeasonMatchNumber, TeamID, Player),
      ~ if_else(
        any(subs_parsed_clean$SeasonMatchNumber == ..1 &
              subs_parsed_clean$TeamID == ..2 &
              subs_parsed_clean$SubIn == ..3),
        "Y", "N"
      )
    )
  )

match_player_entries <- match_player_entries %>%
  mutate(
    WasPlayerSubbedOut = pmap_chr(
      list(SeasonMatchNumber, TeamID, Player),
      ~ if_else(
        any(subs_parsed_clean$SeasonMatchNumber == ..1 &
              subs_parsed_clean$TeamID == ..2 &
              subs_parsed_clean$SubOut == ..3),
        "Y", "N"
      )
    )
  )

Matches_ID_unique <- Matches_ID %>%
  mutate(SeasonMatchNumber = as.character(SeasonMatchNumber)) %>%
  select(SeasonMatchNumber, TeamID, FirstHalfLength, SecondHalfLength) %>%
  distinct()

# Now join safely by both SeasonMatchNumber and TeamID
match_player_entries <- match_player_entries %>%
  mutate(SeasonMatchNumber = as.character(SeasonMatchNumber)) %>%
  left_join(Matches_ID_unique, by = c("SeasonMatchNumber", "TeamID"))

# First, extract only relevant sub-in/out info
sub_in_info <- subs_parsed_clean %>%
  select(SeasonMatchNumber, TeamID, Player = SubIn, SubbedInHalf = Half, SubbedInMinute = Minute)
  
sub_out_info <- subs_parsed_clean %>%
  select(SeasonMatchNumber, TeamID, Player = SubOut, SubbedOutHalf = Half, SubbedOutMinute = Minute)
  
# Now, join it to match_player_metrics for only those who were subbed in/out
match_player_entries <- match_player_entries %>%
  left_join(sub_in_info, by = c("SeasonMatchNumber", "TeamID", "Player"))

match_player_entries <- match_player_entries %>%
  left_join(sub_out_info, by = c("SeasonMatchNumber", "TeamID", "Player"))

#create a function to turn everything into seconds
convert_mmss_to_seconds <- function(mmss) {
  mm <- floor(mmss / 100)
  ss <- mmss %% 100
  mm * 60 + ss
}

#Convert everything to numeric
match_player_entries <- match_player_entries %>%
  mutate(
    FirstHalfLength = as.numeric(FirstHalfLength),
    SecondHalfLength = as.numeric(SecondHalfLength),
    SubbedInMinute = as.numeric(SubbedInMinute),
    SubbedOutMinute = as.numeric(SubbedOutMinute)
  )

# Apply to your data frame
match_player_entries <- match_player_entries %>%
  mutate(
    FirstHalfSeconds = convert_mmss_to_seconds(FirstHalfLength),
    SecondHalfSeconds = convert_mmss_to_seconds(SecondHalfLength)
  )

#Let's do math to figure out how many seconds each player played
match_player_entries <- match_player_entries %>%
  mutate(
    SubInOutConflict = if_else(
      WasPlayerSubbedIn == "Y" & WasPlayerSubbedOut == "Y",
      TRUE, FALSE
    )
  ) %>% 
  mutate(
    MatchSecondsPlayed = case_when(
      # Not subbed in and not subbed out → full match
      WasPlayerSubbedIn == "N" & WasPlayerSubbedOut == "N" ~ (FirstHalfSeconds + SecondHalfSeconds),
      
      # Not subbed in and subbed out in 1st half
      WasPlayerSubbedIn == "N" & WasPlayerSubbedOut == "Y" & SubbedOutHalf == "1" ~
        if_else(SubbedOutMinute == 46, FirstHalfSeconds, (SubbedOutMinute * 60)),
      
      # Not subbed in and subbed out in 2nd half
      WasPlayerSubbedIn == "N" & WasPlayerSubbedOut == "Y" & SubbedOutHalf == "2" ~
        if_else(SubbedOutMinute == 46,
                FirstHalfSeconds,
                (FirstHalfSeconds + ((SubbedOutMinute - 45)*60))),
      
      # Subbed in and subbed in 1st half
      WasPlayerSubbedIn == "Y" & WasPlayerSubbedOut == "N" & SubbedInHalf == "1" ~
        if_else(SubbedInMinute == 46, convert_mmss_to_seconds(SecondHalfLength), 
                (SecondHalfSeconds + (FirstHalfSeconds - (SubbedInMinute*60)))),
      
      # Subbed in and subbed in 2nd half
      WasPlayerSubbedIn == "Y" & WasPlayerSubbedOut == "N" & SubbedInHalf == "2" ~
        if_else(SubbedInMinute == 46, SecondHalfSeconds, 
                (SecondHalfSeconds - ((SubbedInMinute - 45)*60))),
      
      TRUE ~ NA_real_  # For now, mark all other cases (e.g. subbed in) as NA
    )
  )

#Now I have seconds played. Add that up for each player over the season.
player_season_totals <- match_player_entries %>%
  group_by(TeamID, Player) %>%
  summarise(
    TotalSecondsPlayed = sum(MatchSecondsPlayed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(TotalSecondsPlayed))


duplicates <- match_player_entries %>%
  group_by(SeasonMatchNumber, TeamID, Player) %>%
  filter(n() > 1) %>%
  arrange(SeasonMatchNumber, TeamID, Player)



