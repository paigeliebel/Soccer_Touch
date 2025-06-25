# Player Touch
# Hypothesis: players will have higher prosocial touch rate before their first goal/assist contribution
# For more info on data frames, see README.md

# Libraries
library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library(dplyr)
library(ggsignif)

# Load data and prior processing
source("Data_Management.R") 
source("Core_Hypothesis.R")
source("MatchPerformance_Stats_PK.R")

# Step 1: Match-by-match seconds played
player_match_seconds <- match_player_entries %>%
  group_by(SeasonMatchNumber, TeamID, Player) %>%
  summarise(
    MatchSecondsPlayed = sum(MatchSecondsPlayed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SeasonMatchNumber, TeamID, Player)

# Step 2: Players with ≥ match_cutoff matches, only lokoing at players with a large enough data set a piece
match_cutoff <- 10
players_with_cutoff_matches <- player_match_seconds %>%
  group_by(TeamID, Player) %>%
  summarise(
    NumMatchesPlayed = n_distinct(SeasonMatchNumber),
    .groups = "drop"
  ) %>%
  filter(NumMatchesPlayed >= match_cutoff)

# Step 3: Explode Goals data
goals_exploded <- Matches_final %>%
  mutate(
    MatchID = str_pad(MatchID, width = 4, pad = "0"),
    TeamID  = str_sub(MatchID, 1, 2)
  ) %>%
  select(SeasonMatchNumber, MatchID, TeamID, GoalsInMatchFor, FirstHalfLength, SecondHalfLength) %>%
  filter(!is.na(GoalsInMatchFor) & GoalsInMatchFor != "" & !(GoalsInMatchFor %in% c("X", "XX"))) %>%
  mutate(
    GoalsInMatchFor = str_split(GoalsInMatchFor, ",\\s*")
  ) %>%
  unnest(GoalsInMatchFor) %>%
  mutate(
    GoalsInMatchFor = str_trim(GoalsInMatchFor),
    StringLength = nchar(GoalsInMatchFor)
  ) %>%
  filter(StringLength == 10) %>%
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

# Step 4: Player goal/assist table
player_goal_events <- goals_exploded %>%
  pivot_longer(cols = c(Scorer, Assistor), names_to = "Role", values_to = "Player") %>%
  filter(Player != "XX", Player != "") %>%
  group_by(SeasonMatchNumber, TeamID, Player) %>%
  summarise(
    FirstScoringTime = min(TotalSecondsElapsed[Role == "Scorer"], na.rm = TRUE),
    FirstAssistingTime = min(TotalSecondsElapsed[Role == "Assistor"], na.rm = TRUE),
    NumGoals = sum(Role == "Scorer"),
    NumAssists = sum(Role == "Assistor"),
    .groups = "drop"
  ) %>%
  mutate(
    FirstScoringTime = ifelse(is.infinite(FirstScoringTime), NA, FirstScoringTime),
    FirstAssistingTime = ifelse(is.infinite(FirstAssistingTime), NA, FirstAssistingTime),
    FirstGoalOrAssistTime = pmin(FirstScoringTime, FirstAssistingTime, na.rm = TRUE),
    Scored = if_else(!is.na(FirstScoringTime), 1, 0),
    Assisted = if_else(!is.na(FirstAssistingTime), 1, 0),
    ScoredOrAssisted = if_else(!is.na(FirstGoalOrAssistTime), 1, 0)
  )

# Step 5: Player-match level table
player_match_with_goals <- match_player_entries %>%
  left_join(player_goal_events, by = c("SeasonMatchNumber", "TeamID", "Player")) %>%
  mutate(PlayerID = Player) %>%
  mutate(MatchHadGoalOrAssist = if_else(is.na(ScoredOrAssisted), 0, ScoredOrAssisted))

# Step 6: Touches preparation
Exclude_Touch_Player <- c("TA", "CO", "NEG")
Exclude_Situation_Player <- c("GF", "GA", "SUB")

Touches_PlayerHyp <- Touches_final %>%
  filter(!(HapticRitual %in% Exclude_Touch_Player)) %>%
  filter(!(Situation %in% Exclude_Situation_Player)) %>%
  mutate(TeamID = Team) %>%
  left_join(
    Matches_final %>%
      mutate(TeamID = str_sub(MatchID, 1, 2)) %>%
      select(SeasonMatchNumber, TeamID, FirstHalfLength),
    by = c("SeasonMatchNumber", "TeamID")
  ) %>%
  mutate(
    FirstHalfLength = as.numeric(FirstHalfLength),
    FirstHalfSeconds = convert_mmss_to_seconds(FirstHalfLength),
    Half = as.numeric(str_sub(Time, 1, 1)),
    MinuteShown = as.numeric(str_sub(Time, 2, 4)),
    SecondShown = as.numeric(str_sub(Time, 5, 6)),
    TimeInSeconds = case_when(
      Half == 1 ~ (MinuteShown * 60) + SecondShown,
      Half == 2 ~ FirstHalfSeconds + ((MinuteShown - 45) * 60) + SecondShown,
      TRUE ~ NA_real_
    )
  )

# EXPAND PlayersInvolved: each player gets touch involvement credit
Touches_PlayerHyp <- Touches_PlayerHyp %>%
  separate_rows(PlayersInvolved, sep = ",\\s*") %>%
  mutate(Player = PlayersInvolved)

# Plot: Histogram of touches by match time in 1-min bins
# Doesn't show much because games last longer than others
Histogram_TouchesTime <- ggplot(Touches_PlayerHyp, aes(x = TimeInSeconds / 60)) +  # convert seconds to minutes
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +  # 1-minute bins
  labs(title = "Touches by Match Time (5-Minute Bins)",
       x = "Match Time (minutes)",
       y = "Number of Touches") +
  theme_minimal()

# Adds total match length in seconds and mutates it to have a Percent completion token
Touches_PlayerHyp <- Touches_PlayerHyp %>%
  left_join(
    Matches_final %>%
      mutate(TeamID = str_sub(MatchID, 1, 2),
             MatchLength_Minutes = floor(as.numeric(MatchLength) / 100),
             MatchLength_Seconds = as.numeric(MatchLength) %% 100,
             TotalMatchSeconds = (MatchLength_Minutes * 60) + MatchLength_Seconds) %>%
      select(SeasonMatchNumber, TeamID, TotalMatchSeconds),
    by = c("SeasonMatchNumber", "TeamID")
  ) %>%
  mutate(PercentOfMatch = TimeInSeconds / TotalMatchSeconds)

# Plot: Histogram with percent of match completion (1% bin)
Histogram_TouchesTime_PercentTime <- ggplot(Touches_PlayerHyp, aes(x = PercentOfMatch)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "black") +
  labs(title = "Touches by % of Match Completion",
       x = "% of Match Completion",
       y = "Number of Touches") +
  theme_minimal()

# Step 1: Bin data into 1% bins
Touches_PlayerHyp_binned <- Touches_PlayerHyp %>%
  mutate(PercentBin = floor(PercentOfMatch / 0.01) * 0.01) %>%  # bins of 1%
  group_by(PercentBin) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Step 2: Plot histogram + smooth trend line (LOESS)
histogram_smoothline <- ggplot(Touches_PlayerHyp_binned, aes(x = PercentBin, y = TouchCount)) +
  geom_col(fill = "steelblue", color = "black", width = 0.01) +  # Histogram bars
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2, span = 0.3) +  # Smooth line
  labs(title = "Touches by % of Match Completion (Smoothed via LOESS)",
       x = "% of Match Completion",
       y = "Number of Touches") +
  theme_minimal()

# Half by Half analysis #
# Step 1: compute percent of half completion — FULL CLEAN VERSION
# Remove possible old columns — only if they exist!
Touches_PlayerHyp <- Touches_PlayerHyp %>%
  select(-any_of(c(
    "FirstHalfLength", "SecondHalfLength",
    "FirstHalfSeconds", "SecondHalfSeconds",
    "FirstHalfSecs", "SecondHalfSecs",
    "TotalMatchSeconds", "TotalMatchSecs"
  )))

# Now safe join
Touches_PlayerHyp <- Touches_PlayerHyp %>%
  left_join(
    Matches_final %>%
      mutate(
        TeamID = str_sub(MatchID, 1, 2),
        FirstHalfLength = as.numeric(FirstHalfLength),
        SecondHalfLength = as.numeric(SecondHalfLength),
        FirstHalfSecs = convert_mmss_to_seconds(FirstHalfLength),
        SecondHalfSecs = convert_mmss_to_seconds(SecondHalfLength),
        TotalMatchSecs = FirstHalfSecs + SecondHalfSecs
      ) %>%
      select(
        SeasonMatchNumber, TeamID,
        FirstHalfSecs, SecondHalfSecs, TotalMatchSecs
      ),
    by = c("SeasonMatchNumber", "TeamID")
  ) %>%
  mutate(
    HalfTotalSecs = if_else(Half == 1, FirstHalfSecs, SecondHalfSecs),
    TimeInHalfSecs = if_else(Half == 1, TimeInSeconds, TimeInSeconds - FirstHalfSecs),  # RESET time for 2nd half
    PercentOfHalf = TimeInHalfSecs / HalfTotalSecs,
    HalfLabel = if_else(Half == 1, "First Half", "Second Half")
  )


# Step 2: bin PercentOfHalf into 1% bins
Touches_PlayerHyp_binned_half <- Touches_PlayerHyp %>%
  mutate(PercentHalfBin = floor(PercentOfHalf / 0.01) * 0.01) %>%  # bins of 1%
  group_by(HalfLabel, PercentHalfBin) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Step 3: plot histogram + smooth line per half
histogram_percenthalf <- ggplot(Touches_PlayerHyp_binned_half, aes(x = PercentHalfBin, y = TouchCount)) +
  geom_col(fill = "steelblue", color = "black", width = 0.01) +  # Histogram bars
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2, span = 0.3) +  # Smooth line
  facet_wrap(~ HalfLabel, ncol = 1) +  # Two rows: first half & second half
  labs(title = "Touches by % of Half Completion (Smoothed)",
       x = "% of Half Completion",
       y = "Number of Touches") +
  theme_minimal()

loess_percenthalf_both <- ggplot(Touches_PlayerHyp_binned_half, aes(x = PercentHalfBin, y = TouchCount, color = HalfLabel)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.6, span = 0.5) +  # Smooth lines only
  labs(title = "Touches by % of Half Completion (Smoothed via LOESS, First vs Second Half)",
       x = "% of Half Completion",
       y = "Number of Touches",
       color = "Half") +
  scale_color_manual(values = c("First Half" = "blue", "Second Half" = "red")) +
  theme_minimal()

##### Great. So now we know there is an "uptick" in touches throughout the match
##### Touch rate doesn't appear 'constant' throughout a match. I feel like I can't compare 
##### touch rate before a goal to touch rate over a whole match of no scoring
##### look at total match by match analysis? Players who scored, the rate before scoring,
##### and the rate of the overall touch rate for the match for the player --> too many confounding variables
##### if the player scores, the team is winning. Do we look at them compared to players who don't score? 
##### do we look at average touch rate up to the point of their average goal scoring moment? That seems wrong. 

#Final Direction:
#Hypothesis: "Do players increase their own prosocial touch rate before scoring a goal?"
#Baseline: their normal match rhythm in matches where they did not score
#Target: matches where they did score → before their first goal only

### Create binned datasets for player LOESS curves ###

# First: Add scoring info to each touch
Touches_PlayerHyp <- Touches_PlayerHyp %>%
  left_join(
    player_goal_events %>%
      select(SeasonMatchNumber, TeamID, Player, FirstScoringTime, Scored),
    by = c("SeasonMatchNumber", "TeamID", "Player")
  ) %>%
  mutate(
    IsBeforeGoal = case_when(
      Scored == 1 & TimeInSeconds < FirstScoringTime ~ "Pre-Goal",
      Scored == 0 ~ "Non-Goal Match",
      TRUE ~ NA_character_
    )
  )

# Keep only touches in Pre-Goal or Non-Goal match
Touches_PlayerHyp_filtered <- Touches_PlayerHyp %>%
  filter(!is.na(IsBeforeGoal))

# Bin by PercentOfMatch, per Player and Condition
Touches_PlayerHyp_binned_Player <- Touches_PlayerHyp_filtered %>%
  mutate(PercentBin = floor(PercentOfMatch / 0.01) * 0.01) %>%
  group_by(Player, IsBeforeGoal, PercentBin) %>%
  summarise(
    TouchCount = n(),
    .groups = "drop"
  )

# Aggregate player average per bin
Player_LOESS_Data <- Touches_PlayerHyp_binned_Player %>%
  group_by(IsBeforeGoal, PercentBin) %>%
  summarise(
    MeanTouchCount = mean(TouchCount),
    .groups = "drop"
  )

# Step 8: Plot — smooth player-average curves
ggplot(Player_LOESS_Data, aes(x = PercentBin, y = MeanTouchCount, color = IsBeforeGoal)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.6, span = 0.4) +
  labs(title = "Player-Averaged Prosocial Touch Rate — Pre-Goal vs Non-Goal Matches",
       x = "% of Match Completion",
       y = "Avg Touches per Player (per % bin)",
       color = "Condition") +
  scale_color_manual(values = c("Pre-Goal" = "red", "Non-Goal Match" = "blue")) +
  theme_minimal()

# Step 9 — Player-wise normalized curves

# Re-join match length into Touches_PlayerHyp_filtered
Touches_PlayerHyp_filtered <- Touches_PlayerHyp_filtered %>%
  left_join(
    Matches_final %>%
      mutate(TeamID = str_sub(MatchID, 1, 2),
             MatchLength_Minutes = floor(as.numeric(MatchLength) / 100),
             MatchLength_Seconds = as.numeric(MatchLength) %% 100,
             TotalMatchSeconds = (MatchLength_Minutes * 60) + MatchLength_Seconds) %>%
      select(SeasonMatchNumber, TeamID, TotalMatchSeconds),
    by = c("SeasonMatchNumber", "TeamID")
  ) %>%
  mutate(
    MatchMinutes = TotalMatchSeconds / 60,
    TouchRatePerMin = 1 / MatchMinutes  # this is your calculation
  )

# Bin + normalize per player
PlayerCurves <- Touches_PlayerHyp_filtered %>%
  mutate(PercentBin = floor(PercentOfMatch / 0.01) * 0.01) %>%
  group_by(Player, IsBeforeGoal, PercentBin) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  group_by(Player, IsBeforeGoal) %>%
  mutate(TouchRate = TouchCount / sum(TouchCount)) %>%  # normalize → % of player's touches
  ungroup()

# Step: compute Player-level average touch rate per match

# Add total match duration
Touches_PlayerHyp_filtered <- Touches_PlayerHyp_filtered %>%
  mutate(MatchMinutes = TotalMatchSeconds / 60,
         TouchRatePerMin = 1 / MatchMinutes)  # 1 touch per min (approximation)

# For each player & match & condition — compute touch rate
Player_MatchRates <- Touches_PlayerHyp_filtered %>%
  group_by(Player, SeasonMatchNumber, IsBeforeGoal) %>%
  summarise(
    NumTouches = n(),
    MatchMinutes = first(MatchMinutes),
    TouchRatePerMin = NumTouches / MatchMinutes,
    .groups = "drop"
  )

#Per-player!
# Average across matches — per player
Player_AvgRates <- Player_MatchRates %>%
  group_by(Player, IsBeforeGoal) %>%
  summarise(
    Avg_TouchRatePerMin = mean(TouchRatePerMin),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = IsBeforeGoal,
    values_from = Avg_TouchRatePerMin
  ) %>%
  filter(!is.na(`Pre-Goal`), !is.na(`Non-Goal Match`)) %>%  # only players with BOTH
  mutate(Diff = `Pre-Goal` - `Non-Goal Match`)

# OR non-parametric
wilcox.test(Player_AvgRates$`Pre-Goal`, Player_AvgRates$`Non-Goal Match`, paired = TRUE)
