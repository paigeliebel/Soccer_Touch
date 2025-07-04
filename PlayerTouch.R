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
source("Overview_Summary_Data.R")
source("MatchPerformance_Stats_PK.R")


#############
# Section A: Prep data
#############

# Correct TotalMatchSeconds computation
Matches_final <- Matches_final %>%
  mutate(
    TeamID = str_sub(MatchID, 1, 2),
    FirstHalfSecs = convert_mmss_to_seconds(as.numeric(FirstHalfLength)),
    SecondHalfSecs = convert_mmss_to_seconds(as.numeric(SecondHalfLength)),
    TotalMatchSeconds = FirstHalfSecs + SecondHalfSecs
  )

# Step 1: Match-by-match seconds played
player_match_seconds <- match_player_entries %>%
  group_by(SeasonMatchNumber, TeamID, Player) %>%
  summarise(
    MatchSecondsPlayed = sum(MatchSecondsPlayed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(SeasonMatchNumber, TeamID, Player)

# Step 2: Players with ≥ match_cutoff matches, only looking at players with a large enough data set a piece
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
    TeamID  = str_sub(MatchID, 1, 2),
    FirstHalfSecs = convert_mmss_to_seconds(as.numeric(FirstHalfLength))
  ) %>%
  select(SeasonMatchNumber, MatchID, TeamID, GoalsInMatchFor, FirstHalfLength, SecondHalfLength, FirstHalfSecs) %>%
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
    TotalSecondsElapsed = case_when(
      Half == 1 ~ (MinuteShown * 60) + Second,
      Half == 2 ~ FirstHalfSecs + ((MinuteShown - 45) * 60) + Second,
      TRUE ~ NA_real_
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

player_match_with_goals <- player_match_with_goals %>%
  left_join(
    Matches_final %>%
      select(SeasonMatchNumber, TeamID, TotalMatchSeconds),
    by = c("SeasonMatchNumber", "TeamID")
  ) %>%
  mutate(PercentGoalTime = FirstScoringTime / TotalMatchSeconds)

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


#############
# Section B: Look at how touch counts change throughout a match
#############

# Looking at total touches across the entire season and all teams and players

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
Touches_PlayerHyp_HalfbyHalf <- Touches_PlayerHyp %>%
  select(-any_of(c(
    "FirstHalfLength", "SecondHalfLength",
    "FirstHalfSeconds", "SecondHalfSeconds",
    "FirstHalfSecs", "SecondHalfSecs"
  )))

# Now safe join
Touches_PlayerHyp_HalfbyHalf <- Touches_PlayerHyp_HalfbyHalf %>%
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
Touches_PlayerHyp_binned_half <- Touches_PlayerHyp_HalfbyHalf %>%
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

######
######
# Is touch associated with scoring or is scoring associated with touch??

# Look until first goal is scored. Is the touch frequency above or below?
# If it is above, then it would suggest that scoring is associated with touch. As in higher than normal touch is indicative of scoring

# Do teams that score the first goal exhibit an above-average touch rate leading up to that goal, compared to their own season baseline?


# If it is below, needs more analysis (maybe dive into how touch looks like whe a team is trailing or ahead)
