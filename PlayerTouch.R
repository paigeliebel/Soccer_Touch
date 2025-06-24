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

# Step 2: Players with ≥ match_cutoff matches
match_cutoff <- 5
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
  select(SeasonMatchNumber, MatchID, TeamID, GoalsInMatchFor, FirstHalfLength) %>%
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
    Second = as.numeric(str_sub(Time, 5, 6)),
    TimeInSeconds = case_when(
      Half == 1 ~ (MinuteShown * 60) + Second,
      Half == 2 ~ FirstHalfSeconds + (MinuteShown * 60) + Second,
      TRUE ~ NA_real_
    )
  )

# Step 7a: Touches before first goal (if any)
Touches_BeforeGoal <- player_match_with_goals %>%
  inner_join(Touches_PlayerHyp, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(str_detect(PlayersInvolved, paste0("\\b", PlayerID, "\\b"))) %>%
  filter(!is.na(FirstScoringTime)) %>%
  filter(TimeInSeconds >= SubInTime & TimeInSeconds <= FirstScoringTime)

# Step 7b: Touches before first goal or assist (if any)
Touches_BeforeGoalAssist <- player_match_with_goals %>%
  inner_join(Touches_PlayerHyp, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(str_detect(PlayersInvolved, paste0("\\b", PlayerID, "\\b"))) %>%
  filter(!is.na(FirstGoalOrAssistTime)) %>%
  filter(TimeInSeconds >= SubInTime & TimeInSeconds <= FirstGoalOrAssistTime)

# Step 7c: Touches in matches with no goal or assist at all
Touches_NoGoalAssist <- player_match_with_goals %>%
  filter(MatchHadGoalOrAssist == 0) %>%
  inner_join(Touches_PlayerHyp, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(str_detect(PlayersInvolved, paste0("\\b", PlayerID, "\\b"))) %>%
  filter(TimeInSeconds >= SubInTime)

# Summary Tables
Touches_BeforeGoal_summary <- Touches_BeforeGoal %>%
  group_by(PlayerID, SeasonMatchNumber) %>%
  summarise(
    TouchesBeforeGoal = n(),
    MatchSecondsPlayed = first(MatchSecondsPlayed),
    TouchRateBeforeGoal = TouchesBeforeGoal / (MatchSecondsPlayed / 60),
    .groups = "drop"
  )

Touches_BeforeGoalAssist_summary <- Touches_BeforeGoalAssist %>%
  group_by(PlayerID, SeasonMatchNumber) %>%
  summarise(
    TouchesBeforeGA = n(),
    MatchSecondsPlayed = first(MatchSecondsPlayed),
    TouchRateBeforeGA = TouchesBeforeGA / (MatchSecondsPlayed / 60),
    .groups = "drop"
  )

Touches_NoGoalAssist_summary <- Touches_NoGoalAssist %>%
  group_by(PlayerID, SeasonMatchNumber) %>%
  summarise(
    Touches_NoGA = n(),
    MatchSecondsPlayed = first(MatchSecondsPlayed),
    TouchRate_NoGA = Touches_NoGA / (MatchSecondsPlayed / 60),
    .groups = "drop"
  )

# Final Player-Level Summary Table
Touches_PlayerSummary <- players_with_cutoff_matches %>%
  rename(PlayerID = Player) %>%
  select(PlayerID, TeamID) %>%
  distinct() %>%
  left_join(
    Touches_BeforeGoal_summary %>%
      group_by(PlayerID) %>%
      summarise(Avg_TouchRate_BeforeGoal = mean(TouchRateBeforeGoal, na.rm = TRUE),
                .groups = "drop"),
    by = "PlayerID"
  ) %>%
  left_join(
    Touches_BeforeGoalAssist_summary %>%
      group_by(PlayerID) %>%
      summarise(Avg_TouchRate_BeforeGoalOrAssist = mean(TouchRateBeforeGA, na.rm = TRUE),
                .groups = "drop"),
    by = "PlayerID"
  ) %>%
  left_join(
    Touches_NoGoalAssist_summary %>%
      group_by(PlayerID) %>%
      summarise(Avg_TouchRate_NoGoalAssist = mean(TouchRate_NoGA, na.rm = TRUE),
                .groups = "drop"),
    by = "PlayerID"
  )

#Wilcoxon Test
Touches_PlayerSummary_long <- Touches_PlayerSummary %>%
  pivot_longer(
    cols = c(Avg_TouchRate_BeforeGoal, Avg_TouchRate_BeforeGoalOrAssist, Avg_TouchRate_NoGoalAssist),
    names_to = "Condition",
    values_to = "TouchRate"
  )

Plot_Wilcox_Player <- ggplot(Touches_PlayerSummary_long, aes(x = Condition, y = TouchRate)) +
  geom_boxplot() +
  geom_point(aes(group = PlayerID), alpha = 0.5, position = position_jitter(width = 0.2)) +
  theme_minimal() +
  labs(title = "Player Prosocial Touch Rates by Match Context")

# Filter to players with BOTH values
wilcox_test_df <- Touches_PlayerSummary %>%
  filter(!is.na(Avg_TouchRate_BeforeGoal), !is.na(Avg_TouchRate_NoGoalAssist))

# Wilcoxon Signed-Rank test
wilcox_test_df_test <- wilcox.test(
  wilcox_test_df$Avg_TouchRate_BeforeGoal,
  wilcox_test_df$Avg_TouchRate_NoGoalAssist,
  paired = TRUE,
  alternative = "two.sided"
)

#Other Pairs: BeforeGoalOrAssist vs NoGoalAssist

wilcox_test_df2 <- Touches_PlayerSummary %>%
  filter(!is.na(Avg_TouchRate_BeforeGoalOrAssist), !is.na(Avg_TouchRate_NoGoalAssist))

wilcox_test_df2_test <- wilcox.test(
  wilcox_test_df2$Avg_TouchRate_BeforeGoalOrAssist,
  wilcox_test_df2$Avg_TouchRate_NoGoalAssist,
  paired = TRUE,
  alternative = "two.sided"
)

