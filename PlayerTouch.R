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
library(purrr)
library(patchwork)



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

# Bin data into 1% bins
Touches_PlayerHyp_binned <- Touches_PlayerHyp %>%
  mutate(PercentBin = floor(PercentOfMatch / 0.01) * 0.01) %>%  # bins of 1%
  group_by(PercentBin) %>%
  summarise(TouchCount = n(), .groups = "drop")

# Plot: Histogram with percent of match completion (1% bin)
Histogram_TouchesTime_PercentTime <- ggplot(Touches_PlayerHyp_binned, aes(x = PercentBin, y = TouchCount)) +
  geom_col(fill = "steelblue", color = "black", width = 0.01) +
  labs(title = "Touches by % of Match Completion",
       x = "% of Match Completion",
       y = "Number of Touches") +
  theme_minimal()

# Plot histogram + smooth trend line (LOESS)
histogram_smoothline <- ggplot(Touches_PlayerHyp_binned, aes(x = PercentBin, y = TouchCount)) +
  geom_col(fill = "steelblue", color = "black", width = 0.01) +  # Histogram bars
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 1.2, span = 0.3) +  # Smooth line
  labs(title = "Total Match Touches from both Teams by % of Match Completion (Smoothed via LOESS)",
       x = "% of Match Completion",
       y = "Number of Touches") +
  theme_minimal()

#Plots by outcome, W, D, L
# Merge outcome with touch data
touches_with_outcome <- Touches_PlayerHyp %>%
  left_join(Matches_finalID %>% select(SeasonMatchNumber, TeamID, Outcome),
            by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(!is.na(Outcome), !is.na(PercentOfMatch))  # Filter to valid rows

# Bin percent of match into 1% intervals
touches_binned <- touches_with_outcome %>%
  mutate(PercentBin = floor(PercentOfMatch / 0.01) * 0.01) %>%
  group_by(Outcome, PercentBin) %>%
  summarise(TouchCount = n(), .groups = "drop")

## Filter to Wins and Losses only
touches_binned_filtered <- touches_binned %>%
  filter(Outcome %in% c("W", "L"))

# Count sample sizes (number of touches per outcome)
n_counts <- touches_binned_filtered %>%
  group_by(Outcome) %>%
  summarise(TotalTouches = sum(TouchCount), .groups = "drop")

# Extract n values
n_win <- n_counts %>% filter(Outcome == "W") %>% pull(TotalTouches)
n_loss <- n_counts %>% filter(Outcome == "L") %>% pull(TotalTouches)


######
# Is touch associated with scoring or is scoring associated with touch??

# Look until first goal is scored. Is the touch frequency above or below?
# If it is above, then it would suggest that scoring is associated with touch. As in higher than normal touch is indicative of scoring

# Do teams that score the first goal exhibit an above-average touch rate leading up to that goal, compared to their own season baseline?

# If it is below, needs more analysis (maybe dive into how touch looks like when a team is trailing or ahead)

######
# _Hypothesis:_ Teams that score the first goal of a match exhibit a higher touch count (up to that point) than their average in matches where they did not score first.

# First, get first goal time as decimal
first_goals_with_bins <- first_goals %>%
  mutate(
    FirstGoalBin = floor(PercentOfMatch * 100),
    FirstGoalDecimal = FirstGoalBin / 100
  ) %>%
  select(SeasonMatchNumber, TeamID, FirstGoalDecimal)

# Calculate average time of first goal (as % of match)
avg_first_goal_percent <- first_goals_with_bins %>%
  summarise(AverageFirstGoal = mean(FirstGoalDecimal, na.rm = TRUE)) %>%
  pull(AverageFirstGoal)

# Count actual touches up to the goal
touch_counts <- Touches_PlayerHyp %>%
  inner_join(first_goals_with_bins, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(PercentOfMatch <= FirstGoalDecimal) %>%
  group_by(SeasonMatchNumber, TeamID, FirstGoalDecimal) %>%
  summarise(TouchesUpToGoal = n(), .groups = "drop")

# Ensure all first-scoring matches are retained, with 0s where no touches
touches_up_to_first_goal <- first_goals_with_bins %>%
  left_join(touch_counts, by = c("SeasonMatchNumber", "TeamID", "FirstGoalDecimal")) %>%
  mutate(TouchesUpToGoal = replace_na(TouchesUpToGoal, 0))


# For each row in touches_up_to_first_goal, compute their baseline from other matches
baseline_touch_counts <- touches_up_to_first_goal %>%
  mutate(FirstScoringMatch = SeasonMatchNumber) %>%
  pmap_dfr(function(FirstScoringMatch, TeamID_focal, FirstGoalDecimal, TouchesUpToGoal, ...) {
    cutoff <- FirstGoalDecimal
    
    # Filter same team, different match, did not score first
    other_matches <- Touches_PlayerHyp %>%
      filter(TeamID == TeamID_focal, SeasonMatchNumber != FirstScoringMatch) %>%
      anti_join(first_goals_with_bins, by = c("SeasonMatchNumber", "TeamID")) %>%
      filter(PercentOfMatch <= cutoff) %>%
      group_by(SeasonMatchNumber) %>%
      summarise(TouchesToCutoff = n(), .groups = "drop")
    
    tibble(
      SeasonMatchNumber = FirstScoringMatch,
      TeamID = TeamID_focal,
      FirstGoalDecimal = cutoff,
      TouchesUpToGoal = TouchesUpToGoal,
      BaselineTouchesToCutoff = mean(other_matches$TouchesToCutoff, na.rm = TRUE)
    )
  })


baseline_touch_counts <- baseline_touch_counts %>%
  mutate(Diff = TouchesUpToGoal - BaselineTouchesToCutoff)

baselinetouch_plot <- ggplot(baseline_touch_counts, aes(x = Diff)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Difference in Touches Before First Goal vs. Baseline",
    x = "(TouchesUpToGoal - BaselineTouchesToSameTimeAsGoalScored)",
    y = "Match Count"
  ) +
  theme_minimal()

# Test: are first-goal touches higher than baseline?
wilcox.test(baseline_touch_counts$Diff, mu = 0, alternative = "greater")

#Not significant

#lets look at match outcome:

combined_plot <- ggplot(touches_binned_filtered, aes(x = PercentBin, y = TouchCount, color = Outcome)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.7, size = 1.2) +
  geom_vline(xintercept = avg_first_goal_percent, linetype = "dashed", color = "black") +
  labs(
    title = paste0("Touch Frequency over the Course of Match \n (Dashed Line = Avg 1st Goal Occurs at ", round(avg_first_goal_percent * 100), "%)"),
    x = "% of Match Completion",
    y = "Touch Count",
    color = "Match Outcome"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("W" = "blue", "L" = "red"))

# Refined Hypothesis: Matches that end in wins are characterized by higher touch rates early in the match compared to matches that end in losses.

# Outcome Level Aggregate across the full season

early_cutoff <- avg_first_goal_percent

early_match_summary <- Touches_PlayerHyp %>%
  filter(PercentOfMatch <= early_cutoff) %>%
  left_join(Matches_finalID %>% select(SeasonMatchNumber, TeamID, Outcome),
            by = c("SeasonMatchNumber", "TeamID")) %>%
  group_by(SeasonMatchNumber, Outcome) %>%
  summarise(EarlyTouches = n(), .groups = "drop") %>%
  filter(Outcome %in% c("W", "L"))


wilcox.test(EarlyTouches ~ Outcome, data = early_match_summary)

#Adjusted to just 0.4 completion and wow significant. Touch is more tied to winning than winning is to touch.
#This aligns with a reverse association: once a team begins to gain the upper hand (possession, control, confidence), they touch more — even before the goal, but closer to it in time.

#Test Across Cutoffs: 

test_across_cutoffs <- function(cutoffs) {
  map_dfr(cutoffs, function(cutoff) {
    temp_summary <- Touches_PlayerHyp %>%
      filter(PercentOfMatch <= cutoff) %>%
      left_join(Matches_finalID %>% select(SeasonMatchNumber, TeamID, Outcome),
                by = c("SeasonMatchNumber", "TeamID")) %>%
      group_by(SeasonMatchNumber, Outcome) %>%
      summarise(EarlyTouches = n(), .groups = "drop") %>%
      filter(Outcome %in% c("W", "L"))
    
    test <- wilcox.test(EarlyTouches ~ Outcome, data = temp_summary)
    
    tibble(
      Cutoff = cutoff,
      P_Value = test$p.value
    )
  })
}

cutoff_seq <- seq(0.1, 0.6, by = 0.01)
pval_results <- test_across_cutoffs(cutoff_seq)

# Find the first cutoff where p-value < 0.05
signif_cutoff <- pval_results %>%
  filter(P_Value < 0.05) %>%
  slice(1)  # First significant entry

# Plot with vertical line and annotation
pval_plot <- ggplot(pval_results, aes(x = Cutoff, y = P_Value)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  geom_vline(xintercept = signif_cutoff$Cutoff, linetype = "dotted", color = "darkgreen") +
  annotate("text", x = signif_cutoff$Cutoff + 0.01, y = 0.5,
           label = paste0("Significant at ", signif_cutoff$Cutoff * 100, "% (p-value < 0.05"),
           color = "darkgreen", angle = 90, hjust = 0) +
  labs(
    title = "P-Value of Early Touch Differences Across Match Time Cutoffs",
    x = "Match Completion Cutoff",
    y = "P-Value"
  ) +
  theme_minimal()


# Winning matches are characterized by increasing touch activity as the match unfolds — not from the start, but starting around one-third into the game.
# 30% coincides with that perfectly. Therefore, it seems like uptick in touch after the first goal event (note that this does not say that it is the team who scored) is associated to winning. 

# If a team scores the first goal, does their subsequent touch behavior differ depending on whether they go on to win or lose the match?

first_goal_with_outcome <- first_goals_with_bins %>%
  left_join(Matches_finalID %>% select(SeasonMatchNumber, TeamID, Outcome),
            by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(Outcome %in% c("W", "L"))  # exclude draws for now

touches_after_first_goal <- Touches_PlayerHyp %>%
  inner_join(first_goal_with_outcome, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(PercentOfMatch > FirstGoalDecimal) %>%  # post-goal touches
  group_by(SeasonMatchNumber, TeamID, Outcome) %>%
  summarise(PostGoalTouches = n(), .groups = "drop")

# Boxplot
boxplot_postfirstgoal <- ggplot(touches_after_first_goal, aes(x = Outcome, y = PostGoalTouches, fill = Outcome)) +
  geom_boxplot() +
  labs(
    title = "Post-First-Goal Touches by Match Outcome",
    subtitle = "Among teams that scored first",
    x = "Match Outcome",
    y = "Post-Goal Touch Count"
  ) +
  theme_minimal()

wilcox.test(PostGoalTouches ~ Outcome, data = touches_after_first_goal)

first_goal_with_group <- first_goal_with_outcome %>%
  mutate(Group = ifelse(Outcome == "W", "ScoredFirst_Won", "ScoredFirst_Lost"))

touches_with_group <- Touches_PlayerHyp %>%
  inner_join(first_goal_with_group %>% select(SeasonMatchNumber, TeamID, Group, FirstGoalDecimal),
             by = c("SeasonMatchNumber", "TeamID"))

touches_binned_grouped_norm <- touches_with_group %>%
  mutate(PercentBin = floor(PercentOfMatch / 0.01) * 0.01) %>%
  group_by(SeasonMatchNumber, TeamID, Group, PercentBin) %>%
  summarise(TouchCount = n(), .groups = "drop")

touches_binned_grouped_avg <- touches_binned_grouped_norm %>%
  group_by(Group, PercentBin) %>%
  summarise(AvgTouchPerMatch = mean(TouchCount), .groups = "drop")

avg_first_goal_by_group <- touches_with_group %>%
  distinct(SeasonMatchNumber, TeamID, Group, FirstGoalDecimal) %>%
  group_by(Group) %>%
  summarise(AvgFirstGoal = mean(FirstGoalDecimal, na.rm = TRUE))

ggplot(touches_binned_grouped_avg, aes(x = PercentBin, y = AvgTouchPerMatch, color = Group)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.7, size = 1.2) +
  labs(
    title = "Average Touch Rate for Teams that Scored First",
    subtitle = "ScoredFirst_Won (n = 131) & ScoredFirst_Lost (n = 18)",
    x = "% of Match Completion",
    y = "Avg Touches per Match (per bin)",
    color = "Outcome Group"
  ) +
  scale_color_manual(values = c("ScoredFirst_Won" = "blue", "ScoredFirst_Lost" = "red")) +
  theme_minimal() +
  geom_vline(data = avg_first_goal_by_group,
             aes(xintercept = AvgFirstGoal, color = Group),
             linetype = "dashed", size = 1) +
  geom_text(data = avg_first_goal_by_group,
            aes(x = AvgFirstGoal, y = Inf,
                label = paste0("Avg Goal: ", round(AvgFirstGoal, 2))),
            angle = 90, vjust = -0.5, hjust = 1.1, size = 3,
            inherit.aes = FALSE)




first_goal_outcomes_summary <- first_goal_with_outcome %>%
  count(Outcome) %>%
  mutate(Percent = round(100 * n / sum(n), 1))

first_goal_outcomes_summary
