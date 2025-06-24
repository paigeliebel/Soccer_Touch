# Player Touch
# Hypothesis: players will have higher prosocial touch rate before their first goal/assist contribution
# For more info on data frames, see README.md

# -----------------------------
# PLAYER TOUCH — MASTER SCRIPT
# Includes:
#   SECTION B — Expanded_v2 (for Role/3/4 plots)
#   SECTION C — Classic Wilcoxon (BeforeGoal vs NoGoalAssist)
#   SECTION D — Role × Context (Faceted)
#   SECTION E — Expanded No-Role (non-facet)
#   SECTION F — Clean 3-Group
#   SECTION G — Clean 4-Group
# Pre-require: Run Core_Hypothesis.R, load Matches + Touches + PlayerMatch
# -----------------------------

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

# --------------
# SECTION B: Expanded Slicing (build Expanded_v2 for both 3/4/role plots)
# --------------

# Step B1: Build ExpandedContext_Label (Expanded_v2)
Touches_PlayerHyp_Expanded_v2 <- player_match_with_goals %>%
  inner_join(Touches_PlayerHyp, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(str_detect(PlayersInvolved, paste0("\\b", PlayerID, "\\b"))) %>%
  mutate(
    TouchRole = case_when(
      Reciprocal %in% c("Y", "G") ~ "Both",
      ToucherNumber == PlayerID ~ "Toucher",
      ToucheeNumber == PlayerID ~ "Touchee",
      TRUE ~ NA_character_
    ),
    ExpandedContext_Label = case_when(
      ScoredOrAssisted == 0 ~ "NoGoalOrAssist",
      ScoredOrAssisted == 1 & TimeInSeconds <= FirstGoalOrAssistTime ~ "BeforeAnyContribution",
      ScoredOrAssisted == 1 & TimeInSeconds > FirstGoalOrAssistTime ~ "AfterFirstContribution",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(TouchRole), !is.na(ExpandedContext_Label))

# Step B1b: Build ExpandedContext_Label 4-Group
Touches_PlayerHyp_Expanded_v2_4group <- player_match_with_goals %>%
  inner_join(Touches_PlayerHyp, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(str_detect(PlayersInvolved, paste0("\\b", PlayerID, "\\b"))) %>%
  mutate(
    TouchRole = case_when(
      Reciprocal %in% c("Y", "G") ~ "Both",
      ToucherNumber == PlayerID ~ "Toucher",
      ToucheeNumber == PlayerID ~ "Touchee",
      TRUE ~ NA_character_
    ),
    ExpandedContext_Label = case_when(
      (ScoredOrAssisted == 0) ~ "NoGoalOrAssist",
      (Scored == 1 & Assisted == 0 & TimeInSeconds <= FirstScoringTime) ~ "BeforeGoal_OnlyGoal",
      (Scored == 0 & Assisted == 1 & TimeInSeconds <= FirstAssistingTime) ~ "BeforeAssist_OnlyAssist",
      (Scored == 1 & Assisted == 1 & TimeInSeconds <= FirstScoringTime) ~ "BeforeGoal_ScoredAndAssist",
      (Scored == 1 & Assisted == 1 & TimeInSeconds <= FirstAssistingTime) ~ "BeforeAssist_ScoredAndAssist",
      (Scored == 0 & Assisted == 1 & TimeInSeconds > FirstAssistingTime) ~ "NoGoalButAssistPossible",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(TouchRole), !is.na(ExpandedContext_Label))

# Step B2: Per PlayerID / Match Summary
Touches_PlayerHyp_summary_role_expanded <- Touches_PlayerHyp_Expanded_v2 %>%
  group_by(PlayerID, TouchRole, ExpandedContext_Label, SeasonMatchNumber) %>%
  summarise(
    Touches = n(),
    MatchSecondsPlayed = first(MatchSecondsPlayed),
    TouchRate = Touches / (MatchSecondsPlayed / 60),
    .groups = "drop"
  )

# Step B2b: Per PlayerID / Match Summary — 4group
Touches_PlayerHyp_summary_role_expanded_4group <- Touches_PlayerHyp_Expanded_v2_4group %>%
  group_by(PlayerID, TouchRole, ExpandedContext_Label, SeasonMatchNumber) %>%
  summarise(
    Touches = n(),
    MatchSecondsPlayed = first(MatchSecondsPlayed),
    TouchRate = Touches / (MatchSecondsPlayed / 60),
    .groups = "drop"
  )

# Step B3: Aggregate avg TouchRate per player
Touches_PlayerHyp_summary_role_expanded_avg <- Touches_PlayerHyp_summary_role_expanded %>%
  group_by(PlayerID, TouchRole, ExpandedContext_Label) %>%
  summarise(
    Avg_TouchRate = mean(TouchRate, na.rm = TRUE),
    .groups = "drop"
  )

# Step B3b: Aggregate avg TouchRate per player — 4group
Touches_PlayerHyp_summary_role_expanded_avg_4group <- Touches_PlayerHyp_summary_role_expanded_4group %>%
  group_by(PlayerID, TouchRole, ExpandedContext_Label) %>%
  summarise(
    Avg_TouchRate = mean(TouchRate, na.rm = TRUE),
    .groups = "drop"
  )

# --------------
# SECTION C: Classic Wilcoxon test (BeforeGoal vs NoGoalAssist)
# --------------

# Prepare data
Touches_PlayerSummary_long <- Touches_PlayerSummary %>%
  pivot_longer(
    cols = c(Avg_TouchRate_BeforeGoal, Avg_TouchRate_BeforeGoalOrAssist, Avg_TouchRate_NoGoalAssist),
    names_to = "Condition",
    values_to = "TouchRate"
  )

# Boxplot
Plot_Wilcox_Player <- ggplot(Touches_PlayerSummary_long, aes(x = Condition, y = TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  labs(
    title = "Player Prosocial Touch Rates by Match Context",
    x = "Condition",
    y = "Touch Rate (touches per min)"
  ) +
  scale_x_discrete(labels = c(
    "Before Goal",
    "Before Goal or Assist",
    "No Goal/Assist"
  )) +
  theme_minimal(base_size = 14)

# Wilcoxon test pairs
wilcox_test_df <- Touches_PlayerSummary %>%
  filter(!is.na(Avg_TouchRate_BeforeGoal), !is.na(Avg_TouchRate_NoGoalAssist))

wilcox_test_df_test <- wilcox.test(
  wilcox_test_df$Avg_TouchRate_BeforeGoal,
  wilcox_test_df$Avg_TouchRate_NoGoalAssist,
  paired = TRUE
)
print(wilcox_test_df_test)

# --------------
# SECTION D: Role × Context (Faceted)
# --------------

# Add Role_Context
Touches_PlayerHyp_summary_role_expanded_avg <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  mutate(Role_Context = paste(TouchRole, ExpandedContext_Label, sep = "_"))

# Kruskal + Pairwise
kruskal_test_facet <- kruskal.test(Avg_TouchRate ~ Role_Context, data = Touches_PlayerHyp_summary_role_expanded_avg)
print(kruskal_test_facet)

pairwise_facet <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_role_expanded_avg$Avg_TouchRate,
  Touches_PlayerHyp_summary_role_expanded_avg$Role_Context,
  p.adjust.method = "bonferroni"
)
print(pairwise_facet)

# Faceted Plot
sample_sizes <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(Role_Context) %>% summarise(n = n())

Touches_PlayerHyp_summary_role_expanded_avg_plot <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  left_join(sample_sizes, by = "Role_Context") %>%
  mutate(label = paste0(Role_Context, "\n(n=", n, ")"))

Plot_RoleContext <- ggplot(Touches_PlayerHyp_summary_role_expanded_avg_plot, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  labs(
    title = "Prosocial Touch Rates — Role × Context",
    x = "Role and Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14)

# --------------
# SECTION E: Expanded No-Role (non-facet)
# --------------

Touches_PlayerHyp_summary_expanded_noRole <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(PlayerID, ExpandedContext_Label) %>%
  summarise(Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE), .groups = "drop") %>%
  group_by(ExpandedContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(ExpandedContext_Label, "\n(n=", n, ")"))

kruskal_test_exp_noRole <- kruskal.test(Avg_TouchRate ~ ExpandedContext_Label, data = Touches_PlayerHyp_summary_expanded_noRole)
print(kruskal_test_exp_noRole)

pairwise_exp_noRole <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_expanded_noRole$Avg_TouchRate,
  Touches_PlayerHyp_summary_expanded_noRole$ExpandedContext_Label,
  p.adjust.method = "bonferroni"
)
print(pairwise_exp_noRole)

Plot_Expanded_NoRole <- ggplot(Touches_PlayerHyp_summary_expanded_noRole, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  labs(
    title = "Prosocial Touch Rates — Expanded Context (no role)",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14)

# --------------
# SECTION F: Clean 3-Group Plot
# --------------

Touches_PlayerHyp_summary_expanded_noRole_4group <- Touches_PlayerHyp_summary_role_expanded_avg_4group %>%
  mutate(
    CleanContext_Label = case_when(
      ExpandedContext_Label == "BeforeAnyContribution" ~ "BeforeAnyContribution",
      ExpandedContext_Label == "AfterFirstContribution" ~ "AfterFirstContribution",
      ExpandedContext_Label == "NoGoalOrAssist" ~ "NoGoalOrAssist",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(CleanContext_Label)) %>%
  group_by(PlayerID, CleanContext_Label) %>%
  summarise(Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE), .groups = "drop") %>%
  group_by(CleanContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(CleanContext_Label, "\n(n=", n, ")"))

kruskal_test_clean3 <- kruskal.test(Avg_TouchRate ~ CleanContext_Label, data = Touches_PlayerHyp_summary_expanded_noRole_3group)
print(kruskal_test_clean3)

pairwise_clean3 <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_expanded_noRole_3group$Avg_TouchRate,
  Touches_PlayerHyp_summary_expanded_noRole_3group$CleanContext_Label,
  p.adjust.method = "bonferroni"
)
print(pairwise_clean3)

Plot_Clean3 <- ggplot(Touches_PlayerHyp_summary_expanded_noRole_3group, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  labs(
    title = "Prosocial Touch Rates — Clean 3-Group Context",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14)

# --------------
# SECTION G: Clean 4-Group Plot
# --------------

Touches_PlayerHyp_summary_expanded_noRole_4group <- Touches_PlayerHyp_summary_role_expanded_avg_4group %>%
  mutate(
    CleanContext_Label = case_when(
      grepl("^BeforeGoal", ExpandedContext_Label) ~ "BeforeGoal",
      grepl("^BeforeAssist", ExpandedContext_Label) ~ "BeforeAssist",
      ExpandedContext_Label == "NoGoalButAssistPossible" ~ "NoGoalButAssistPossible",
      ExpandedContext_Label == "NoGoalOrAssist" ~ "NoGoalOrAssist",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(CleanContext_Label)) %>%
  group_by(PlayerID, CleanContext_Label) %>%
  summarise(Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE), .groups = "drop") %>%
  group_by(CleanContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(CleanContext_Label, "\n(n=", n, ")"))

kruskal_test_clean4 <- kruskal.test(Avg_TouchRate ~ CleanContext_Label, data = Touches_PlayerHyp_summary_expanded_noRole_4group)
print(kruskal_test_clean4)

pairwise_clean4 <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_expanded_noRole_4group$Avg_TouchRate,
  Touches_PlayerHyp_summary_expanded_noRole_4group$CleanContext_Label,
  p.adjust.method = "bonferroni"
)
print(pairwise_clean4)

Plot_Clean4 <- ggplot(Touches_PlayerHyp_summary_expanded_noRole_4group, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  labs(
    title = "Prosocial Touch Rates — Clean 4-Group Context",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14)

