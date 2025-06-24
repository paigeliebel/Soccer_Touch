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
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  # OPTIONAL: uncomment this if you want to see player-level lines
  # geom_line(aes(group = PlayerID), color = "gray80", alpha = 0.5) +
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
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

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

#################Comparing roles and context

# 1. Create combined label for Role + Context
Touches_PlayerHyp_summary_role_expanded_avg <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  mutate(Role_Context = paste(TouchRole, ExpandedContext_Label, sep = "_"))

# 2. Kruskal-Wallis: does Role+Context affect Avg_TouchRate?
kruskal_test_overall <- kruskal.test(Avg_TouchRate ~ Role_Context, data = Touches_PlayerHyp_summary_role_expanded_avg)
print(kruskal_test_overall)

# 3. Pairwise Wilcoxon test (adjusted p-values)
pairwise_overall <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_role_expanded_avg$Avg_TouchRate,
  Touches_PlayerHyp_summary_role_expanded_avg$Role_Context,
  p.adjust.method = "bonferroni"
)

print(pairwise_overall)


# 4. Boxplot with ggsignif + sample sizes

# Sample sizes:
sample_sizes <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(Role_Context) %>%
  summarise(n = n(), .groups = "drop")

# Merge into data:
Touches_PlayerHyp_summary_role_expanded_avg_plot <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  left_join(sample_sizes, by = "Role_Context") %>%
  mutate(label = paste0(Role_Context, "\n(n=", n, ")"))

# All pairwise comparisons:
all_levels <- unique(Touches_PlayerHyp_summary_role_expanded_avg_plot$label)
comparisons_all <- combn(all_levels, 2, simplify = FALSE)

# Final Plot:
Plot_RoleContext <- ggplot(Touches_PlayerHyp_summary_role_expanded_avg_plot, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons_all, map_signif_level = TRUE, step_increase = 0.1) +
  labs(
    title = "Prosocial Touch Rates — Role × Context",
    x = "Role and Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

##### Expanded slicing — for clear contexts (GOAL vs ASSIST vs NoGoalButAssistPossible etc.)

##### Faceted Plot (Role x Context)

# Sample sizes:
sample_sizes_exp <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(TouchRole, ExpandedContext_Label) %>%
  summarise(n = n(), .groups = "drop")

# Merge:
Touches_PlayerHyp_summary_role_expanded_avg_plot <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  left_join(sample_sizes_exp, by = c("TouchRole", "ExpandedContext_Label")) %>%
  mutate(label = paste0(ExpandedContext_Label, "\n(n=", n, ")"))

# All pairs:
all_levels_exp <- unique(Touches_PlayerHyp_summary_role_expanded_avg_plot$label)
comparisons_exp <- combn(all_levels_exp, 2, simplify = FALSE)

# Faceted Plot:
Plot_Expanded_Facet <- ggplot(Touches_PlayerHyp_summary_role_expanded_avg_plot, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  facet_wrap(~ TouchRole) +
  labs(
    title = "Prosocial Touch Rates — Expanded Context × Role",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

##### Non-facet Plot (no role)
Touches_PlayerHyp_summary_expanded_noRole <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(PlayerID, ExpandedContext_Label) %>%
  summarise(
    Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ExpandedContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(ExpandedContext_Label, "\n(n=", n, ")"))

# All pairs:
all_levels_exp_noRole <- unique(Touches_PlayerHyp_summary_expanded_noRole$label)
comparisons_exp_noRole <- combn(all_levels_exp_noRole, 2, simplify = FALSE)

# Non-facet Plot:
Plot_Expanded_NoRole <- ggplot(Touches_PlayerHyp_summary_expanded_noRole, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons_exp_noRole, map_signif_level = TRUE, step_increase = 0.1) +
  labs(
    title = "Prosocial Touch Rates — Expanded Context (no role)",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

##### Kruskal and pairwise (no role)
kruskal_test_exp_noRole <- kruskal.test(Avg_TouchRate ~ ExpandedContext_Label, data = Touches_PlayerHyp_summary_expanded_noRole)
print(kruskal_test_exp_noRole)

pairwise_exp_noRole <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_expanded_noRole$Avg_TouchRate,
  Touches_PlayerHyp_summary_expanded_noRole$ExpandedContext_Label,
  p.adjust.method = "bonferroni"
)
print(pairwise_exp_noRole)
##### Expanded slicing — for clear contexts (GOAL vs ASSIST vs NoGoalButAssistPossible etc.)

##### Expanded slicing — PREP BOTH 4-group and 3-group

# Step E1a: ExpandedContext_Label — 4-group version
Touches_PlayerHyp_Expanded_v2_4group <- player_match_with_goals %>%
  inner_join(Touches_PlayerHyp, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(str_detect(PlayersInvolved, paste0("\\b", PlayerID, "\\b"))) %>%
  mutate(
    TouchRole = case_when(
      Reciprocal %in% c("Y", "G") ~ "Both",
      ToucherNumber == PlayerID ~ "Toucher",
      ToucheeNumber == PlayerID ~ "Touchee",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(TouchRole)) %>%
  mutate(
    ExpandedContext_Label = case_when(
      # pure NO goal or assist — "non-contributor"
      (ScoredOrAssisted == 0) ~ "NoGoalOrAssist",
      
      # scored at least one goal — before first goal
      (Scored == 1 & Assisted == 0 & TimeInSeconds <= FirstScoringTime) ~ "BeforeGoal_OnlyGoal",
      
      # assisted but did not score — before first assist
      (Scored == 0 & Assisted == 1 & TimeInSeconds <= FirstAssistingTime) ~ "BeforeAssist_OnlyAssist",
      
      # scored AND assisted — before first goal
      (Scored == 1 & Assisted == 1 & TimeInSeconds <= FirstScoringTime) ~ "BeforeGoal_ScoredAndAssist",
      
      # assisted AND scored — before first assist
      (Scored == 1 & Assisted == 1 & TimeInSeconds <= FirstAssistingTime) ~ "BeforeAssist_ScoredAndAssist",
      
      # matches where player did not score — but could assist
      (Scored == 0 & Assisted == 1 & TimeInSeconds > FirstAssistingTime) ~ "NoGoalButAssistPossible",
      
      # catch-all (anything else — don't analyze here)
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ExpandedContext_Label))


# Step E1b: ExpandedContext_Label — 3-group version
Touches_PlayerHyp_Expanded_v2_3group <- player_match_with_goals %>%
  inner_join(Touches_PlayerHyp, by = c("SeasonMatchNumber", "TeamID")) %>%
  filter(str_detect(PlayersInvolved, paste0("\\b", PlayerID, "\\b"))) %>%
  mutate(
    TouchRole = case_when(
      Reciprocal %in% c("Y", "G") ~ "Both",
      ToucherNumber == PlayerID ~ "Toucher",
      ToucheeNumber == PlayerID ~ "Touchee",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(TouchRole)) %>%
  mutate(
    ExpandedContext_Label = case_when(
      (ScoredOrAssisted == 0) ~ "NoGoalOrAssist",
      (ScoredOrAssisted == 1 & TimeInSeconds <= FirstGoalOrAssistTime) ~ "BeforeAnyContribution",
      (ScoredOrAssisted == 1 & TimeInSeconds > FirstGoalOrAssistTime) ~ "AfterFirstContribution",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ExpandedContext_Label))



# Step E2: Summary per PlayerID + TouchRole + ExpandedContext_Label

Touches_PlayerHyp_summary_role_expanded <- Touches_PlayerHyp_Expanded_v2 %>%
  group_by(PlayerID, TouchRole, ExpandedContext_Label, SeasonMatchNumber) %>%
  summarise(
    Touches = n(),
    MatchSecondsPlayed = first(MatchSecondsPlayed),
    TouchRate = Touches / (MatchSecondsPlayed / 60),
    .groups = "drop"
  )

# Step E3: Aggregate to player avg

Touches_PlayerHyp_summary_role_expanded_avg <- Touches_PlayerHyp_summary_role_expanded %>%
  group_by(PlayerID, TouchRole, ExpandedContext_Label) %>%
  summarise(
    Avg_TouchRate = mean(TouchRate, na.rm = TRUE),
    .groups = "drop"
  )

##### Clean 3-group (BeforeAnyContribution vs After vs NoContribution)

# Step C1: Collapse ExpandedContext_Label to Clean3Group
Touches_PlayerHyp_summary_expanded_noRole_3group <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(PlayerID, ExpandedContext_Label) %>%
  summarise(
    Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
  summarise(
    Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CleanContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(CleanContext_Label, "\n(n=", n, ")"))

# All pairs:
all_levels_clean3 <- unique(Touches_PlayerHyp_summary_expanded_noRole_3group$label)
comparisons_clean3 <- combn(all_levels_clean3, 2, simplify = FALSE)


# Verify how many PLAYER-MATCH rows per context (diagnostic only)

Touches_PlayerHyp_summary_role_expanded %>%
  group_by(ExpandedContext_Label) %>%
  summarise(n_matches = n_distinct(paste(PlayerID, SeasonMatchNumber))) %>%
  arrange(desc(n_matches)) %>%
  print(n = Inf)


##### Faceted Plot (Role x Context)

# Sample sizes:
sample_sizes_exp <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(TouchRole, ExpandedContext_Label) %>%
  summarise(n = n(), .groups = "drop")

# Merge:
Touches_PlayerHyp_summary_role_expanded_avg_plot <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  left_join(sample_sizes_exp, by = c("TouchRole", "ExpandedContext_Label")) %>%
  mutate(label = paste0(ExpandedContext_Label, "\n(n=", n, ")"))

# All pairs:
all_levels_exp <- unique(Touches_PlayerHyp_summary_role_expanded_avg_plot$label)
comparisons_exp <- combn(all_levels_exp, 2, simplify = FALSE)

# Faceted Plot:
Plot_Expanded_Facet <- ggplot(Touches_PlayerHyp_summary_role_expanded_avg_plot, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  facet_wrap(~ TouchRole) +
  labs(
    title = "Prosocial Touch Rates — Expanded Context × Role",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

##### Clean 3-group (BeforeAnyContribution vs After vs NoContribution)

# Step C1: Collapse ExpandedContext_Label to Clean3Group
Touches_PlayerHyp_summary_expanded_noRole_3group <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(PlayerID, ExpandedContext_Label) %>%
  summarise(
    Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
  summarise(
    Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CleanContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(CleanContext_Label, "\n(n=", n, ")"))

# All pairs:
all_levels_clean3 <- unique(Touches_PlayerHyp_summary_expanded_noRole_3group$label)
comparisons_clean3 <- combn(all_levels_clean3, 2, simplify = FALSE)

# Clean 3-group Plot:
Plot_Clean3 <- ggplot(Touches_PlayerHyp_summary_expanded_noRole_3group, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons_clean3, map_signif_level = TRUE, step_increase = 0.1) +
  labs(
    title = "Prosocial Touch Rates — Clean 3-Group Context (Before/After/NoContribution)",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

##### Kruskal + pairwise for clean 3-group
kruskal_test_clean3 <- kruskal.test(Avg_TouchRate ~ CleanContext_Label, data = Touches_PlayerHyp_summary_expanded_noRole_3group)
print(kruskal_test_clean3)

pairwise_clean3 <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_expanded_noRole_3group$Avg_TouchRate,
  Touches_PlayerHyp_summary_expanded_noRole_3group$CleanContext_Label,
  p.adjust.method = "bonferroni"
)
print(pairwise_clean3)


##### Non-facet Plot (no role)
Touches_PlayerHyp_summary_expanded_noRole <- Touches_PlayerHyp_summary_role_expanded_avg %>%
  group_by(PlayerID, ExpandedContext_Label) %>%
  summarise(
    Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ExpandedContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(ExpandedContext_Label, "\n(n=", n, ")"))

# All pairs:
all_levels_exp_noRole <- unique(Touches_PlayerHyp_summary_expanded_noRole$label)
comparisons_exp_noRole <- combn(all_levels_exp_noRole, 2, simplify = FALSE)

# Non-facet Plot:
Plot_Expanded_NoRole <- ggplot(Touches_PlayerHyp_summary_expanded_noRole, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons_exp_noRole, map_signif_level = TRUE, step_increase = 0.1) +
  labs(
    title = "Prosocial Touch Rates — Expanded Context (no role)",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

##### Kruskal and pairwise (no role)
kruskal_test_exp_noRole <- kruskal.test(Avg_TouchRate ~ ExpandedContext_Label, data = Touches_PlayerHyp_summary_expanded_noRole)
print(kruskal_test_exp_noRole)

pairwise_exp_noRole <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_expanded_noRole$Avg_TouchRate,
  Touches_PlayerHyp_summary_expanded_noRole$ExpandedContext_Label,
  p.adjust.method = "bonferroni"
)
print(pairwise_exp_noRole)

##### Clean 4-group (collapsed assist cases)

# Step C1: Collapse ExpandedContext to 4 categories
Touches_PlayerHyp_summary_expanded_noRole_4group <- Touches_PlayerHyp_summary_expanded_noRole %>%
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
  summarise(
    Avg_TouchRate = mean(Avg_TouchRate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CleanContext_Label) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(label = paste0(CleanContext_Label, "\n(n=", n, ")"))

# All pairs:
all_levels_clean <- unique(Touches_PlayerHyp_summary_expanded_noRole_4group$label)
comparisons_clean <- combn(all_levels_clean, 2, simplify = FALSE)

# Clean 4-group Plot:
Plot_Clean4 <- ggplot(Touches_PlayerHyp_summary_expanded_noRole_4group, aes(x = label, y = Avg_TouchRate)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  geom_signif(comparisons = comparisons_clean, map_signif_level = TRUE, step_increase = 0.1) +
  labs(
    title = "Prosocial Touch Rates — Clean 4-Group Context",
    x = "Context",
    y = "Avg Touch Rate (touches per min)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

##### Kruskal + pairwise for clean 4-group
kruskal_test_clean4 <- kruskal.test(Avg_TouchRate ~ CleanContext_Label, data = Touches_PlayerHyp_summary_expanded_noRole_4group)
print(kruskal_test_clean4)

pairwise_clean4 <- pairwise.wilcox.test(
  Touches_PlayerHyp_summary_expanded_noRole_4group$Avg_TouchRate,
  Touches_PlayerHyp_summary_expanded_noRole_4group$CleanContext_Label,
  p.adjust.method = "bonferroni"
)
print(pairwise_clean4)

