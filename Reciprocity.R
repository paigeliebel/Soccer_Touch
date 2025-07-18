#Reciprocity
# For more information on these data frames please look at the README.md file

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
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggrepel)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Overview_summary_Data.R") #Runs and brings in data frames from Core_Hypothesis.R script
source("MatchPerformance_Stats_PK.R") #Runs abd brings in data frames from MatchPerformance and stats script

############################ Reciprocal/Non-Reciprocal ############################
# Define situations to exclude
exclude_situations <- c("GF", "GA", "SUB")

# Filter and calculate touches by reciprocity group per team (excluding specified situations)
Reciprocity_by_Team_filtered <- Touches_ReciprocalNonRecip %>%
  filter(!is.na(Reciprocity_Group)) %>%
  filter(!Situation %in% exclude_situations) %>%   # exclude GF, GA, SUB
  group_by(Team, Reciprocity_Group) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = Reciprocity_Group, values_from = TouchCount, values_fill = 0) %>%
  mutate(
    Reciprocal_To_NonRecip_Ratio = Reciprocal / NonReciprocal,
    DataFlag = if_else(Reciprocal + NonReciprocal == 0, "No Touch Data", "Data Available")
  )

#Add R1, R2 etc for team rank/labeling
FinalStandings <- FinalStandings %>%
  mutate(
    TeamID = str_pad(as.character(TeamID), width = 2, pad = "0"),
    RankLabel = paste0("R", Rank)
  )

# Ensure Team IDs are padded the same way
Reciprocity_by_Team_filtered <- Reciprocity_by_Team_filtered %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0"))

# Step 1: Join with FinalStandings to get RankLabel
Reciprocity_with_rank <- Reciprocity_by_Team_filtered %>%
  mutate(Team = str_pad(as.character(Team), width = 2, pad = "0")) %>%
  left_join(FinalStandings %>% select(TeamID, RankLabel, Rank),  # <-- include Rank here
            by = c("Team" = "TeamID")) %>%
  filter(DataFlag == "Data Available") %>%
  mutate(
    DisplayLabel = paste0(RankLabel),
    DisplayLabel = factor(DisplayLabel, levels = DisplayLabel[order(-Reciprocal_To_NonRecip_Ratio)])
  )

# Step 4: Plot the lollipop chart
lollipop_season_recip_byteam_plot <- ggplot(Reciprocity_with_rank, 
                                            aes(x = DisplayLabel, y = Reciprocal_To_NonRecip_Ratio)) +
  geom_segment(aes(xend = DisplayLabel, y = 0, yend = Reciprocal_To_NonRecip_Ratio), 
               color = "gray70") +
  geom_point(color = "gray40", size = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  coord_flip() +
  labs(
    title = "Reciprocity Touch Ratio (Season Aggregate)",
    x = "Team (Rank Label: R1 = Best)",
    y = "Reciprocal:Nonreciprocal Touch Ratio"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black")
  )

# Plot reciprocal ratio vs final season rank
Reciprocal_vs_Rank_Plot_filtered <- ggplot(Reciprocity_with_rank, aes(x = Rank, y = Reciprocal_To_NonRecip_Ratio)) +
  geom_point(size = 3, color = "black") +
  geom_text(aes(label = DisplayLabel), 
            family = "Times New Roman",
            size = 3,
            vjust = -1,  # Adjust vertically if needed
            hjust = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  scale_x_reverse(breaks = 1:14) +
  labs(
    title = "Reciprocity Ratio vs Final Season Rank",
    x = "Final Season Rank (1 = Best)",
    y = "Reciprocal:Non-Reciprocal Touch Ratio"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black")
  )


# Check normality of the ratio
shap_recip <- shapiro.test(Reciprocity_with_rank$Reciprocal_To_NonRecip_Ratio)
shap_rank <- shapiro.test(as.numeric(gsub("R", "", Reciprocity_with_rank$DisplayLabel)))  # approximate Rank

# Perform Pearson correlation (parametric)
pearson_ratio<- cor.test(Reciprocity_with_rank$Reciprocal_To_NonRecip_Ratio,
         Reciprocity_with_rank$Rank,
         method = "pearson")

recip_ratio_lm <- lm(Rank ~ Reciprocal_To_NonRecip_Ratio, data = Reciprocity_with_rank)
summary(recip_ratio_lm)


# Not statistically significant, suggesting that the reciprocal/nonreciprocal ratio is linked
# to the GF, SUBs, and GA situations. 
# What is going on here? Perhaps GF, GA, and SUB all have high reciprocal touch 
# Step 1 & 2: Summarize counts and proportions by Situation and Reciprocity_Group

library(scales)

# Categorize situations into GF, GA, SUB, or Other
Touches_ReciprocalNonRecip <- Touches_ReciprocalNonRecip %>%
  mutate(
    Situation_Bucket = case_when(
      Situation %in% c("GF", "GA", "SUB") ~ Situation,
      TRUE ~ "Other"
    )
  )

# Summarize counts per Reciprocity_Group and Situation_Bucket
situation_summary_reciprocity <- Touches_ReciprocalNonRecip %>%
  filter(!is.na(Reciprocity_Group)) %>%
  group_by(Reciprocity_Group, Situation_Bucket) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Reciprocity_Group) %>%
  mutate(
    Percent = Count / sum(Count)
  ) %>%
  ungroup()

# Plot grouped bar chart with percent on y-axis
situation_summary_reciprocity_plot <- ggplot(situation_summary_reciprocity, aes(x = Reciprocity_Group, y = Percent, fill = Situation_Bucket)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +  # side-by-side bars
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportion of Situations by Reciprocity Group",
    x = "",
    y = "Percentage of Touches",
    fill = "Situation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#### Okay ya, so the ratio of a teams reciprocity is significantly different when accounting for GF and SUBS.

######## Lets look at reciprocal and non recriprocal totally separately ####
# Sum total touches by team for reciprocal
Recip_by_team <- Reciprocal_Touches %>%
  group_by(Team) %>%
  summarise(Reciprocal = n(), .groups = "drop")

# Sum total touches by team for nonreciprocal
Nonrecip_by_team <- NonReciprocal_Touches %>%
  group_by(Team) %>%
  summarise(NonReciprocal = n(), .groups = "drop")

# Join with FinalStandings (make sure TeamID format matches 'Team')
FinalStandings <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0"))

#Add R1, R2 etc for team rank/labeling
FinalStandings <- FinalStandings %>%
  mutate(
    TeamID = str_pad(as.character(TeamID), width = 2, pad = "0"),
    RankLabel = paste0("R", Rank)
  )

# Join reciprocal touches with standings
Recip_vs_rank <- FinalStandings %>%
  left_join(Recip_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(Reciprocal)) %>%
  mutate(Rank = as.numeric(Rank))

# Join nonreciprocal touches with standings
NonRecip_vs_rank <- FinalStandings %>%
  left_join(Nonrecip_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(NonReciprocal)) %>%
  mutate(Rank = as.numeric(Rank))

#Normalcy Test 
# Sum total touches by team for reciprocal
Recip_by_team <- Reciprocal_Touches %>%
  group_by(Team) %>%
  summarise(Reciprocal = n(), .groups = "drop")

# Sum total touches by team for nonreciprocal
Nonrecip_by_team <- NonReciprocal_Touches %>%
  group_by(Team) %>%
  summarise(NonReciprocal = n(), .groups = "drop")

# Join with FinalStandings (make sure TeamID format matches 'Team')
FinalStandings <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0"))


# Join reciprocal touches with standings
Recip_vs_rank <- FinalStandings %>%
  left_join(Recip_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(Reciprocal)) %>%
  mutate(Rank = as.numeric(Rank))

# Join nonreciprocal touches with standings
NonRecip_vs_rank <- FinalStandings %>%
  left_join(Nonrecip_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(NonReciprocal)) %>%
  mutate(Rank = as.numeric(Rank))


# Plot Reciprocal touches vs Rank
recip_touch_vs_rank_plot <- ggplot(Recip_vs_rank, aes(x = Rank, y = Reciprocal)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  scale_x_reverse(breaks = 1:max(Recip_vs_rank$Rank)) +
  labs(title = "Reciprocal Touches vs Final Season Rank",
       x = "Final Season Rank (1 = Best)",
       y = "Total Reciprocal Touches") +
  theme_minimal()

# Plot NonReciprocal touches vs Rank
nonrecip_touch_vs_rank_plot <- ggplot(NonRecip_vs_rank, aes(x = Rank, y = NonReciprocal)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_x_reverse(breaks = 1:max(NonRecip_vs_rank$Rank)) +
  labs(title = "NonReciprocal Touches vs Final Season Rank",
       x = "Final Season Rank (1 = Best)",
       y = "Total NonReciprocal Touches") +
  theme_minimal()

#Normalcy Check
normalcy_recip <- shapiro.test(Recip_vs_rank$Reciprocal)
normalcy_nonrecip <- shapiro.test(NonRecip_vs_rank$NonReciprocal)

#Relationship testing
spear_recip <- cor.test(Recip_vs_rank$Reciprocal, Recip_vs_rank$Rank, method = "spearman")
spear_nonrecip <- cor.test(NonRecip_vs_rank$NonReciprocal, NonRecip_vs_rank$Rank, method = "spearman")

lm_recip <- lm(Rank ~ Reciprocal, data = Recip_vs_rank)
lm_nonrecip <- lm(Rank ~ NonReciprocal, data = NonRecip_vs_rank)


###Combine into long format. Make overlap graph
# Combine into long format for shared plotting
Recip_long <- Recip_vs_rank %>%
  select(TeamID, Rank, RankLabel, TouchCount = Reciprocal) %>%
  mutate(Type = "Reciprocal")

NonRecip_long <- NonRecip_vs_rank %>%
  select(TeamID, Rank, RankLabel, TouchCount = NonReciprocal) %>%
  mutate(Type = "Nonreciprocal")

RecipNonrecip_combined <- bind_rows(Recip_long, NonRecip_long)

#rename
RecipNonrecip_combined <- RecipNonrecip_combined %>%
  mutate(Type = recode(Type,
                       "Nonreciprocal" = "Non-Reciprocal",
                       "Reciprocal" = "Reciprocal"))

#plot together
recip_nonrecip_combined_plot <- ggplot(RecipNonrecip_combined, aes(x = Rank, y = TouchCount, color = Type)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_x_reverse(breaks = 1:max(RecipNonrecip_combined$Rank)) +
  scale_color_manual(
    values = c("Reciprocal" = "steelblue4", "Non-Reciprocal" = "indianred3"),
    name = "Touch Type"
  ) +
  labs(
    title = "Reciprocal & Non-Reciprocal Touch Frequency \n vs Final Season Rank",
    x = "Final Season Rank (1 = Best)",
    y = "Touch Count"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black")
  )


#################Match outcome Ratio ###############3
# ratio

library(dplyr)
library(ggplot2)

# Filter data to exclude GF, GA, SUB situations
filtered_touches_recip_nonrecip <- Touches_ReciprocalNonRecip %>%
  filter(!Situation %in% c("GF", "GA", "SUB")) %>%
  filter(!is.na(Reciprocity_Group))

# Calculate reciprocal and nonreciprocal counts per team per match
touches_per_match_ratio <- filtered_touches_recip_nonrecip %>%
  group_by(Team, SeasonMatchNumber, Reciprocity_Group) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = Reciprocity_Group, values_from = TouchCount, values_fill = 0) %>%
  mutate(Recip_NonRecip_Ratio = Reciprocal / NonReciprocal)

# Join rank info to your per-match data
touches_per_match_ranked <- touches_per_match_ratio %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID"))

# Convert Team to factor ordered by Rank
touches_per_match_ranked <- touches_per_match_ranked %>%
  mutate(Team = factor(Team, levels = FinalStandings %>% arrange(Rank) %>% pull(TeamID)))

# Ensure DisplayLabel is a factor ordered by ascending rank
touches_per_match_ranked <- touches_per_match_ratio %>%
  left_join(FinalStandings %>% 
              select(TeamID, Rank, RankLabel), 
            by = c("Team" = "TeamID")) %>%
  filter(!is.na(RankLabel)) %>%
  mutate(
    DisplayLabel = factor(RankLabel, levels = paste0("R", sort(unique(Rank))))  # R1 = best
  )

# Reverse the order to put R1 on the right
display_levels_reversed <- rev(levels(touches_per_match_ranked$DisplayLabel))
touches_per_match_ranked$DisplayLabel <- factor(touches_per_match_ranked$DisplayLabel,
                                                levels = display_levels_reversed)

# Plot with grayscale and reversed x-axis (R1 on right)
PerMatchRatioSpread <- ggplot(touches_per_match_ranked, aes(x = DisplayLabel, y = Recip_NonRecip_Ratio)) +
  geom_boxplot(fill = "gray80", color = "gray20", outlier.colour = "black", outlier.size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_x_discrete(limits = display_levels_reversed) +
  labs(
    title = "Match-Level: Variation of Within-Team Reciprocity Ratios",
    x = "Team (Ordered by Final Season Rank)",
    y = "Reciprocal:Nonreciprocal Touch Ratio"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black")
  )

##############################################################################
# Join with Matches_finalID to get goal differential
touches_ratio_goaldiff <- touches_per_match_ratio %>%
  left_join(Matches_finalID %>% select(SeasonMatchNumber, TeamID, GoalDiff),
            by = c("SeasonMatchNumber", "Team" = "TeamID")) %>%
  filter(!is.na(Recip_NonRecip_Ratio))  # remove NA ratios if any (e.g. NonReciprocal = 0)

# Step 1: Join in final season rank
touches_ratio_goaldiff_ranked <- touches_ratio_goaldiff %>%
  left_join(FinalStandings %>% select(TeamID, Rank), by = c("Team" = "TeamID")) %>%
  filter(!is.na(Rank))

# Step 2: Plot with green-to-red color gradient by final rank
ratio_vs_goaldiff_plot <- ggplot(touches_ratio_goaldiff_ranked, aes(x = Recip_NonRecip_Ratio, y = GoalDiff, color = Rank)) +
  geom_point(alpha = 0.9, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.1) +
  scale_color_gradient(
    low = "limegreen",  # best rank = green
    high = "firebrick",  # worst rank = red
    name = "Final Rank",
    guide = guide_colorbar(reverse = TRUE)  # so best rank is green at top of legend
  ) +
  labs(
    title = "Match-Level: Goal Differential vs Reciprocity Ratio",
    x = "Reciprocal:Non-Reciprocal Touch Ratio per Match",
    y = "Goal Differential",
    caption = "Green = Top-ranked teams, Red = Lower-ranked teams"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black")
  )

#Normality tests:
# Test normality of Reciprocal:Nonreciprocal Ratio
shap_ratio <- shapiro.test(touches_ratio_goaldiff_ranked$Recip_NonRecip_Ratio)

# Test normality of Goal Differential
shap_goaldiff <- shapiro.test(touches_ratio_goaldiff_ranked$GoalDiff)

pearson_match_ratio <- cor.test(
  touches_ratio_goaldiff_ranked$Recip_NonRecip_Ratio,
  touches_ratio_goaldiff_ranked$GoalDiff,
  method = "pearson"
)

match_ratio_lm <- lm(GoalDiff ~ Recip_NonRecip_Ratio, data = touches_ratio_goaldiff_ranked)
summary(match_ratio_lm)

