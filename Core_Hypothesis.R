# Core_Hypothesis
# Added Outlier Analysis as well
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library (dplyr)

source("Overview_Summary_Data.R") #Runs and brings in data frames

#Check to make sure data frames are loaded:
if (!exists("Touches_final") | !exists("Matches_final") | !exists("FinalStandings")) {
  stop("Touches_final, Matches_final, or FinalStandings not loaded. Check Data_Management.R.")
}

############################ Create Data Set for this Hypothesis ############################ 

#Count of frequency of touches per team
Touches_by_team <- Touches_CoreData %>%
  mutate(Team = str_trim(as.character(Team))) %>%
  count(Team, name = "TotalTouches")

# Make sure TeamID is padded to match
FinalStandings <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0"))

#Join touch counts with final standings
Team_Touches_Standings <- FinalStandings %>%
  left_join(Touches_by_team, by = c("TeamID" = "Team")) %>%
  filter(!is.na(TotalTouches))

#Plot with regression line : final rankings to frequency of touch
library(ggrepel)

TouchFreq_vs_FinalStandings <- ggplot(Team_Touches_Standings, aes(x = Rank, y = TotalTouches)) +
  geom_point(size = 3, color = "gray30") +
  geom_text_repel(aes(label = Team), size = 3.5, max.overlaps = Inf) +  # ← Team names
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  scale_x_reverse(breaks = 1:14) +
  labs(
    title = "Final Rank vs Overall Touch Frequency",
    x = "Final Season Rank (1 = Best)",
    y = "Total Touches (Filtered)"
  ) +
  theme_minimal()

Team_Touches_Standings <- Team_Touches_Standings %>%
  mutate(
    Rank = as.numeric(Rank),
    TotalTouches = as.numeric(TotalTouches)
  )

#relationshp testing
totalseason_lineaermodel <- lm(TotalTouches ~ Rank, data = Team_Touches_Standings)
totalseason_pearson <- cor.test(Team_Touches_Standings$Rank, Team_Touches_Standings$TotalTouches, method = "pearson")

##### Nothing of interest found. There does not appear to be a relationship between end of season rank and filtered data

#possibly something with unfiltered? Should I not have filtered?

# Step 2: Use the "unfiltered" dataframe that already contains total touches per team
# Rename for consistency
Touches_by_team_unfiltered <- unfiltered %>%
  rename(TotalTouches = `Total Season Touches`)

Team_Touches_Standings_unfiltered <- FinalStandings %>%
  left_join(Touches_by_team_unfiltered, by = c("TeamID" = "TeamID")) %>%
  filter(!is.na(TotalTouches))

TouchFreq_vs_FinalStandings_unfiltered <- ggplot(Team_Touches_Standings_unfiltered, aes(x = Rank, y = TotalTouches)) +
  geom_point(size = 3, color = "gray30") +
  geom_text_repel(aes(label = Team), size = 3.5, max.overlaps = Inf) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  scale_x_reverse(breaks = 1:14) +
  labs(
    title = "Final Rank vs Overall Touch Frequency (Unfiltered)",
    x = "Final Season Rank (1 = Best)",
    y = "Total Touches (Unfiltered)"
  ) +
  theme_minimal()

Team_Touches_Standings_unfiltered <- Team_Touches_Standings_unfiltered %>%
  mutate(
    Rank = as.numeric(Rank),
    TotalTouches = as.numeric(TotalTouches)
  )

totalseason_lineaermodel_unfiltered <- lm(TotalTouches ~ Rank, data = Team_Touches_Standings_unfiltered)
totalseason_lineaermodel_unfiltered_summary <- summary(totalseason_lineaermodel_unfiltered)

totalseason_pearson_unfiltered <- cor.test(
  Team_Touches_Standings_unfiltered$Rank,
  Team_Touches_Standings_unfiltered$TotalTouches,
  method = "pearson"
)

#confirmed that there is not even statistical significance when looking at unfiltered data... goals etc don't matter

############################ Within-Team Variability in Touch Frequency ############################ 

# Looks at the variability a team has across matches throughout the season
# Do less variable teams do better? 

team_variation <- Touches_per_match %>%
  group_by(Team) %>%
  summarise(iqr_touch = IQR(TouchCount, na.rm = TRUE))

team_variation_ranked <- team_variation %>%
  left_join(FinalStandings %>% select(TeamID, Rank), 
            by = c("Team" = "TeamID")) %>%
  mutate(Rank = as.numeric(Rank))

team_variation_plot <- ggplot(team_variation_ranked, aes(x = Rank, y = iqr_touch)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  scale_x_reverse() +
  labs(
    title = "Within-Team Touch Variation (IQR) vs Final Season Rank",
    x = "Final Rank (1 = Best)",
    y = "IQR of Touches per Match"
  ) +
  theme_minimal()

team_variation_summary <- cor.test(team_variation_ranked$iqr_touch, team_variation_ranked$Rank, method = "spearman")

# No relationship between variability within-a-team and end of season rankings. 

###### Dive into Outlier Analysis! This will maybe show interesting stuff. 
## check out the OverviewSummaryData sheet for this 

