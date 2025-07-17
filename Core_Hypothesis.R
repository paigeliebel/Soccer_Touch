# Core_Hypothesis
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(extrafont)     # Optional for font registration on some systems
library(showtext)      # Better font support across systems



source("Overview_Summary_Data.R") #Runs and brings in data frames

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

TouchFreq_vs_FinalStandings <- ggplot(Team_Touches_Standings, aes(x = Rank, y = TotalTouches)) +
  geom_point(size = 3, color = "gray30") +
  geom_text_repel(aes(label = RankLabel), size = 3.5, max.overlaps = Inf, family = "Times New Roman") +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linewidth = 1) +
  scale_x_reverse(breaks = 1:14) +
  labs(
    title = "Season-Level: Touch Frequency vs End of Season Rank",
    x = "End of Season Rank (Best = 1)",
    y = "Total Season Touch Instances by Team"
  ) +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  )

Team_Touches_Standings <- Team_Touches_Standings %>%
  mutate(
    Rank = as.numeric(Rank),
    TotalTouches = as.numeric(TotalTouches)
  )

# Shapiro-Wilk test for normality
shapiro.test(Team_Touches_Standings$TotalTouches)  # for total touches
# Failed to reject, therefore Normal distribution 

#relationshp testing
totalseason_lineaermodel <- lm(TotalTouches ~ Rank, data = Team_Touches_Standings)
totalseason_pearson <- cor.test(Team_Touches_Standings$Rank, Team_Touches_Standings$TotalTouches, method = "pearson")

##### Nothing of interest found. There does not appear to be a relationship between end of season rank and filtered data

############################ Within-Team Variability in Touch Frequency ############################ 

# Looks at the variability a team has across matches throughout the season
# Do less variable teams do better? 

team_variation <- Touches_per_match %>%
  group_by(Team) %>%
  summarise(iqr_touch = IQR(TouchCount, na.rm = TRUE))

team_variation_ranked <- team_variation %>%
  left_join(FinalStandings %>% select(TeamID, Rank, RankLabel), 
            by = c("Team" = "TeamID")) %>%
  mutate(Rank = as.numeric(Rank))

team_variation_plot <- ggplot(team_variation_ranked, aes(x = Rank, y = iqr_touch)) +
  geom_point(size = 3, color = "gray30") +
  geom_text_repel(aes(label = RankLabel), size = 3.5, family = "Times New Roman", max.overlaps = Inf) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linewidth = 1) +
  scale_x_reverse(breaks = 1:14) +
  labs(
    title = "Within-Team Touch Variation (IQR) vs Final Season Rank",
    x = "Final Season Rank (1 = Best)",
    y = "IQR of Touches per Match across Season"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85"),
    axis.line = element_line(color = "black"),
    panel.border = element_blank()
  )

team_variation_summary <- cor.test(team_variation_ranked$Rank, team_variation_ranked$iqr_touch, method = "spearman")

# No relationship between variability within-a-team and end of season rankings. 

###### Dive into Outlier Analysis! This will maybe show interesting stuff. 
## check out the OverviewSummaryData sheet for this 

