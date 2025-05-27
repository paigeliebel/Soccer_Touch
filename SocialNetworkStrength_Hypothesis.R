# Social Network Strength Hypothesis
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


source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Core_Hypothesis.R") #Runs and brings in data frames from Core_Hypothesis.R script
source("InterMatch_Variability_Hypothesis.R") #Runs and brings in data frames from InterMatch_Variability_Hypothesis.R script

#note that this is not complete until we get Simon Data (only 2/3s of it so far)
#Ensure to use the correct dfs. Touches_final and Matches_final are correct. They only include assigned rater data, no repeat matches

#Check to make sure data frames are loaded:

if (!exists("Touches_final") | !exists("Touches_scaled") | !exists("Matches_finalID") | !exists("FinalStandings") | !exists("Touches_CoreHyp")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded. Check Data_Management.R and Core_Hypothesis.R.")
}

############################ Social Network Strength ############################

#Clean data to grab what we need: reciprocity, toucher numbers, touchee numbers
#Group event also count towards reciprocal, so long as it is not GF, SUB etc 

Touches_Reciprocal <- Touches_final %>% 
  mutate(
    Team = as.character(Team),
    Reciprocity = str_trim(Reciprocal) #cleans up white spaces in case
  )

# Count touches by team and reciprocity type
Reciprocity_by_Team <- Touches_Reciprocal %>%
  filter(!is.na(Reciprocity)) %>%
  mutate(
    ReciprocityType = case_when(
      Reciprocity %in% c("Y", "G") ~ "Reciprocal", #Reciprocal includes group events
      Reciprocity == "N" ~ "NonReciprocal",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(ReciprocityType)) %>%
  group_by(Team, ReciprocityType) %>%
  summarise(TouchCount = n(), .groups = "drop") %>%
  pivot_wider(names_from = ReciprocityType, values_from = TouchCount, values_fill = 0) %>%
  mutate(
    Reciprocal_To_NonRecip_Ratio = Reciprocal / NonReciprocal,
    DataFlag = if_else(Reciprocal + NonReciprocal == 0, "No Touch Data", "Data Available")
  )

#Join with Final Standings
Reciprocal_vs_Rank <- FinalStandings %>%
  mutate(TeamID = str_pad(as.character(TeamID), width = 2, pad = "0")) %>%
  left_join(Reciprocity_by_Team, by = c("TeamID" = "Team"))

#Plot
ggplot(Reciprocal_vs_Rank, aes(x = Rank, y = Reciprocal_To_NonRecip_Ratio)) +
  geom_point(size = 3, color = "steelblue") +  # all teams shown in one color
  geom_smooth(
    data = filter(Reciprocal_vs_Rank, !is.na(Reciprocal_To_NonRecip_Ratio)),
    aes(x = Rank, y = Reciprocal_To_NonRecip_Ratio),
    method = "lm", se = FALSE, color = "black"
  ) +
  scale_x_reverse(breaks = 1:max(Reciprocal_vs_Rank$Rank)) +
  labs(
    title = "Reciprocal Touch Ratio vs Final Season Rank per Team",
    subtitle = "Higher ratios may reflect stronger intra-team cohesion",
    x = "Team Final Season Rank",
    y = "Team Touch Ratio (Reciprocal : Non-Reciprocal)",
  ) +
  theme_minimal()

#Stats Sum
reciprocal_lm <- lm(Rank ~ Reciprocal_To_NonRecip_Ratio, data = Reciprocal_vs_Rank)


############################ Reciprocal/Non-Reciprocal | Social Network Strength ############################

#Reciprocal vs non-reciprocal touch | simple final standings to ratio of reciprocal/non-reciprocal
#Exclude Injury Touch because it is usually uni-direcitonal, injured player does not touch back


############################ Player Concentration | Social Network Strength ############################

#concentration of touch within players on a team could be a metric for overall team cohesion | ridge plot
#include injur-touch because we are looking at how touchy some players are
