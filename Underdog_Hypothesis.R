# Underdog Hypothesis
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)
library (dplyr)

source("Data_Management.R") #Runs and brings in data frames from Data_Management.R script
source("Core_Hypothesis.R") #Runs and brings in data frames from Core_Hypothesis.R script

#note that this is not complete until we get Simon Data (only 2/3s of it so far)
#Ensure to use the correct dfs. Touches_final and Matches_final are correct. They only include assigned rater data, no repeat matches

#Check to make sure data frames are loaded:
if (!exists("Touches_final") | !exists("Matches_final") | !exists("FinalStandings")) {
  stop("Touches_final, Matches_final, or FinalStandings not loaded. Check Data_Management.R.")
}

if (!exists("Touches_final") | !exists("Matches_final") | !exists("FinalStandings") | !exists("Touches_CoreHyp")) {
  stop("Touches_final, Matches_final, Touhe_CoreHyp or FinalStandings not loaded. Check Data_Management.R and Core_Hypothesis.R.")
}

############################ Underdog Hypothesis ############################















