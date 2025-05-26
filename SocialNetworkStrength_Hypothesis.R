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

############################ Social Network Strength ############################

#Reciprocal vs non-reciprocal touch | simple final standings to ratio of reciprocal/non-reciprocal
#concentration of touch within players on a team could be a metric for overall team cohesion | ridge plot