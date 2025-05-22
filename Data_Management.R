# Data_Management
# For more information on these data frames please look at the README.md file

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(readxl)
library(rmarkdown)
library(readr)

################################################################

# raters and corresponding sheets
# Rater1 = Paige, Rater2 = Tobi, Rater3 = Simon

Touch_Rater1 <- read_csv("SpreadSheets/Touch_Paige.csv")
Touch_Rater1 <- Touch_Rater1 %>% mutate(Rater = "Rater1") #adding Rater identifier to df
Touch_Rater2 <- read_csv("SpreadSheets/Touch_Tobi.csv")
Touch_Rater2 <- Touch_Rater2 %>% mutate(Rater = "Rater2")
#Touch_Rater3 <- read_csv("SpreadSheets/Touch_Simon.csv")
#Touch_Rater3 <- Touch_Rater3 %>% mutate(Rater = "Rater3")
Touch_Dataframes_List <- list(Touch_Rater1, Touch_Rater2) #list of touch dataframes wanted to be passed into Touches

Touches <- bind_rows(Touch_Dataframes_List) #Total touch df

Match_Rater1 <- read_csv("SpreadSheets/Match_Paige.csv")
Match_Rater1 <- Match_Rater1 %>% mutate(Rater = "Rater1")
Match_Rater2 <- read_csv("SpreadSheets/Match_Tobi.csv")
Match_Rater2 <- Match_Rater2 %>% mutate(Rater = "Rater2")
#Match_Rater3 <- read_csv("SpreadSheets/Match_Simon.csv")
#Match_Rater3 <- Match_Rater3 %>% mutate(Rater = "Rater3")
Match_Dataframes_List <- list(Match_Rater1, Match_Rater2) #list of match dfs wanted to be passed into Matches

Matches <- bind_rows(Match_Dataframes_List) #Total match df

################################################################

#Primary_SeasonOverview <- read_csv("SpreadSheets/Primary_SeasonOverview.csv")

Team_IDs <- read_csv("SpreadSheets/TeamIDs.csv")

#Stadiums <- read_csv("SpreadSheets/Stadiums.csv")

#MatchAssignments_WatchOrder <- read_csv("SpreadSheets/MatchAssignments_WatchOrder.csv")

StandingsByWeek <- read_csv("SpreadSheets/StandingsByWeek_Clean.csv")

FinalStandings <- read_csv("SpreadSheets/FinalSeasonStandings.csv")

################################################################

#Defining valid entries into columns based on project setup

# Define valid values

valid_touch_actions <- c(
  "Tap", "Bump", "Push", "Squeeze", "Grab",
  "Kiss", "Hug", "Rub", "Stroke"
)

valid_reciprocal <- c("N", "Y", "G")

valid_body_parts <- c(
  "H", "Arm", "FT", "Legs", "BT", "Gluteal Region", 
  "Head", "Neck", "Feet"
)

valid_situations <- c(
  "F", "FY", "FR", "KS", "SA", "PP", "SUB", "GF", "GA", "DA",
  "CK", "TI", "REF", "IT", "HB", "OFF", "GK", "HUD", "WALL",
  "PEN", "Other", "BRAWL"
)

valid_hapticrituals <- c(
  "HF1", "HF2", "LF1", "LF2", "CO", "HS", "HT", "P", "CB",
  "GHUG", "FB", "HR", "HH", "CR", "DP", "BS", "HUP", "CAP",
  "SG", "NEG", "Other", "FBP", "BBP", "HUG", "CHUG", "TA", "SHUG"
)

################################################################

#Cleaning Functions to fix user input error: Such as "Arn" to "Arm"

clean_touch_action <- function(x) {
  case_when(
    x %in% c("GHUG", "HUG") ~ "Hug",
    TRUE ~ x
  )
}

clean_reciprocal <- function(x) {
  case_when(
    x == "B" ~ "N",
    TRUE ~ x
  )
}

clean_body_parts <- function(x) {
  x %>%
    str_split(",\\s*") %>%
    map_chr(~ paste(
      str_replace_all(.x, c(
        "Arn" = "Arm",
        "AH" = "Arm",
        "Hand" = "H",
        "Back Torso" = "BT",
        "Front Torso" = "FT",
        "\\bLeg\\b" = "Legs",
        "Bt" = "BT",
        "Ft" = "FT"
      )),
      collapse = ", "
    ))
}

clean_situation <- function(x) {
  case_when(
    x == "FEF" ~ "REF",
    x == "HK" ~ "GK",
    TRUE ~ x
  )
}

clean_haptic_ritual <- function(x) {
  case_when(
    x == "LF!" ~ "LF1",
    TRUE ~ x
  )
}

clean_visibility <- function(x) {
  case_when(
    x %in% c("B", "H") ~ "G",
    TRUE ~ x
  )
}

#Now applying these functions to corresponding data frames and columns

Touches <- Touches %>%
  mutate(
    TouchAction = clean_touch_action(TouchAction),
    ToucherBodyPart = clean_body_parts(ToucherBodyPart),
    ToucheeBodyPart = clean_body_parts(ToucheeBodyPart),
    Situation = clean_situation(Situation),
    HapticRitual = clean_haptic_ritual(HapticRitual),
    Reciprocal = clean_reciprocal(Reciprocal),
    Visibility = clean_visibility(Visibility)
  )

################################################################

#Troubleshooting: Looking at each column and checking values to look for human error while inputting data during collection

cols <- c("Team", "TouchAction", "Reciprocal", "ToucherBodyPart", "ToucheeBodyPart", "Situation", "HapticRitual", "Visibility")

#Starting with Touches

unique_vals <- Touches %>%
  select(all_of(cols)) %>% 
  map(~ unique(.))

max_len <- max(lengths(unique_vals))
padded <- map(unique_vals, ~ { length(.) <- max_len; . })
unique_values_df <- as.data.frame(padded)
View(unique_values_df)

#determining which touchID and rater discrepancies are connected to:

Touches_invalid <- Touches %>%
  filter(
    !(Reciprocal %in% valid_reciprocal) |
      is_invalid_bodypart(ToucherBodyPart) |
      is_invalid_bodypart(ToucheeBodyPart) |
      !(Situation %in% valid_situations) |
      !(HapticRitual %in% valid_rituals) |
      !(TouchAction %in% valid_touch_actions)
  )

Touches_invalid_rater1 <- filter(Touches_invalid, Rater == "Rater1")
Touches_invalid_rater2 <- filter(Touches_invalid, Rater == "Rater2")
#Touches_invalid_rater3 <- filter(Touches_invalid, Rater == "Rater3")
  

