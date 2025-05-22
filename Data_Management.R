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

raters <- list(
  Rater1 = list(touch = "Touch_Paige.csv", match = "Match_Paige.csv"),
  Rater2 = list(touch = "Touch_Tobi.csv",  match = "Match_Tobi.csv")
  # Rater3 = list(touch = "Touch_Simon.csv", match = "Match_Simon.csv")  # <- uncomment when ready to add Simon's data
)

Touch_Dataframes_List <- map2(names(raters), raters, function(rater_name, files) { #Creates a list of data frames that are each raters csv files
  read_csv(file.path("SpreadSheets", files$touch)) %>%
    mutate(Rater = rater_name) #adds a column with raters name
})

Match_Dataframes_List <- map2(names(raters), raters, function(rater_name, files) {
  read_csv(file.path("SpreadSheets", files$match)) %>%
    mutate(Rater = rater_name)
})

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

is_invalid_bodypart <- function(x) { #splits lists within cell to look at individual values (checks to make sure "FT, Arn" is flagged for "Arn")
  x %>%
    str_split(",\\s*") %>%
    map_lgl(~ any(!.x %in% valid_body_parts))
}

is_invalid_haptic_ritual <- function(x) { #splits lists within cell to look at individual values (flags things like "CR, BF" for "BF" which is not a valid haptic ritual)
  x %>%
    str_split(",\\s*") %>%
    map_lgl(~ any(!.x %in% valid_hapticrituals))
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

#Function to let us know where the invalid feature occurs. Output is a new column that contains the column location of the error
get_invalid_fields <- function(row) {
  invalid_fields <- c()
  
  if (!(row$Reciprocal %in% valid_reciprocal)) {
    invalid_fields <- c(invalid_fields, "Reciprocal")
  }
  
  if (is_invalid_bodypart(row$ToucherBodyPart)) {
    invalid_fields <- c(invalid_fields, "ToucherBodyPart")
  }
  
  if (is_invalid_bodypart(row$ToucheeBodyPart)) {
    invalid_fields <- c(invalid_fields, "ToucheeBodyPart")
  }
  
  if (!(row$Situation %in% valid_situations)) {
    invalid_fields <- c(invalid_fields, "Situation")
  }
  
  if (is_invalid_haptic_ritual(row$HapticRitual)) {
    invalid_fields <- c(invalid_fields, "HapticRitual")
  }
  
  if (!(row$TouchAction %in% valid_touch_actions)) {
    invalid_fields <- c(invalid_fields, "TouchAction")
  }
  
  paste(invalid_fields, collapse = ", ")
}
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
      is_invalid_haptic_ritual(HapticRitual) |
      !(TouchAction %in% valid_touch_actions)
  )

Touches_invalid_by_rater <- split(Touches_invalid, Touches_invalid$Rater) #split by rater

Touches_invalid_labeled <- map(Touches_invalid_by_rater, function(df) { #apply get_invalid_fields() to each row in each rater's dataframe
  df %>%
    rowwise() %>%
    mutate(Invalid_Fields = get_invalid_fields(cur_data())) %>%
    ungroup()
})

Touches_invalid_rater1 <- Touches_invalid_labeled[["Rater1"]]
Touches_invalid_rater2 <- Touches_invalid_labeled[["Rater2"]]
#Touches_invalid_rater3 <- Touches_invalid_labeled[["Rater3"]]




