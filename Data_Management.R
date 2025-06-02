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
  Rater2 = list(touch = "Touch_Tobi.csv",  match = "Match_Tobi.csv"),
  Rater3 = list(touch = "Touch_Simon.csv", match = "Match_Simon.csv")  
)

Touch_Dataframes_List <- map2(names(raters), raters, function(rater_name, files) {
  read_csv(file.path("SpreadSheets", files$touch)) %>%
    slice(-1) %>%  # remove instruction row
    mutate(across(where(is.character), ~ str_replace_all(., '^"|"$', ""))) %>%
    mutate(Rater = rater_name)
})

Touches <- bind_rows(Touch_Dataframes_List)

Match_Dataframes_List <- map2(names(raters), raters, function(rater_name, files) {
  read_csv(file.path("SpreadSheets", files$match)) %>%
    slice(-1) %>%  # removes instruction row (row 2 from the sheet)
    mutate(across(where(is.character), ~ str_replace_all(., '^"|"$', ""))) %>%
    mutate(Rater = rater_name)
})

Matches <- bind_rows(Match_Dataframes_List)


############################ Additional Data for Comparing ############################ 

#Primary_SeasonOverview <- read_csv("SpreadSheets/Primary_SeasonOverview.csv")

Team_IDs <- read_csv("SpreadSheets/TeamIDs.csv")

#Stadiums <- read_csv("SpreadSheets/Stadiums.csv")

#MatchAssignments_WatchOrder <- read_csv("SpreadSheets/MatchAssignments_WatchOrder.csv")

StandingsByWeek <- read_csv("SpreadSheets/StandingsByWeek_Clean.csv")

FinalStandings <- read_csv("SpreadSheets/FinalSeasonStandings.csv")

############################ Defining Valid Entries ############################ 

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

############################ Cleaning Data | Obvious Human Error ############################ 

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
    as.character() %>%
    str_remove_all('^\"|\"$') %>%           # remove any surrounding quotes
    str_split(",\\s*") %>%
    map_chr(~ paste(
      str_trim(str_replace_all(.x, c(
        "Arn" = "Arm",
        "AH" = "Arm",
        "Hand" = "H",
        "Foot" = "Feet",
        "Back Torso" = "BT",
        "Front Torso" = "FT",
        "\\bLeg\\b" = "Legs",
        "Bt" = "BT",
        "Ft" = "FT"
      ))),
      collapse = ", "
    ))
}

clean_situation <- function(x) {
  case_when(
    x == "FEF" ~ "REF",
    x == "HK" ~ "GK",
    x == "PK" ~ "PEN",
    x == "Ref" ~ "REF",
    TRUE ~ x
  )
}

clean_haptic_ritual <- function(x) {
  x %>%
    as.character() %>%
    str_remove_all('^\"|\"$') %>%
    str_split(",\\s*") %>%
    map_chr(~ paste(
      str_trim(str_replace_all(.x, c(
        "LF!" = "LF1"
      ))),
      collapse = ", "
    ))
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

############################ Functions to check for invalid entries in data frame ############################ 

#First fxns for checking columns that had lists (read in strangely so needed extra love)

is_invalid_bodypart <- function(x) { #splits lists within cell to look at individual values (checks to make sure "FT, Arn" is flagged for "Arn")
  x %>%
    as.character() %>%                    # ensures all input is character
    str_split(",\\s*") %>%
    map_lgl(~ {
      parts <- .x
      if (any(is.na(parts)) || any(parts == "NA")) return(TRUE)  # flag both empty and NA values
      any(!parts %in% valid_body_parts)     # TRUE only if any part is invalid
    })
}

is_invalid_haptic_ritual <- function(x) { #splits lists within cell to look at individual values (flags things like "CR, BF" for "BF" which is not a valid haptic ritual)
  x %>%
    as.character() %>%
    str_split(",\\s*") %>%
    map_lgl(~ {
      parts <- .x
      if (any(is.na(parts)) || any(parts == "NA")) return(TRUE)
      any(!parts %in% valid_hapticrituals)
    })
}

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
############################ Trouble-Shooting | Human Input Error ############################ 

#Troubleshooting: Looking at each column and checking values to look for human error while inputting data during collection

cols <- c("Team", "TouchAction", "Reciprocal", "ToucherBodyPart", "ToucheeBodyPart", "Situation", "HapticRitual", "Visibility")

#Starting with Touches

unique_vals <- Touches %>%
  select(all_of(cols)) %>% 
  map(~ unique(.))

max_len <- max(lengths(unique_vals))
padded <- map(unique_vals, ~ { length(.) <- max_len; . })
unique_values_df <- as.data.frame(padded)

#determining which touchID and rater discrepancies are connected to
#"Show me the mistakes, so I can fix them or track who made them":

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
    mutate(Invalid_Fields = get_invalid_fields(pick(everything()))) %>%
    ungroup()
})

Touches_invalid_rater1 <- Touches_invalid_labeled[["Rater1"]]
Touches_invalid_rater2 <- Touches_invalid_labeled[["Rater2"]]
Touches_invalid_rater3 <- Touches_invalid_labeled[["Rater3"]]

############################ Final Data Frame Creation | Separation of Inter-rater Game Data ############################ 

#Creating the final data frames used for analysis
#Removes the data from matches not assigned to that specific rater. For example, if Rater 1 was assigned match 126, 
#then Rater 2 and 3 also watched it and have populated data for that match.
#this section removes that data created from rater 2 and 3 keeping only rater 1s info.
#Do this to both the Matches and Touches Dataframs (has to happen twice)

#Define match assignments
InterRaterMatches_Rater1 <- c("127", "115", "26", "87") #Paige
InterRaterMatches_Rater2 <- c("114", "39", "163", "42") #Tobi
InterRaterMatches_Rater3 <- c("95", "150", "38", "160") #Simon

#Combine all 12 interrater matches with rater mapping
interrater_assignments <- tibble(
  SeasonMatchNumber = c(InterRaterMatches_Rater1, InterRaterMatches_Rater2, InterRaterMatches_Rater3),
  AssignedRater = c(rep("Rater1", length(InterRaterMatches_Rater1)),
                    rep("Rater2", length(InterRaterMatches_Rater2)),
                    rep("Rater3", length(InterRaterMatches_Rater3)))
)

#SeasonMatchNumber imported differently, need to change to same character thingy
interrater_assignments <- interrater_assignments %>%
  mutate(SeasonMatchNumber = as.character(SeasonMatchNumber))

Touches <- Touches %>%
  mutate(SeasonMatchNumber = str_remove(as.character(SeasonMatchNumber), "^0+")) #removes leading zeros

Matches <- Matches %>%
  mutate(SeasonMatchNumber = str_remove(as.character(SeasonMatchNumber), "^0+")) #removes leading zeros

#make new data frame from Touches, if row contains InterRaterMatches_Rater1 and the word rater 1, keep it, otherwise delete row

Touches_final <- Touches %>% #This df has all matches, but excludes the repeated matches watched for inter-rater study except by the rater they were originally assigned for
  anti_join(interrater_assignments, by = c("SeasonMatchNumber" = "SeasonMatchNumber")) %>%
  bind_rows(
    Touches %>%
      inner_join(interrater_assignments, by = c("SeasonMatchNumber")) %>%
      filter(Rater == AssignedRater)
  )

#Same as above but for matches now (performance data for each match)
Matches_final <- Matches %>%
  anti_join(interrater_assignments, by = c("SeasonMatchNumber" = "SeasonMatchNumber")) %>%
  bind_rows(
    Matches %>%
      inner_join(interrater_assignments, by = c("SeasonMatchNumber")) %>%
      filter(Rater == AssignedRater)
  )


############################ Inter-rater Game Data ############################ 

#Creation of data frames for Inter-rater data analysis
#Simply looks at all matches that were watched by multiple people

Touches_interrater <- Touches %>%
  inner_join(interrater_assignments, by = "SeasonMatchNumber")

Matches_interrater <- Matches %>%
  inner_join(interrater_assignments, by = "SeasonMatchNumber")


