# Soccer_Touch
Soccer Touch Project Innsbruck Liebel Fulbright

This project looks at physical touch between teammates. The data was collected by 3 individual raters.

The Rscripts built out so far are listed and described below:

-------------------------------------------------------------------------------------------------------------

# Data_Management.R

  This scripts goal is to cleanup, double-check, and merge data sheets accumulated on google sheets throughout data collection.
  
  Reach out to paigeliebel@gmail.com to gain access to the raw data for this project.
  
  Each rater has two data collection sheets: Touch_RaterIdentifier and Match_RaterIdentifier
  
  The term RaterIdentifier refers to the moniker given to each rater. 
  
  *Touch_RaterIdentifier* sheets = contain the coded physical touches per match (~5000 rows per rater)
  
  *Match_RaterIndtifier* sheets = contain individual match performance data collected by the raters (~140 rows per rater)
  
  These sheets are merged together to create the complete data sets called "Touches" and "Matches"

  *Touches* = dataframe of all touches collected by all raters
  
    Columns in the Touches dataframe include such things as "TouchID", "SeasonMatchNumber", "Time", "Team", "Touch Action", "Reciprocal", "ToucherNumber", "ToucherBodyPart", "Situation", "Duration", "Haptic Ritual" and so on...
    
  *Matches* = dataframe of all match performance sheets collected by all raters
  
    Columns in the Matches dataframe include such things as "TeamName", "MatchID", "Outcome", "MatchLength", "Starters", "ShotsOnGoal", "TotalPasses", "CornersFor", "FoulsPerpetrated", "OwnGoals", "PKTime and so on...

  Additionally, there are extra dataframes to help with connecting data and extra information to expand on the Matches and Touches:
  
    Note that these dataframes are mostly connected to eachother by the variables called "TouchID", "SeasonMatchNumber", and "TeamID"
  
  *Primary_SeasonOverview* = dataframe of a season overview, assigning matches to match_numbers and containing data such as weather, date, kickoff time and stadium played
  
    Columns in the Primary_SeasonOverview nclude such things as "SeasonMatchNumber", "City", "LocalKickOffTime", "MatchAttendance", "Temperature", "Weather", "Home_TeamID", "HomeTeam_MatchID", "First_or_Second_Meeting" and so on...
    
  *Team_IDs* = dataframe that defines the TeamIDs paired to each team
  
  *Stadiums* = dataframe that assigns teams to their corresponding stadiums and stadium capacities

  *MatchAssignments_WatchOrder* = dataframe that provides the order in which matches were watched by each rater

  *StandingsByWeek* = dataframe pulled from Wikipeadia that shows the standing of each team at the start of each competition weekend/week
  
    WikiSource: http://en.wikipedia.org/wiki/2024_National_Women%27s_Soccer_League_season

  *FinalStandings* = dataframe that has final season stats pulled from NWSL website
-------------------------------------------------------------------------------------------------------------

# Core_Hypothesis.R

  This script conducts the analysis for the Core Hypothesis and its direct sub-parts.

  Core Hypothesis: Prosocial touches between teammates may serve as an indicator for overall team cohesion. Therefore, we propose that teams with a greater frequency of these interactions will secure higher positions in the season’s final standings.
  
  Prosocial touches = all haptic rituals excluding: TA, CO, NEG
  
  Situations excluded = GF, GA, SUB

  Analysis GamePlan: Use of simple linear regression/multiple regression analysis: model relationship between prosocial touch frequency and final standings, controlling for factors such as match length

-------------------------------------------------------------------------------------------------------------
  
