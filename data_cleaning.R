##### Last modified by Jessie at 1:00am on 15 Jan
##### 
##### Hi all, I have done some basic data preparation,
##### including converting variables into correct data types,
##### and corrected data types. P
##### Please first change the working directory (where your .sqlite file is) on line
##### then run all of them, and you will obtain the full datasets in .rds file

### 2 new features created: player age and player body mass index

# rm(list = ls()) NOTE DANIEL: HAZARD; DONT DO IN CODE 

library(RSQLite)
library(dplyr)
library(stringr)

# setwd("/Users/jessiehsieh/Documents/Programming/Statical Programming Language/Seminar Paper/"): NOTE DANIEL: Don't do here! It does not work for others, set manually or in PROEJCT

##### Before running further, please change this line to your own file directory for the .sqlite file:
con = dbConnect(SQLite(), dbname="database.sqlite")

dbListTables(con)    #FYI
## [1] "Country"         "League"          "Match"           "Player"         
## [5] "Player_Stats"    "Team"            "sqlite_sequence"

League = tbl_df(dbGetQuery(con,"SELECT * FROM League"))
Match = tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
Player = tbl_df(dbGetQuery(con,"SELECT * FROM Player"))
Player_Attributes = tbl_df(dbGetQuery(con, "SELECT * FROM Player_Attributes"))
Team = tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
Team_Attributes = tbl_df(dbGetQuery(con, "SELECT * FROM Team_Attributes"))

# convert data types and remove NAs for each table

### dealing with table PLAYER

Player$birthday = as.Date(Player$birthday)


### dealing with table PLAYER_ATTRIBUTE

colnames(Player_Attributes)[4] = "date_recorded"
Player_Attributes$date_recorded = as.Date(Player_Attributes$date_recorded)
  # get rid of those rows which are missing a substantial among of stats constantly
Player_Attributes = subset(Player_Attributes, is.na(Player_Attributes$positioning) == FALSE)
  # it looks like rows containing missing values will not be helpful for PCA anyways
  # so we could just remove them
Player_Attributes = subset(Player_Attributes, is.na(Player_Attributes$curve) == FALSE)
  #turn "preferred foot" into a binary variable
Player_Attributes$preferred_foot_is_right = as.numeric(as.factor(Player_Attributes$preferred_foot))-1
Player_Attributes$preferred_foot =NULL


##### joining Player & Player_Attribute to form Player_all

Player_Attributes$id = NULL
Player_all = inner_join(Player, Player_Attributes)
Player_all$attacking_work_rate = NULL     # Ambiguous levels
Player_all$defensive_work_rate = NULL     # Ambiguous levels
  # add two more features by subtracting columns: age, BMI of player
Player_all$age = round(difftime(Player_all$date_recorded, Player_all$birthday, units = "weeks") /52)
Player_all$BMI = round((Player_all$weight * 0.453592) / (Player_all$height/ 100)^2, digits = 3)


##### joining Team & Team_Attribute to form Team_all
Team_Attributes$id = NULL
Team_all = inner_join(Team, Team_Attributes)

### dealing with table MATCH

  # remove country_id, match_api_id and all coordinates (not interesting)
Match$country_id = NULL
Match$match_api_id = NULL
colnames(Match)[10:53]
Match[,10:53] = NULL
  #variable types
Match$season = as.factor(Match$season)
Match$stage = as.factor(Match$stage)
  #convert match date into the type Date
Match$date = as.Date(Match$date)

#which seasons do not have data about team composition?
Match_without0809 = subset(Match, as.character(season)!="2008/2009")   #>800 NAs for player info


saveRDS(Match_without0809, "Match.rds")
saveRDS(Player_all, "Player_all.rds")