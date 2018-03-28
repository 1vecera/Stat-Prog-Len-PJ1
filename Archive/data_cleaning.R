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

Match = tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
Player = tbl_df(dbGetQuery(con,"SELECT * FROM Player"))
Player_Attributes = tbl_df(dbGetQuery(con, "SELECT * FROM Player_Attributes"))

# convert data types and remove NAs for each table

### dealing with table PLAYER
Player$birthday = as.Date(Player$birthday)


### dealing with table PLAYER_ATTRIBUTE

colnames(Player_Attributes)[4] = "date_recorded"
Player_Attributes$date_recorded = as.Date(Player_Attributes$date_recorded)
  # check if some data missed certain attribute constantly
  # gather all columns with 836 NA's
Player_Attributes$attacking_work_rate = NULL     # Ambiguous levels
Player_Attributes$defensive_work_rate = NULL     # Ambiguous levels
group1 = Player_Attributes[unlist(lapply(Player_Attributes, function(X) sum(is.na(X))==836))]
  # get a data frame of one rows containing
test = subset(group1, is.na(group1$overall_rating))
  # and oberve if all values are NA
summary(test)
  # gather all columns with 2713 NA's
group2 = Player_Attributes[unlist(lapply(Player_Attributes, function(X) sum(is.na(X))==2713))]
  # get a data frame of one rows containing
test = subset(group1, is.na(group1$vision))
  # and oberve if all values are NA
summary(test)
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
saveRDS(Player_all, "Player_all.rds")

##### joining Team & Team_Attribute to form Team_all
Team_Attributes$id = NULL
Team_all = inner_join(Team, Team_Attributes)

### dealing with table MATCH

  # remove country_id, match_api_id and all coordinates (not interesting)
Match$country_id = NULL
Match$match_api_id = NULL
colnames(Match)[10:53]
Match[,10:53] = NULL
Match[,32:39] = NULL
  #variable types
Match$season = as.factor(Match$season)
Match$stage = as.factor(Match$stage)
  #convert match date into the type Date
Match$date = as.Date(Match$date)

summary(Match %>% group_by(season))
library(purrr)
num_na = function(column) {return(sum(is.na(column)))}
lapply(Match, num_na)
avg_num_na = function(column) {return(sum(is.na(column))/22)}
Match[,c(3,10:31)] %>% split(.$season) %>% map(avg_num_na)

#which seasons do not have data about team composition?
Match_without0809 = subset(Match, as.character(season)!="2008/2009")   #>800 NAs for player info
# saveRDS(Match_without0809, "Match.rds")
Match_for_prediction = Match_without0809[,1:31]

# read in the principal components for all players as player_pc
playerPC = readRDS("Data_Players_Match_predictions.rds")

# create new columns in Match dataframe for the principal components
pc_num = 1:4
home_colnames = {}
away_colnames = {}
for (i in 1:11) {
  for(j in pc_num){  colname = (paste("homeplayer", i, "pc", j, sep = "_"))
                     home_colnames = append(home_colnames, colname)
                     colname = (paste("awayplayer", i, "pc", j, sep = "_"))
                     away_colnames = append(away_colnames, colname)
  } }

Match_for_prediction[c(home_colnames, away_colnames)] = NA


# a function to be applied to each columns of positions
each = data.frame(matrix(vector(),nrow(Match_for_prediction),4))
each[id_exist,]=getPC(id)

gatherPC = function(id) {
  #if (is.na(id)) { each[nrow(each)+1, ] = rep(NA,4) }
  #else { each[nrow(each)+1, ] = getPC(id)
}

apply(Match_for_prediction["home_player_1"],1, function(id) gatherPC())

getPC(Match_for_prediction["away_player_3"][[1]])

getPC <- function(apiID) { PC = {}
                            findFIFAid = subset(Player_all, player_api_id == apiID)[1,4]
                            print(findFIFAid)
                            PC = subset(playerPC, player_fifa_api_id == findFIFAid[[1]])[,2:5]
                            return(PC)}

Match_clean = apply(Match_for_prediction, 1, addPC)
