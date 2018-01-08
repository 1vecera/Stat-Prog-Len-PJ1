##### Last modified by Jessie at 1:00am on 13 Dec
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
con <- dbConnect(SQLite(), dbname="database.sqlite")

dbListTables(con)    #FYI
## [1] "Country"         "League"          "Match"           "Player"         
## [5] "Player_Stats"    "Team"            "sqlite_sequence"

League <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
Match <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
Player <- tbl_df(dbGetQuery(con,"SELECT * FROM Player"))
Player_Attributes <- tbl_df(dbGetQuery(con, "SELECT * FROM Player_Attributes"))
Team <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
Team_Attributes <- tbl_df(dbGetQuery(con, "SELECT * FROM Team_Attributes"))

# convert data types and remove NAs for each table

### dealing with table PLAYER

summary(Player)
Player$birthday <- as.Date(Player$birthday)

# dealing with table PLAYER_ATTRIBUTE
summary(Player_Attributes)
colnames(Player_Attributes)[4] <- "date_recorded"
Player_Attributes$date_recorded <- as.Date(Player_Attributes$date_recorded)

# Just check if some records are constantly missing
Player_missing_stats<- data.frame(Player_Attributes$id[is.na(Player_Attributes$overall_rating)==TRUE])
colnames(Player_missing_stats) <- c("overall_rating")
Player_missing_stats$potential <- Player_Attributes$id[is.na(Player_Attributes$potential)==TRUE]
Player_missing_stats$crossing <- Player_Attributes$id[is.na(Player_Attributes$crossing)==TRUE]
Player_missing_stats$finishing <- Player_Attributes$id[is.na(Player_Attributes$finishing)==TRUE]
Player_missing_stats$heading_accuracy <- Player_Attributes$id[is.na(Player_Attributes$heading_accuracy)==TRUE]
Player_missing_stats$short_passing <- Player_Attributes$id[is.na(Player_Attributes$short_passing)==TRUE]
Player_missing_stats$dribbling <- Player_Attributes$id[is.na(Player_Attributes$dribbling)==TRUE]
Player_missing_stats$free_kick_accuracy <- Player_Attributes$id[is.na(Player_Attributes$free_kick_accuracy)==TRUE]
Player_missing_stats$long_passing <- Player_Attributes$id[is.na(Player_Attributes$long_passing)==TRUE]
Player_missing_stats$ball_control <- Player_Attributes$id[is.na(Player_Attributes$ball_control)==TRUE]
Player_missing_stats$acceleration <- Player_Attributes$id[is.na(Player_Attributes$acceleration)==TRUE]
Player_missing_stats$sprint_speed <- Player_Attributes$id[is.na(Player_Attributes$sprint_speed)==TRUE]
Player_missing_stats$reactions <- Player_Attributes$id[is.na(Player_Attributes$reactions)==TRUE]
Player_missing_stats$shot_power <- Player_Attributes$id[is.na(Player_Attributes$shot_power)==TRUE]
Player_missing_stats$stamina <- Player_Attributes$id[is.na(Player_Attributes$stamina)==TRUE]
Player_missing_stats$strength <- Player_Attributes$id[is.na(Player_Attributes$strength)==TRUE]
Player_missing_stats$long_shots <- Player_Attributes$id[is.na(Player_Attributes$long_shots)==TRUE]
Player_missing_stats$aggression <- Player_Attributes$id[is.na(Player_Attributes$aggression)==TRUE]
Player_missing_stats$interceptions <- Player_Attributes$id[is.na(Player_Attributes$interceptions)==TRUE]
Player_missing_stats$positioning <- Player_Attributes$id[is.na(Player_Attributes$positioning)==TRUE]
Player_missing_stats$penalties <- Player_Attributes$id[is.na(Player_Attributes$penalties)==TRUE]
Player_missing_stats$marking <- Player_Attributes$id[is.na(Player_Attributes$marking)==TRUE]
Player_missing_stats$standing_tackle <- Player_Attributes$id[is.na(Player_Attributes$standing_tackle)==TRUE]
Player_missing_stats$gk_diving <- Player_Attributes$id[is.na(Player_Attributes$gk_diving)==TRUE]
Player_missing_stats$gk_handling <- Player_Attributes$id[is.na(Player_Attributes$gk_handling)==TRUE]
Player_missing_stats$gk_kicking <- Player_Attributes$id[is.na(Player_Attributes$gk_kicking)==TRUE]
Player_missing_stats$gk_positioning <- Player_Attributes$id[is.na(Player_Attributes$gk_positioning)==TRUE]
Player_missing_stats$gk_reflexes <- Player_Attributes$id[is.na(Player_Attributes$gk_reflexes)==TRUE]
summary(Player_missing_stats)

#get rid of those rows which are missing a substantial among of stats constantly
Player_Attributes <- subset(Player_Attributes, is.na(Player_Attributes$positioning) == FALSE)
summary(Player_Attributes)

#how about the other portion of missing values?
Player_missing_stats<- data.frame(Player_Attributes$id[is.na(Player_Attributes$volleys)==TRUE])
colnames(Player_missing_stats) <- c("volleys")
Player_missing_stats$curve <- Player_Attributes$id[is.na(Player_Attributes$curve)==TRUE]
Player_missing_stats$agility <- Player_Attributes$id[is.na(Player_Attributes$agility)==TRUE]
Player_missing_stats$balance <- Player_Attributes$id[is.na(Player_Attributes$balance)==TRUE]
Player_missing_stats$jumping <- Player_Attributes$id[is.na(Player_Attributes$jumping)==TRUE]
Player_missing_stats$vision <- Player_Attributes$id[is.na(Player_Attributes$vision)==TRUE]
Player_missing_stats$sliding_tackle <- Player_Attributes$id[is.na(Player_Attributes$sliding_tackle)==TRUE]

#....and explore them
who_miss_some_records <- subset(Player_Attributes, is.na(Player_Attributes$curve) == TRUE)
summary(as.factor(Player_Attributes$player_fifa_api_id))
summary(as.factor(who_miss_some_records$player_fifa_api_id))

# it looks like rows containing missing values will not be helpful for PCA anyways
# so we could just remove them
Player_Attributes <- subset(Player_Attributes, is.na(Player_Attributes$curve) == FALSE)
summary(Player_Attributes)

#turn "preferred foot" into a binary variable
Player_Attributes$preferred_foot_is_right <- as.numeric(as.factor(Player_Attributes$preferred_foot))-1
Player_Attributes$preferred_foot <-NULL

# not sure what to do with working rates, which has 8 and 19 levels respectively
# and the relative relationship between levels is not clear
Player_Attributes$attacking_work_rate <- as.factor(Player_Attributes$attacking_work_rate)
Player_Attributes$defensive_work_rate <- as.factor(Player_Attributes$defensive_work_rate)
levels(Player_Attributes$attacking_work_rate)
levels(Player_Attributes$defensive_work_rate)

##### joining Player & Player_Attribute to form Player_all
Player_Attributes$id = NULL
Player_all <- inner_join(Player, Player_Attributes)
Player_all$attacking_work_rate <- NULL
Player_all$defensive_work_rate <- NULL
Player_all$age <- round(difftime(Player_all$date_recorded, Player_all$birthday, units = "weeks") /52)
Player_all$BMI <- round((Player_all$weight * 0.453592) / (Player_all$height/ 100)^2, digits = 3)

summary(Player_all)
str(Player_all)
rm(Player_missing_stats)
rm(who_miss_some_records)
rm(Player)
rm(Player_Attributes)

##### joining Team & Team_Attribute to form Team_all
Team_Attributes$id = NULL
Team_all <- inner_join(Team, Team_Attributes)

### dealing with table MATCH
summary(Match)

# remove country_id, match_api_id and all coordinates (not interesting)
Match$country_id <- NULL
Match$match_api_id <- NULL
colnames(Match)[10:53]
Match[,10:53] <- NULL

#variable types
Match$season <- as.factor(Match$season)
Match$stage <- as.factor(Match$stage)
summary(Match$stage[as.numeric(Match$season)==3])   #there are in total 38 stages in a season

#convert match date into the type Date
Match$date <- as.Date(Match$date)
summary(Match$date)
summary(Match$date[as.numeric(Match$season)==3])   #each season start from July to May next year

#which seasons do not have data about team composition?
summary(Match$season[is.na(Match$home_player_1)==TRUE])
Match_without0809 <- subset(Match, as.character(season)!="2008/2009")   #>800 NAs for player info
summary(Match_without0809)

#subset Match table to further analyse the messy data
Match_all_stats <- subset(Match, is.na(goal)==FALSE)
summary(Match_all_stats)
Match_without0809[c("goal","shoton", "shotoff", "foulcommit", "card", "cross", "corner", "possession")] <- NULL   #9887 NAs
Match_without0809[c("B365H","B365D", "B365A", "BWH", "BWD", "BWA", "IWH", "IWD","IWA", "LBH", "LBD", "LBA", "PSH", "PSD", "PSA", "WHH", "WHD", "WHA", "SJH", "SJD", "SJA")] <- NULL
Match_without0809[c("VCH","VCD", "VCA", "GBH", "GBD", "GBA", "BSH", "BSD","BSA")] <- NULL
summary(Match_without0809)

saveRDS(Match_without0809, "Match.rds")
saveRDS(Player_all, "Player_all.rds")


