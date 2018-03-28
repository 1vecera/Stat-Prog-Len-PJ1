[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Preprocessing** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: Preprocessing

Published in: Statistical Programming Languages - Student Project on Analaysis of a FIFA Data set

Description: 'In this quantlet we read the data from a database file into tibble data frames, convert variable types, subset the interesting columns and rows and join data frames for further analysis. '

Keywords: tibble, group_by, inner_join

Author: Jessie Hsieh

See also: other quantlets in this project

Submitted: 14.03.2018

```


### R Code:
```r
if(!file.exists("database.sqlite")){
  print("Please download the database file from kaggle.com, unzip and put in working directory.")
print("https://www.kaggle.com/hugomathien/soccer/data")
}

# load/Instal required packages
for (i in c("RSQLite","dplyr","stringr","purrr")){
  if (!require(i,character.only = T)) {
    install.packages(i, dependencies = T)
    library(i,character.only = T)
  } 
}

# prepare a list of the leagues that we are interested in, for predictability

leagues           = data.frame( league_name = character(), 
                                league_id = integer(), 
                                stringsAsFactors = FALSE
)
leagues[1, ]      = c("England", 1729)
leagues[2, ]      = c("France", 4769)
leagues[3, ]      = c("Germany", 7809)
leagues[4, ]      = c("Italy", 10257)
leagues[5, ]      = c("Spain", 21518)


if(file.exists("database.sqlite")){

    # read the database file and look at all tables contained inside
    con = dbConnect(SQLite(), dbname = "Quanlet1 Preprocessing/database.sqlite")
    dbListTables(con)

    # convert the tables needed into tibbles using simple queries
    Match             = tbl_df( dbGetQuery( con, "SELECT * FROM Match"))
    Player            = tbl_df( dbGetQuery( con, "SELECT * FROM Player"))
    Player_Attributes = tbl_df( dbGetQuery( con, "SELECT * FROM Player_Attributes"))
    all.leagues       = dbGetQuery( conn = con, statement = "SELECT * FROM 'League' ")
    
    league.ids        = paste(leagues[, 2], collapse = ", ")
    table             = dbGetQuery( conn = con, 
                                    statement = paste( "SELECT * FROM Match WHERE league_id IN (", 
                                                       league.ids, 
                                                       ")",
                                                       sep = "")
                                    )
    
    print("As database file exists, raw data loaded from database file.")
    
} else {
    Match             = tbl_df(readRDS("Quanlet1 Preprocessing/raw_Match.rds"))
    Player            = tbl_df(readRDS("Quanlet1 Preprocessing/raw_Player.rds"))
    Player_Attributes = tbl_df(readRDS("Quanlet1 Preprocessing/raw_Player_Attributes.rds"))
    
    table             = readRDS("Quanlet1 Preprocessing/raw_table.rds")
    all.leagues       = readRDS("Quanlet1 Preprocessing/all_leagues.rds")
    
    print("As database file doesn't exist, raw data loaded from rds file.")
}

# prepare the league and match data frame for predictability
saveRDS( all.leagues, "Quanlet1 Preprocessing/all_leagues.rds")
saveRDS( leagues, "Quanlet1 Preprocessing/leagues.rds")

matches = table[, c("id", "league_id", "season", "home_team_api_id", "away_team_api_id", 
                    "B365H", "B365D", "B365A")]
saveRDS( matches, "Quanlet1 Preprocessing/matches_predictability.rds")

    
# correct data type and make colnames intuitive
Player$birthday                 = as.Date(Player$birthday)
colnames(Player_Attributes)[4]  = "date_recorded"
Player_Attributes$date_recorded = as.Date(Player_Attributes$date_recorded)

# drop columns due to ambiguous levels
Player_Attributes$attacking_work_rate = NULL  
Player_Attributes$defensive_work_rate = NULL  

# check in summary of test if the player attribute records are consistently missing
group1 = Player_Attributes[unlist(lapply(Player_Attributes, function(X) sum(is.na(X)) == 836))]
test   = subset(group1, is.na(group1$overall_rating))

group2 = Player_Attributes[unlist(lapply(Player_Attributes, function(X) sum(is.na(X)) == 2713))]
test   = subset(group2, is.na(group2$vision))

# remove records which are problematic as confirmed above
Player_Attributes = subset(Player_Attributes, is.na(Player_Attributes$overall_rating) == FALSE)
Player_Attributes = subset(Player_Attributes, is.na(Player_Attributes$vision)         == FALSE)

# make this feature binary
Player_Attributes$preferred_foot_is_right                                              = 0
Player_Attributes$preferred_foot_is_right[Player_Attributes$preferred_foot == "right"] = 1
Player_Attributes$preferred_foot                                                       = NULL

# use an inner_join to get a dataframe which contains info from both Player and Player_Attributes
Player_Attributes$id = NULL
Player_all           = inner_join(Player, Player_Attributes)

# drop columns which are not needed
Match$country_id   = NULL
Match$match_api_id = NULL
Match[, 10:53]     = NULL

# appropriate data types
Match$season = as.factor(Match$season)
Match$stage  = as.factor(Match$stage)
Match$date   = as.Date(Match$date)

# check number of missing values in all columns, so NAs exist only in positions and betting odds
num_na = function(column) {
    return(sum(is.na(column)))
}
lapply(Match, num_na)

# check number of average number of missing values of players in each season
avg_num_na = function(column) {
    return(sum(is.na(column))/22)
}
Match[, c(3, 10:31)] %>% split(.$season) %>% map(avg_num_na)

# get rid of season 2008 to 2009 due to too many NAs
Match_without0809 = subset(Match, as.character(season) != " 2008/2009")

# save useful data frames
saveRDS(Player[,3:4], "Quanlet1 Preprocessing/Player_names.rds")
saveRDS(Match_without0809, "Quanlet1 Preprocessing/Match_without0809.rds")
saveRDS(Player_all, "Quanlet1 Preprocessing/Player_all.rds")
```
