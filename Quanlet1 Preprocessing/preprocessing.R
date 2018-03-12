if(!file.exists("database.sqlite")){
    print("Please download the database file from kaggle.com, unzip and put in working directory.")
    print("https://www.kaggle.com/hugomathien/soccer/data")
}

# load required packages
if (!require("RSQLite")) {
    install.packages("RSQLite", dependencies = TRUE)
    library(RSQLite)
}
if (!require("dplyr")) {
    install.packages("dplyr", dependencies = TRUE)
    library(dplyr)
}
if (!require("stringr")) {
    install.packages("stringr", dependencies = TRUE)
    library(stringr)
}
if (!require("purrr")) {
    install.packages("purrr", dependencies = TRUE)
    library(purrr)
}

# read the database file and look at all tables contained inside
con = dbConnect(SQLite(), dbname = "database.sqlite")
dbListTables(con)

# convert the tables needed into tibbles using simple queries
Match             = tbl_df(dbGetQuery(con, "SELECT * FROM Match"))
Player            = tbl_df(dbGetQuery(con, "SELECT * FROM Player"))
Player_Attributes = tbl_df(dbGetQuery(con, "SELECT * FROM
                                            Player_Attributes"))

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