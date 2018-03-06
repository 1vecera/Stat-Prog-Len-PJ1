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
library ( RSQLite )
library ( dplyr )
library ( stringr )
library ( purrr )

con = dbConnect ( SQLite () , dbname =" database . sqlite ")
dbListTables (con )

Match = tbl_df ( dbGetQuery (con ," SELECT * FROM Match ") )
Player = tbl_df ( dbGetQuery (con ," SELECT * FROM Player ") )
Player_Attributes = tbl_df ( dbGetQuery (con , " SELECT * FROM
                                            Player_Attributes ") )

Player$birthday = as.Date( Player$birthday )
colnames ( Player_Attributes ) [4] = " date_recorded "
Player_Attributes$attacking_work_rate = NULL     # Ambiguous levels
Player_Attributes$defensive_work_rate = NULL     # Ambiguous levels
Player_Attributes$date_recorded = as.Date(Player_Attributes$date_recorded )
group1 = Player_Attributes [ unlist ( lapply ( Player_Attributes ,
                                                  function (X) sum(is.na(X) )== 836) ) ]
test = subset (group1 , is.na( group1$overall_rating ) )
summary ( test )
group2 = Player_Attributes [ unlist ( lapply ( Player_Attributes ,
                                                  function (X) sum(is.na(X) )== 2713) ) ]
test = subset (group1 , is.na( group1$vision ) )
summary ( test )

Player_Attributes = subset ( Player_Attributes , is.na(
  Player_Attributes$positioning ) == FALSE )
Player_Attributes = subset ( Player_Attributes , is.na(
  Player_Attributes$curve ) == FALSE )
Player_Attributes$preferred_foot_is_right = as.numeric (as.factor (
  Player_Attributes$preferred_foot ) ) -1

Player_Attributes$preferred_foot = NULL

Player_Attributes$id = NULL
Player_all = inner_join (Player , Player_Attributes )

Match$country_id = NULL
Match$match_api_id = NULL
colnames ( Match ) [10:53]
Match [ ,10:53] = NULL
Match$season = as.factor ( Match$season )
Match$stage = as.factor ( Match$stage )
Match$date = as.Date ( Match$date )

num_na = function ( column ) { return (sum (is.na( column ) ) ) }
lapply (Match , num_na )
avg_num_na = function ( column ) { return (sum(is.na( column ) ) /22) }
Match [ ,c (3 ,10:31) ] %>% split (. $season ) %>% map( avg_num_na )

Match_without0809 = subset (Match , as.character ( season ) !=" 2008/2009") 

```
