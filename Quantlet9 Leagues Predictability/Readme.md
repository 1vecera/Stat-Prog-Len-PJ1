
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_Analaysis_of_a_Fifa_DataSet_Leagues_Predictability** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: SPL_Analysis_of_a_Fifa_DataSet_Leagues_Predictability

Published in: Statistical Programming Languages - Student Project on Analysis of a FIFA Data set

Description: 'In this quantlet we are calculating the predictability of chosen leagues. We are using entropy as a proxy of the level of competitiveness of a certain league or a match. The entropy is calculated on the basis of probabilities which different outcomes of the matches have, which are, in its turn, retrieved from the bidding odds provided by a gambling agency. Datasets with mean entropy by leagues and teams were also calculated and exported to a .rds file'

Keywords: Entropy, competitivness, predictability

Author: Kseniia Ovadenko

See also: other quantlets in this project

Submitted: 14.03.2018

```


### R Code:
#### Retreiving the information about leagues
```{r}
library("RSQLite")
library("entropy")
library("rio")

con = dbConnect(drv = RSQLite::SQLite(), dbname = "database.sqlite")
all.leagues = dbGetQuery(conn = con, statement = "SELECT * FROM 'League' ")
print(all.leagues)
```

#### Picking some leagues

```{r}
leagues = data.frame(league_name = character(), league_id = integer(), 
    stringsAsFactors = FALSE)
leagues[1, ] = c("England", 1729)
leagues[2, ] = c("France", 4769)
leagues[3, ] = c("Germany", 7809)
leagues[4, ] = c("Italy", 10257)
leagues[5, ] = c("Spain", 21518)

leagues
```


#### Retreiving odds for all matches in selected leagues


```{r}
league.ids = paste(leagues[, 2], collapse = ", ")
table = dbGetQuery(conn = con, statement = paste("SELECT * FROM Match WHERE league_id IN (", 
    league.ids, ")", sep = ""))
matches = table[, c("id", "league_id", "season", "home_team_api_id", "away_team_api_id", 
    "B365H", "B365D", "B365A")]
dbDisconnect(con)
head(matches)
```

#### For each match computing entropy of odds

```{r}
MatchEntropy = function(row) {
    odds = row[c("B365H", "B365D", "B365A")]
    probs = vapply(as.numeric(odds), function(x) 1/x, 1)
    probs_sum = sum(probs, na.rm = TRUE)
    norm_probs = vapply(probs, function(x) x/probs_sum, 1)
    entropy(norm_probs, na.rm = TRUE)
}

matches["entropy"] = apply(matches, 1, MatchEntropy)
head(matches)
```

#### Computing mean entropy across all leagues and individual teams and exporting data

```{r}
home.matches = subset(matches, select = c("league_id", "home_team_api_id", 
    "season", "entropy"))
names(home.matches)[2] = "team_id"

guest.matches = subset(matches, select = c("league_id", "away_team_api_id", 
    "season", "entropy"))
names(guest.matches)[2] = "team_id"
all.matches = rbind(home.matches, guest.matches)

team.entropy = aggregate(all.matches$entropy, list(league_id = all.matches$league_id, 
    team = all.matches$team_id, season = all.matches$season), FUN = function(x) mean(x, 
    na.rm = TRUE))

league.entropy = aggregate(matches$entropy, list(league_id = matches$league_id, 
    season = matches$season), FUN = function(x) mean(x, na.rm = TRUE))

print(league.entropy)

# set name to entropy column
names(team.entropy)[4] = "ent"
names(league.entropy)[3] = "ent"

# add human-readable league names
team.entropy = merge(team.entropy, leagues, by = "league_id")
league.entropy = merge(league.entropy, leagues, by = "league_id")
head(team.entropy)
head(league.entropy)
export(team.entropy, "team_entropy.rds")
export(league.entropy, "league_entropy.rds")
```