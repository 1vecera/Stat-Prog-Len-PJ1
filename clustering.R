##### Last modified by Jessie at 15:30pm on 17 Dec
##### 
##### 
##### This code did the clustering of player data. 
##### Please change the working directory to run it.
##### Player.rds is the data storage file obtained from data_cleaning.R
##### 

### Only analyze the cluster with the latest record of each player

rm(list = ls())

library(cluster)
library(fpc)



# setwd("/Users/jessiehsieh/Documents/Programming/Statical Programming Language/Seminar Paper/")
Player = readRDS("Player.rds")

unique_player = unique(Player$player_api_id)
Player_latest = subset(Player, date_recorded < 01-01-2017)   #just to get an empty dataset

for (i in 1:length(unique_player)){
    this_player = subset(Player, player_api_id == unique_player[i])
    latest_record = subset(this_player, date_recorded == max(date_recorded))
    Player_latest = rbind(Player_latest, latest_record)
    print(paste0("combined row ", i, " out of", length(unique_player)))    
}

# save the latest records
saveRDS(Player_latest, "Player_latest.rds")
Player_latest = readRDS("Player_latest.rds")



# Determine number of clusters by within group sum of squares
Player_latest[c("player_name", "date_recorded", "birthday", "player_fifa_api_id")] = NULL
Player_latest$age = as.numeric(Player_latest$age)
Player_latest[,3:42] = data.frame(scale(Player_latest[,3:42]))

wss = (nrow(Player_latest)-1)*sum(apply(Player_latest,2,var))
for (i in 2:15) wss[i] = sum(kmeans(Player_latest, 
                                     centers=i)$withinss)
bss = (nrow(Player_latest)-1)*sum(apply(Player_latest,2,var))
for (i in 2:15) bss[i] = sum(kmeans(Player_latest, 
                                     centers=i)$betweenss)

plot(1:15, wss/(wss+bss), type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares to TSS")
abline(a = 0.2, b = 0, col = "red")

# K-Means Cluster Analysis
fit = kmeans(Player_latest[,3:42], 3) # 5 cluster solution
# get cluster means 
aggregate(Player_latest[,3:42],by=list(fit$cluster),FUN=mean)
# append cluster assignment
Player_kmean = data.frame(Player_latest, fit$cluster)

# vary parameters for most readable graph
clusplot(Player_kmean, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

plotcluster(Player_kmean, fit$cluster)

# how many of each group?
table(Player_kmean$fit.cluster)


# Hierarchical clustering

Player.dist = dist(Player_latest[,-2])
Player.hclust = hclust(Player.dist)
plot(Player.hclust)
grouping = cutree(Player.hclust,3)
Player_hclust = data.frame(Player_latest,grouping)
table(grouping)

# evaluating hclust
