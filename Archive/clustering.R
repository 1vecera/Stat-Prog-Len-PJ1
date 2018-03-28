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
library(dplyr)
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}
if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("dendextend")) {
  install.packages("dendextend", dependencies = TRUE)
  library(dendextend)
}


setwd("/Users/jessiehsieh/Documents/Programming/SPL/2018")
# Player = readRDS("Player.rds")

# unique_player = unique(Player$player_api_id)
# Player_latest = subset(Player, date_recorded < 01-01-2017)   #just to get an empty dataset

# for (i in 1:length(unique_player)){
  #   this_player = subset(Player, player_api_id == unique_player[i])
  #   latest_record = subset(this_player, date_recorded == max(date_recorded))
  #   Player_latest = rbind(Player_latest, latest_record)
  #   print(paste0("combined row ", i, " out of", length(unique_player)))    
# }

# save the latest records
# saveRDS(Player_latest, "Player_latest.rds")
Player_new = readRDS("Player_Attributes_quant_mean.rds")


#Player_all = readRDS("Player_all.rds")
#Player_all[,c(1,3,5:10,39:44)] = NULL
#Player_new = aggregate(Player_all, by = list(Player_all$player_api_id), FUN = mean)[-1]

# Determine number of clusters by within group sum of squares
#Player_new[,2:29] = data.frame(scale(Player_new[,2:29]))


wss = (nrow(Player_new)-1)*sum(apply(Player_new,2,var))
bss = (nrow(Player_new)-1)*sum(apply(Player_new,2,var))
for (i in 2:15) { 
  result = kmeans(Player_new, 
                  iter.max = 15, centers=i)
  wss[i] = result$tot.withinss
  bss[i] = sum(result$betweenss)
}

plot(1:15, wss/(bss+wss), type="b", main = "Choosing K in kmean clustering",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares to TSS")
abline(a = 0.2, b = 0, col = "red")


# K-Means Cluster Analysis
fit = kmeans(Player_new[2:29], 3) # 3 cluster solution
# append cluster assignment
Player_kmean = data.frame(Player_new, fit$cluster)
# get cluster means 
means = aggregate(Player_kmean[2:30],by=list(fit$cluster),FUN=mean)[,-1]

# visualizing the means of the clusters with a heatmap

means_matrix = data.matrix(means[,1:28])
my_palette = brewer.pal(9,"Blues")
par(cex.main=1) 
heatmap.2(means_matrix,
          Rowv=NA, Colv=NA,
          main = "Aggregated means from kmeans", # heat map title
          col=my_palette,
          scale="none", 
          key=TRUE, 
          symkey=FALSE, 
          labRow = c("Group 1", "Group 2", "Group 3"),
          cexRow=1,
          density.info="none", 
          trace="none",
          margins=c(8,4)
          )
dev.off()               # close the PNG device


# vary parameters for most readable graph
clusplot(Player_kmean[2:30], fit$cluster, color=TRUE, shade=TRUE, 
          lines=0)

plotcluster(Player_kmean[2:30], fit$cluster)

# how many of each group?
table(Player_kmean$fit.cluster)


# Hierarchical clustering

Player.dist = dist(Player_new[,-1], method = "euclidean")
Player.hclust = hclust(Player.dist, method = "complete")

# better plots
#d_iris = dist(iris2) # method="man" # is a bit better
#hc_iris = hclust(d_iris)
#iris_species = rev(levels(iris[,5]))

dend = as.dendrogram(Player.hclust)
# order it the closest we can to the order of the observations:
dend = rotate(dend, 1:10582)

# Color the branches based on the clusters:
dend = color_branches(dend, k=3, groupLabels = c("Group 2", "Group 1", "Group 3"))

# We hang the dendrogram a bit:
dend = hang.dendrogram(dend,hang_height=0.3)
# reduce the size of the labels:
# dend = assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend = set(dend, "labels_cex", 0.1)
# And plot:
par(mar = c(3,2,3,0.2)+0.1, bg = NA)
plot(cut(dend, h=170)$upper, 
     main = "Upper tree of dendrogram from hclust", 
     horiz =  TRUE,  nodePar = list(cex = .007))

# observe numbers of players in clusters at different levels of the tree
groupings = cutree(Player.hclust, c(2:8))
apply(groupings, 2,table)

Player_hclust = data.frame(Player_new,groupings[,"3"])
colnames(Player_hclust)[30] = "grouping"


# evaluating hclust
hclust_means = aggregate(Player_hclust[2:30],by=list(grouping),FUN=mean)[,-1]

means_matrix = data.matrix(hclust_means[,1:28])
my_palette = brewer.pal(9,"Oranges")
par(cex.main=1) 
heatmap.2(means_matrix,
          Rowv=NA, Colv=NA,
          main = "Aggregated means from hclust", # heat map title
          col=my_palette,
          scale="none", 
          key=TRUE, 
          symkey=FALSE, 
          labRow = c("Group 1", "Group 2", "Group 3"),
          cexRow=1,
          density.info="none", 
          trace="none",
          margins=c(8,4)
)
dev.off()               # close the PNG device

# find out who belongs to different groups in the clustering methods
Player_kmean$fit.hclust = Player_hclust$grouping
ambig_players = subset(Player_kmean[,c(1,30,31)], (fit.cluster-fit.hclust!=1)&(fit.cluster-fit.hclust!=-2))
ambig_players = inner_join(ambig_players, Player_names)
# most of them are midfielders