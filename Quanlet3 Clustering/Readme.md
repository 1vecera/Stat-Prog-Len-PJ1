[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **Clustering** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml
Name of Quantlet: Clustering

Published in: Statistical Programming Languages - Student Project on Analaysis of a FIFA Dataset

Description: 'In this quantlet, we used k-mean and agglomerative clustering methods with player attributes to assign groups to the players. Graph, dendrogram and heat maps were plotted to illustrate the determination of number of clusters, orders of joining clusters and attributes’ mean within clusters respectively. Referring to the names of the player, we can verify the relationship between clusters and their positions.’

Keywords: kmeans, hclust, dendrogram, heat map

Author: Jessie Hsieh

See also: other quantlets in this project

Submitted: 14.03.2018
```


### R Code:

### hclust :
```{r}
# load the required package, install if necessary

if (!require("cluster")) {
  install.packages("cluster", dependencies = TRUE)
  library(cluster)
}
if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}
if (!require("dendextend")) {
  install.packages("dendextend", dependencies = TRUE)
  library(dendextend)
}

# read in the mean player attributes prepared in advance
# this dataset was used in principal component analysis

Player_new = readRDS("Player_Attributes_quant_mean.rds")

# get the Euclidean distance between players
Player.dist = dist(Player_new[,-1], method = "euclidean")

# generate hclust object using the function with complete linkage method
Player.hclust = hclust(Player.dist, method = "complete")

# make the hclust object into a dendrogram so it can use the class functions
dend = as.dendrogram(Player.hclust)

# color the branches based on the clusters
dend = color_branches(dend, k=3, groupLabels = c("Group 2", "Group 1", "Group 3"))

# hang the dendrogram so the branches looks longer in the plot
dend = hang.dendrogram(dend,hang_height=0.3)

# set small label size
dend = set(dend, "labels_cex", 0.1)

# plot with desirable margins with transparent background
par(mar = c(3,2,3,0.2)+0.1, bg = NA)
plot(cut(dend, h=170)$upper, 
     main = "Upper tree of dendrogram from hclust", 
     horiz = TRUE,  nodePar = list(cex = .007))

# observe numbers of players in clusters at different levels of the tree, output at report
groupings = cutree(Player.hclust, c(2:8))
apply(groupings, 2,table)

# assign 3 clusters to the players and change the column name
Player_hclust = data.frame(Player_new,groupings[,"3"])
colnames(Player_hclust)[30] = "grouping"

# get the cluster means for interpretation
hclust_means = aggregate(Player_hclust[2:30],by=list(grouping),FUN=mean)[,-1]

# visualizing the means of the clusters with a heatmap
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

### find out who were grouped differently in the two clustering methods

# read in the dataframe from kmean results
Player_kmean = readRDS("Player_kmean.rds")

# put the result of agglomerative clustering into the data frame
# simple logical operation was used to find out who was grouped differently
# because the names of the groups were assigned randomly
Player_kmean$fit.hclust = Player_hclust$grouping
diff_players = subset(Player_kmean[,c(1,30,31)], 
                      (fit.cluster-fit.hclust!=1)&(fit.cluster-fit.hclust!=-2))

# join the names to the agglomerative clustering results
diff_players = inner_join(diff_players, Player_names)

# look at the names, most of them are midfielders
diff_players$player_name
```

### kmeans :
```{r}
# load the required package, install if necessary

if (!require("cluster")) {
  install.packages("cluster", dependencies = TRUE)
  library(cluster)
}
if (!require("fpc")) {
  install.packages("fpc", dependencies = TRUE)
  library(fpc)
}
if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}
if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
# read in the mean player attributes prepared in advance
# this dataset was used in principal component analysis

Player_new = readRDS("Player_Attributes_quant_mean.rds")

# prepare the within SS for one cluster, and set that equal for between SS 

wss = (nrow(Player_new)-1)*sum(apply(Player_new,2,var))
bss = (nrow(Player_new)-1)*sum(apply(Player_new,2,var))

# for each number of clusters, find out WSS and BSS from kmeans()

for (i in 2:15) { 
  result = kmeans(Player_new, iter.max = 15, centers=i)
  wss[i] = result$tot.withinss
  bss[i] = sum(result$betweenss)
}

# plot wss:tss against K with both line and points

plot(1:15, wss/(bss+wss), type="b", main = "Choosing K in kmean clustering",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares to TSS")
abline(a = 0.2, b = 0, col = "red")


# choosing K=3 from the elbow joint, use kmeans() again to get the cluster solution
fit = kmeans(Player_new[2:29], 3) 

# assign the clusters to the players
Player_kmean = data.frame(Player_new, fit$cluster)

# get the cluster means for interpretation
means = aggregate(Player_kmean[2:30],by=list(fit$cluster),FUN=mean)[,-1]

# visualizing the means of the clusters with a heatmap
means_matrix = data.matrix(means[,1:28])               # acceptable format for heatmap.2()
my_palette = brewer.pal(9,"Blues")                     # desirable color palette
par(cex.main=1)                                        # readable title
heatmap.2(means_matrix,
          Rowv=NA, Colv=NA,                            # no dendrograms on the axis
          main = "Aggregated means from kmeans",       # heat map title
          col=my_palette,
          scale="none",                                # use scale as given
          key=TRUE,                                    # show color key
          symkey=FALSE, 
          labRow = c("Group 1", "Group 2", "Group 3"),
          cexRow=1,                                    # size of row labels
          density.info="none",                     
          trace="none",
          margins=c(8,4)
)

# how many players are in each group?
table(Player_kmean$fit.cluster)

# read in player information for the names matching
Player_names = readRDS("Player_names.rds")

# join the names to the k-mean clustering results
Player_kmean = inner_join(Player_kmean, Player_names)

# check player names in each cluster to find out their positions
Player_kmean$player_name[Player_kmean$fit.cluster == 1]
Player_kmean$player_name[Player_kmean$fit.cluster == 2]
Player_kmean$player_name[Player_kmean$fit.cluster == 3]

# save the resulting data frame as .rds file
saveRDS(Player_kmean, "Player_kmean.rds")
```
