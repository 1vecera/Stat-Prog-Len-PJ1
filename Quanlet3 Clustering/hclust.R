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