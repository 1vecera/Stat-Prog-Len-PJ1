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

Player_new = readRDS("Quanlet3 Clustering/Player_Attributes_quant_mean.rds")

# prepare the within SS for one cluster, and set that equal for between SS 

wss = (nrow(Player_new) -1) * sum(apply(Player_new ,2 ,var))
bss = (nrow(Player_new) -1) * sum(apply(Player_new ,2 ,var))

# for each number of clusters, find out WSS and BSS from kmeans()

for (i in 2:15) { 
    result = kmeans(Player_new, iter.max = 15, centers=i)
    wss[i] = result$tot.withinss
    bss[i] = sum(result$betweenss)
}

# plot wss:tss against K with both line and points

par( mar = c(3,3,3,2) + 0.1, bg = NA)
plot( 1:15, wss/(bss+wss), 
      type = "b", 
      main = "Choosing K in kmean clustering",
      xlab = "Number of Clusters",
      ylab = "Within groups sum of squares to TSS" 
      )

abline(a = 0.2, b = 0, col = "red")


# choosing K=3 from the elbow joint, use kmeans() again to get the cluster solution
fit = kmeans(Player_new[2:29], 3) 

# assign the clusters to the players
Player_kmean = data.frame(Player_new, fit$cluster)

# get the cluster means for interpretation
means = aggregate( Player_kmean[2:30],by = list(fit$cluster), FUN = mean)[,-1]

# visualizing the means of the clusters with a heatmap
dev.off()
means_matrix = data.matrix(means[,1:28])                # acceptable format for heatmap.2()
my_palette   = brewer.pal(9,"Blues")                    # desirable color palette
par(cex.main = 1, bg = NA)                                      # readable title, transparent bg
heatmap.2( means_matrix,
           dendrogram   = "none",                             # no dendrograms on the axis
           Rowv         = NA, 
           Colv         = NA,                                   # no dendrograms on the axis
           main         = "Aggregated means from kmeans",       # heat map title
           col          = my_palette,
           scale        = "none",                               # use scale as given
           key          = TRUE,                                 # show color key
           symkey       = FALSE, 
           labRow       = c("Group 1", "Group 2", "Group 3"),
           cexRow       = 1,                                    # size of row labels
           density.info = "none",                     
           trace        = "none",
           margins      = c(8,4)
           )

# how many players are in each group?
table( Player_kmean$fit.cluster )

# read in player information for the names matching
Player_names = readRDS("Quanlet3 Clustering/Player_names.rds")

# join the names to the k-mean clustering results
Player_kmean = inner_join(Player_kmean, Player_names)

# check player names in each cluster to find out their positions
# Player_kmean$player_name[Player_kmean$fit.cluster == 1]
# Player_kmean$player_name[Player_kmean$fit.cluster == 2]
# Player_kmean$player_name[Player_kmean$fit.cluster == 3]

# save the resulting data frame as .rds file
saveRDS(Player_kmean, "Quanlet3 Clustering/Player_kmean.rds")