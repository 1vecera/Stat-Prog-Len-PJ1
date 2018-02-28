# Name of Quantlet :Principal Component Analysis (PCA)

# Description: PCA function from scratch PCA can be thought of as fitting an n-dimensional ellipsoid to the data, where each
# axis of the ellipsoid represents a principal component. If some axis of the ellipsoid is small, then the variance along
# that axis is also small, and by omitting that axis and its corresponding principal component from our representation of the
# dataset, we lose only a commensurately small amount of information.

# Author :Julien Kraemer

# Example :Run the PCA on a set of player attributes ,visualize the results in 2-dimension and compare with the function
# dudi.pca

rm(list = ls())

library(psych)
library(ade4)

# 2/We extract quantitative information

# We load the table Player_all Check if the Finished files are in the directory
if (file.exists("Player_all.rds")) {
    Player_all = readRDS("Player_all.rds")
} else {
    source("data_cleaning.R")
}



# 2/We extract the quantitative informations

# We generate here a new database where we average over the several games of each player
quant = Player_all[, c(4, 11:38)]  #We only keep a subset of nonquantitative variables
quant = na.omit(quant)  #We have to delete the rows containing some missing values #not the best way to do
L = levels(as.factor(quant$player_fifa_api_id))

Player_Attributes_quant_mean = aggregate(quant, by = list(quant$player_fifa_api_id), mean)[-1]




# 5/ First results

# We can check that we obtain the same results with the function of R
source("PCA_Function.R")  #Get in the PCA Function created by Julien


pca = PCA(Player_Attributes_quant_mean[-1], desiredvariance = 0.8, norm = T, order = 10)  #All of the attributes without the id
pca2 = PCA(Player_Attributes_quant_mean[-1], desiredvariance = 0.75, order = 2)  #Pca with Just 2 parts





cum = pca$cum  #relative cumsum of the inertia 
FF = pca$FF  #contributions of the players to the axes (It should be interesting to visualize the positions of the players in the plane F1-F2)
G = pca$G  #contributions of the variables to the axes (rule of thumb : We only keep the contributions greather than 1/p*100=2.7)
R = pca$R  #correlation matrix
