# Descriptive Analysis ---------------------------------------------------------------l

library(ggplot2)
library(ggrepel)
library(tidyr)

#Extracting the quantitative informations
View(Player_all)
#Generate a new database where we average over the several games of each player
quant = Player_all[,c(2,11:38)] #We only keep a subset of nonquantitative variables
quant = na.omit(quant) #We have to delete the rows containing some missing values 
L = levels(as.factor(quant$player_api_id)) #Factorize Player IDs

Player_Attributes_quant_mean = aggregate(quant,by=list(quant$player_api_id), mean)[-1]

#Rename variables
Player_Attributes_new = setNames(Player_Attributes_quant_mean, c("Player ID", "Crossing", "Finishing", "Heading", "Short Passing", 
                                                                "Volleys", "Dribbling", "Curve", "Free Kick", "Long Passing", 
                                                                "Ball Control", "Acceleration", "Sprint Speed", "Agility", "Reactions", 
                                                                "Balance", "Shot Power", "Jumping", "Stamina", "Strength", "Long Shots", 
                                                                "Aggression", "Interceptions", "Positioning", "Vision", "Penalties", "Marking", 
                                                                "Standing Tackle", "Sliding Tackle"))
#Select variables according to their category
Player_Attributes_Technical = Player_Attributes_new[c(2:11,17,21,26:29)] #Technical Attributes
Player_Attributes_Mental = Player_Attributes_new[c(22:25)] #Mental Attributes
Player_Attributes_Physical = Player_Attributes_new[c(12:16,18:20)] #Physical Attributes

#Set the graph margins
par(mar = c(2.1,7.1,2.1,2.1))

#Create Boxplot for each category
Boxplot_Technical = boxplot(Player_Attributes_Technical, horizontal = T, #Technical Attributes
                           las=1, main = "Player Attributes: Technical", cex.axis = 0.9, col = 'red')

Boxplot_Mental = boxplot(Player_Attributes_Mental, horizontal = T, #Mental Attributes
                            las = 1, main = "Player Attributes: Mental", cex.axis = 0.9, col = 'green')

Boxplot_Physical = boxplot(Player_Attributes_Physical, horizontal = T, #Physical Attributes
                         las = 1, main = "Player Attributes: Physical", cex.axis = 0.9, col = 'blue')
