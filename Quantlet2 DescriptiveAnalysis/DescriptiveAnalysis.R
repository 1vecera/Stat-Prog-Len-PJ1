#Descriptive Analysis ---------------------------------------------------------------l
#Visualization of distribution

library(ggplot2)
library(tidyr)

#Load Table Player_all from preprocessing  (Quantlet 1)
Player_all = readRDS("Player_all.rds")

#Extract the quantitative informations
View(Player_all)
#Generate a new database where we average over the several games of each player
quant = Player_all[,c(2,11:38)] #Keep a subset of nonquantitative variables
quant = na.omit(quant) #Delete the rows containing some missing values 
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
par(mar = c(5.1,4.1,4.1,3.1))

#Create Boxplot for each attribute category
#Technical Skills
Player_Attributes_Technical_Boxplot = ggplot(stack(Player_Attributes_Technical), 
                                             aes(x = ind, y = values)) +
                                             geom_boxplot(fill='grey', color="darkred") #Define colours
Technical_Boxplot = Player_Attributes_Technical_Boxplot + 
                    coord_flip() + #Horizontal Boxplot
                    labs(title = "Player Attributes: Technical", x = "Player Attributes", y = "Attribute Values") +  #Label title and axes
                    theme_bw() + #Transparent Background
                     theme(legend.position="none") #Remove legend

ggsave("Boxplot_Attributes_Technical.pdf", Technical_Boxplot, width = 30, height = 20, units = "cm")


#Mental Skills
Player_Attributes_Mental_Boxplot = ggplot(stack(Player_Attributes_Mental), 
                                          aes(x = ind, y = values)) +
                                          geom_boxplot(fill='grey', color="darkgreen") #Define colours
Mental_Boxplot = Player_Attributes_Mental_Boxplot + 
                 coord_flip() + #Horizontal Boxplot
                 labs(title = "Player Attributes: Mental", x = "Player Attributes", y = "Attribute Values") +  #Label title and axes
                 theme_bw() + #Transparent Background
                 theme(legend.position="none") #Remove legend

Mental_Boxplot
ggsave("Boxplot_Attributes_Mental.pdf", Mental_Boxplot, width = 30, height = 20, units = "cm")


#Physical Skills
Player_Attributes_Physical_Boxplot = ggplot(stack(Player_Attributes_Physical), 
                                            aes(x = ind, y = values)) +
                                            geom_boxplot(fill='grey', color="darkblue") #Define colours
Physical_Boxplot = Player_Attributes_Physical_Boxplot + 
                 coord_flip() + #Horizontal Boxplot
                 labs(title = "Player Attributes: Physical", x = "Player Attributes", y = "Attribute Values") +  #Label title and axes
                 theme_bw() + #Transparent Background
                 theme(legend.position="none") #Remove legend

Physical_Boxplot
ggsave("Boxplot_Attributes_Physical.pdf", Physical_Boxplot, width = 30, height = 20, units = "cm")
