#GGPlot ------------------------------------------------------------------
#Create master plot

library(ggplot2)
library(tidyr)
library(ggrepel)



#Tidy data  
DF_Loadings = readRDS("DF_Loadings.rds")
DF_Factor_loadings_tidy_raw = as.data.frame(cbind(c("Crossing", "Finishing", "Heading Accuracy", "Short Passing", 
                                                    "Volleys", "Dribbling", "Curve", "Free Kick Accuracy", "Long Passing", 
                                                    "Ball Control", "Acceleration", "Sprint Speed", "Agility", "Reactions", 
                                                    "Balance", "Shot Power", "Jumping", "Stamina", "Strength", "Long Shots", 
                                                    "Aggression", "Interceptions", "Positioning", "Vision", "Penalties", "Marking", 
                                                    "Standing Tackle", "Sliding Tackle"),DF_Loadings[,c(1:2,7:8)]))
#Rename variable 
names(DF_Factor_loadings_tidy_raw)[1] = "Att_Name"

DF_Factor_loadings_tidy  = gather(DF_Factor_loadings_tidy_raw
                                  , key = "Type_Rotation_NrCols_ComponentNumber",
                                  value = "value", - Att_Name) %>%    
                                  separate(Type_Rotation_NrCols_ComponentNumber, into=c("Type","Rotation","Nrcols","ComponentNumber"))%>% 
                                  spread(key = "ComponentNumber", value = "value")

DF_Factor_loadings_tidy[5:6] = lapply(X = DF_Factor_loadings_tidy[5:6], as.numeric)
names(DF_Factor_loadings_tidy)[5:6] =c("C1","C2")

#Remove useless variable
rm(DF_Factor_loadings_tidy_raw)

#Create two axes
crossx = geom_hline(yintercept = 0)
crossy = geom_vline(xintercept = 0)

#Plot factor loadings
Plot_FactorLoadings = ggplot(data = DF_Factor_loadings_tidy , aes(x = 0 , y = 0, xend= C1, yend = C2, label =Att_Name  )) + 
                      geom_segment(arrow = arrow(length = unit(0.5, "cm")),alpha = 0.7) +
                      crossx + crossy + 
                      geom_text_repel(aes(x = C1*1.00  , y = C2*1.00),
                                      nudge_x = 0.03, # nudge_y = 0.03,
                                      #position = "jitter",
                                      alpha = 1, col= "red") +
                                      scale_x_continuous("First Component"
                                      # ,limits = c(0.75,0.75) #- not working
                                      ) + 
                                      scale_y_continuous("Second Component" 
                                      # ,limits = c(0.75,0.75) #- not working
                                      ) +
                                      facet_wrap(~  Rotation )  + theme_bw()     #Two graphics rotated and not-rotated 
ggsave("Factor_Loadings.pdf", Factor_Loadings, width = 40, height = 22, units = "cm")

#Plot the players
DF_Player_Scores_VR12 = readRDS("DF_Player_Scores_VR12.rds")

Plot_Players = ggplot(data = DF_Player_Scores_VR12, aes(x=RC_VR_NrCol4_1, y = RC_VR_NrCol4_2))+ 
               geom_point( alpha = .3)  +
               scale_x_continuous("First Rotated Component"                  
               ) + 
               scale_y_continuous("Second Rotated  Component" ) +
               geom_label_repel(data = subset(DF_Player_Scores_VR12, RC_VR_NrCol4_1 > 7.5 | RC_VR_NrCol4_2 > 4.8),
               aes(label = player_name), col = "red", segment.color ="red", size = 2.5)   + theme_bw()                
ggsave("Plot_Players.pdf", Factor_Loadings, width = 20, height = 20, units = "cm") #Save plot
