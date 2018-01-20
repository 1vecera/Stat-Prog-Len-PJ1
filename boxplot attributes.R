
library(ggplot2)
library(ggrepel)
library(tidyr)


Player_Attributes_quant_mean=aggregate(quant,by=list(quant$player_fifa_api_id), mean)[-1] 
  
gsub("(^[[:alpha:]])", "\\U\\1", colnames(Player_Attributes_quant_mean), perl=TRUE) #Capitalize first letter

#Rename (not best way to do)
Player_Attributes_new<-setNames(Player_Attributes_quant_mean, c("Player ID", "Crossing", "Finishing", "Heading Accuracy", "Short Passing", 
                                                                "Volleys", "Dribbling", "Curve", "Free Kick Accuracy", "Long Passing", 
                                                                "Ball Control", "Acceleration", "Sprint Speed", "Agility", "Reactions", 
                                                                "Balance", "Shot Power", "Jumping", "Stamina", "Strength", "Long Shots", 
                                                                "Aggression", "Interceptions", "Positioning", "Vision", "Penalties", "Marking", 
                                                                "Standing Tackle", "Sliding Tackle"))
#Sort Variables according to Categories
Player_Attributes_Sorted<-Player_Attributes_new[c(12:16, 18:20, 22:25,2:11, 17, 21, 26:29)]

par(mar = c(2.1,7.1,2.1,2.1))

#Creating boxplot,

colors = c(rep("red",8),rep("blue",4),rep("green",16))

xlims<-range(Player_Attributes_Sorted)*c(0.8,1.0)
Attributes_boxplot<-boxplot(Player_Attributes_Sorted, horizontal=T,
                            ylab="",
                            las=1, main="Player Attributes",
                            cex.axis=0.8, col=colors,
                            ylim=xlims) 


#Creating Legend ##DOENST WORK!

    
    



labels=c("Technical", "Physical", "Mental")
legend(x=75, y=29, inset=0.5, title="Legend", labels, col=c("green", "red", "blue"),
       lty=1:2, cex=0.8 )


# GGPlot ------------------------------------------------------------------

# Apply the naming functions


List_scores = mapply(name_pca,List_scores,c("NR","NR","VR","VR"))
DF_Player_Scores = do.call(cbind, List_scores)

row.names(DF_Player_Scores) = Player_Attributes_quant_mean[,1]


#Creation of the master plot

#Get tidy data out 


DF_Factor_loadings_tidy_raw = as.data.frame(cbind(c("Crossing", "Finishing", "Heading Accuracy", "Short Passing", 
                                                    "Volleys", "Dribbling", "Curve", "Free Kick Accuracy", "Long Passing", 
                                                    "Ball Control", "Acceleration", "Sprint Speed", "Agility", "Reactions", 
                                                    "Balance", "Shot Power", "Jumping", "Stamina", "Strength", "Long Shots", 
                                                    "Aggression", "Interceptions", "Positioning", "Vision", "Penalties", "Marking", 
                                                    "Standing Tackle", "Sliding Tackle"),DF_Loadings[,c(1:2,7:8)]))
names(DF_Factor_loadings_tidy_raw)[1] = "Att_Name"


DF_Factor_loadings_tidy  = gather(DF_Factor_loadings_tidy_raw
                                  , key ="Type_Rotation_NrCols_ComponentNumber",
                                  value = "value", - Att_Name) %>%    
  separate(Type_Rotation_NrCols_ComponentNumber, into=c("Type","Rotation","Nrcols","ComponentNumber"))%>% 
  spread(key = "ComponentNumber", value = "value")
DF_Factor_loadings_tidy[5:6] = lapply(X = DF_Factor_loadings_tidy[5:6], as.numeric)
names(DF_Factor_loadings_tidy)[5:6] =c("C1","C2")



rm(DF_Factor_loadings_tidy_raw)

#create DAH MASTER Ducking PLOT

crossx = geom_hline(yintercept = 0)
crossy = geom_vline(xintercept = 0)




ggplot(data = DF_Factor_loadings_tidy , aes(x= 0 , y= 0, xend= C1, yend = C2, label =Att_Name  )) + 
  geom_segment(arrow = arrow(length = unit(0.5, "cm")),alpha = 0.7) +
  crossx + crossy + 
  geom_text_repel(aes(x= C1*1.00  , y= C2*1.00),
              nudge_x = 0.03, # nudge_y = 0.03,
             #position = "jitter",
             alpha = 1, col= "red") +
  scale_x_continuous("First Component"
                     # ,limits = c(0.75,0.75) #- not working
  ) + 
  scale_y_continuous("Second Component" 
                     # ,limits = c(0.75,0.75) #- not working
  ) +
  facet_wrap(~  Rotation ) 


# Plot of the players
library(dplyr)

DF_Player_Scores_VR12 = data.frame(Player_Attributes_quant_mean[1],(DF_Player_Scores[,c(7,8)]))
  DF_Player_Scores_VR12 = left_join( x= DF_Player_Scores_VR12, y= Player_all[,3:4], by = "player_fifa_api_id")
?semi_join
  DF_Player_Scores_VR12 =  unique ( DF_Player_Scores_VR12)
  names(DF_Player_Scores_VR12)


ggplot(data = DF_Player_Scores_VR12, aes(x=RC_VR_NrCol4_1, y = RC_VR_NrCol4_2))+ 
  geom_point( alpha = .3)  +
  scale_x_continuous("First Rotated Component"
                    
  ) + 
  scale_y_continuous("Second Rotated  Component" ) +
  geom_text_repel(data = subset(DF_Player_Scores_VR12, RC_VR_NrCol4_1 > 7.5 | RC_VR_NrCol4_2 > 4.8),
                  aes(label = player_name), col = "red")
                   
summary(subset(DF_Player_Scores_VR12, RC_VR_NrCol4_1 > 8.4 | RC_VR_NrCol4_2 > 4.8 , col = "red"  ))



?scale_x_continuous





