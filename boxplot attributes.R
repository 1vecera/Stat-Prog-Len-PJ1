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







