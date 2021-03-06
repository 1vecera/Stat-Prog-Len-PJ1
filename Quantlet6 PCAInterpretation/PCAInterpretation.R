#Create plots for interpretation

library(ggplot2)
library(tidyr)
library(ggrepel)

#Tidy data  
DF_Loadings = readRDS("DF_Loadings.rds")
att_names = rownames(DF_Loadings)
att_names= gsub('_',' ',att_names)
att_names = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", att_names, perl=TRUE)

#Rename variable 
DF_Factor_loadings_tidy_raw = data.frame(att_names_tidy,DF_Loadings[,c(1:2,7:8)])
names(DF_Factor_loadings_tidy_raw)[1] = "Att_Name"

#Adjust and select columns for ggplot
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
                                      facet_wrap(~ Rotation)  #Two graphics rotated and not-rotated 
                                      + theme_bw()     #Transparent Background
ggsave("Factor_Loadings.pdf", Factor_Loadings, width = 40, height = 22, units = "cm")

#Plot the players
DF_Player_Scores_VR12 = readRDS("DF_Player_Scores_VR12.rds")

Plot_Players = ggplot(data = DF_Player_Scores_VR12, aes(x=RC_VR_NrCol4_1, y = RC_VR_NrCol4_2))+ 
               geom_point( alpha = .3) 
               + scale_x_continuous("First Rotated Component")                  
               + scale_y_continuous("Second Rotated  Component" ) 
               + geom_label_repel(data = subset(DF_Player_Scores_VR12, RC_VR_NrCol4_1 > 7.5 | RC_VR_NrCol4_2 > 4.8),
               aes(label = player_name), col = "red", segment.color ="red", size = 2.5)   
               + theme_bw()  #Transparent Background              
#Save Plot
ggsave("Plot_Players.pdf", Factor_Loadings, width = 20, height = 20, units = "cm") #Save plot
