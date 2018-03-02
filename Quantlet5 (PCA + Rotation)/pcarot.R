library(dplyr)

#We load the table Player_all
if(file.exists("Player_all.rds")){ #Check if the Finished files are in the directory
  Player_all = readRDS("Player_all.rds")
} else {source("data_cleaning.R")}



#2/We extract the quantitative informations

#We generate here a new database where we average over the several games of each player
quant=Player_all[,c(4,11:38)] #We only keep a subset of nonquantitative variables
quant=na.omit(quant) #We have to delete the rows containing some missing values #not the best way to do
L=levels(as.factor(quant$player_fifa_api_id))

Player_Attributes_quant_mean=aggregate(quant,by=list(quant$player_fifa_api_id), mean)[-1]




#5/ First results

#We can check that we obtain the same results with the function of R
source("PCA_Function.R") #Get in the PCA Function created by Julien


pca=PCA(Player_Attributes_quant_mean[-1],desiredvariance = 0.8, norm = T, order = 10) #All of the attributes without the id
pca2 = PCA(Player_Attributes_quant_mean[-1],desiredvariance = 0.75 , order = 2) #Pca with Just 2 parts


# Rotations ---------------------------------------------------------------
# Perform the rotations

varimax_rotation_PCA = varimax((pca$G)*-1)
varimax_rotation_PCA2 = varimax((pca2$G)*-1)


List_loadings = list(No_Rotation_PCA = (pca$G*-1), No_Rotation_PCA2 = (pca2$G*-1), #Not Rotated loadings first and second part
                     varimax_rotation_PCA =  varimax_rotation_PCA$loadings[,] , #Rotated loadings
                     varimax_rotation_PCA2 =   varimax_rotation_PCA2$loadings[,] #Rotated loadings
)






# Functions for naming columns and rows

name_pca = function(PC_loadings,rotation_name = "NR"){ 
  PC_loadings_new = PC_loadings
  nr_components = ncol(PC_loadings)
  PC_or_RC = ifelse(rotation_name == "NR","PC","RC")
  Numbers = 1:nr_components
  colnames(PC_loadings_new) = paste(PC_or_RC, rotation_name,
                                    paste("NrCol",nr_components,sep=""),
                                    Numbers,
                                    sep = "_")
  return(PC_loadings_new)
}


# Applying this function on the list

List_loadings = mapply(name_pca,List_loadings,c("NR","NR","VR","VR")) #Applying the function name_PCA with different arguemnts

DF_Loadings = do.call(cbind, List_loadings)
rownames(DF_Loadings) = colnames(quant)[-1]


# Create Scores for each player Attention - minus has to go away

List_scores = list(No_Rotation_PCA  = -pca$FF,
                   No_Rotation_PCA2 = -pca2$FF,
                   varimax_rotation_PCA = -pca$FF %*% varimax_rotation_PCA$rotmat, #Rotate the scores
                   varimax_rotation_PCA2 =  -pca2$FF %*% varimax_rotation_PCA2$rotmat) #Rotate the scores

# Apply the naming functions


List_scores = mapply(name_pca,List_scores,c("NR","NR","VR","VR"))
DF_Player_Scores = do.call(cbind, List_scores)

row.names(DF_Player_Scores) = Player_Attributes_quant_mean[,1]

View(DF_Player_Scores)


#Create data subset for the graphs with player names 
DF_Player_Scores_VR12 = data.frame(Player_Attributes_quant_mean[1],(DF_Player_Scores[,c(7,8)]))
DF_Player_Scores_VR12 = left_join( x= DF_Player_Scores_VR12, y= Player_all[,3:4], by = "player_fifa_api_id")
DF_Player_Scores_VR12 =  unique ( DF_Player_Scores_VR12)

 saveRDS(DF_Player_Scores_VR12, file= "DF_Player_Scores_VR12.rds")
 saveRDS(DF_Player_Scores, file = "DF_Player_Scores.rds")
 saveRDS(DF_Loadings, file = "DF_Loadings.rds")
 
 # Data Export Jessie ------------------------------------------------------
 
 
 
 DF_Player_Scores_VR14 = data.frame(Player_Attributes_quant_mean[1],(DF_Player_Scores[,c(7:10)]))
 View(DF_Player_Scores_VR14)
 saveRDS(DF_Player_Scores_VR14, file = "Data_Players_Match_Predictions.rds")

