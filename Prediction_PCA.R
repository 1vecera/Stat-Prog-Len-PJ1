#Analysis of the prediction power
# library(RSQLite)
# Before export
# con = dbConnect(SQLite(), dbname="database.sqlite")
# Match = tbl_df(dbGetQuery(con,"SELECT * FROM Match"))

library(dplyr)
library(stringr)
library(data.table)
library(pROC)
library(ggplot2)


#First we need to make sure we have the right data

Match = readRDS("Match.rds")
Match = as.data.table(Match)

#Get out just subset of columns we will use
#Mostly only the player ids and other parts
Match_filter = Match[,c(1,2,4,6,7,10,11,44,55,56:77)]
Match_filter = na.omit(Match_filter) #Get rid of matches with missing values and player ides
#We are talking about only 4000 rows
#We decided to leave the first season out of our analysis
Match_filter = Match_filter[season != "2008/2009"]

#Now we can start joining in the results of the pca analysis

Data_players = data.table(readRDS("Data_players_Match_predictions.rds"))

#We use the different columns to get the data together
merge_list = list()

for (i in names(Match_filter)[10:31]) {
  #For each play in the match, we join the 3 PCA columns
  merge_loop = merge(x = Data_players, y = Match_filter,by.x= "player_api_id", by.y = i, all.y = T)
  merge_loop = merge_loop[order(match_api_id),] #Order everything in the same order
  merge_loop  = merge_loop[,c(1:5,10)] #We want just the RC and the information about the players 
  merge_list[[i]] = merge_loop
}
merge_df_raw = do.call(cbind,merge_list)


#get rid of columns with api_id as part of them after the controlling them manually 
id_columns_location = grepl("api_id",names(merge_df_raw))
merge_df_clean = merge_df_raw[,-id_columns_location,with = F]
#Add the original columns from the match data
Match_filter= Match_filter[order(match_api_id)] #make sure we don't missmatch the data
if (identical(
  merge_df_raw$home_player_3.match_api_id,
  Match_filter$match_api_id)){ #Sample check if the ids are right
  merge_df_clean = cbind(Match_filter, merge_df_clean )
  print("Columns added, please do not rerun.")}

merge_df_clean = data.table(na.omit(merge_df_clean)) #Get rid of circa 1000 rows which are not complete
#Add column showing if the home has won using the data.table syntax :=

merge_df_clean[home_team_goal > away_team_goal, home_team_win:= T ]
merge_df_clean[!home_team_goal > away_team_goal, home_team_win:= F ]
#We wanted to see if the means themselves can be used as predictions factors instead the ones of the single players
subset_RC_Columns = merge_df_clean[,grepl("RC_",names(merge_df_clean)),with = F]
subset_RC_Columns_home = subset_RC_Columns[,grepl("home_",names(subset_RC_Columns)),with = F]
subset_RC_Columns_away = subset_RC_Columns[,!grepl("home_",names(subset_RC_Columns)),with = F]

Principal_Components_Means_List = list()

#Calculate means for all of the teams and components
for (i in paste0("4_",1:4)){ #Getting one after each other the columns out we requre
  name_column_home = paste0("mean_home_",i) #creating names for the lists
  name_column_away = paste0("mean_away_",i)
  
  #Calculating the means for hte rows of the selected colums
  Principal_Components_Means_List[[name_column_home]] = 
    rowMeans(x = subset_RC_Columns_home[,grepl(i,names(subset_RC_Columns_home)),with = F])
  Principal_Components_Means_List[[name_column_away]] = 
    rowMeans(x = subset_RC_Columns_away[,grepl(i,names(subset_RC_Columns_away)),with = F])
}
#mean of the factors
#Combining into the data frame
Principal_Components_Means_DT = do.call(cbind.data.frame,Principal_Components_Means_List)
#Sorting the data to have first home and than away columns
Principal_Components_Means_DT = Principal_Components_Means_DT[,sort(names(Principal_Components_Means_DT),decreasing = T)]


#Join the final data together, want just the result, the prediction columns and id so we have something to check if it does make sense
prediction_data_fin = data.table(merge_df_clean[,c(1,33:length(merge_df_clean)),with = F])

#We want to test interaction terms for different teams
#For each team and each principal component, we want to create the name
interaction_terms = " home_team_win ~ ."  #We start by string which we used for the first regressiion
for (i in paste0("4_",1:4)){ #We go component by component and create the interaction term
  home_interaction_term = 
      paste0(collapse = ":", #combine the names of the columns with ":" as separator
             unlist(names(prediction_data_fin))[grepl(i,names(prediction_data_fin)) & #columns of the compnent 
                                                  grepl("home",names(prediction_data_fin))]) #home team
  away_interaction_term =
      paste0(collapse = ":", 
             unlist(names(prediction_data_fin))[grepl(i,names(prediction_data_fin)) & #columns of the compnent
                !grepl("home",names(prediction_data_fin))]#home team
  )
  #we combine the interaction terms with plus sign
  interaction_terms = paste(interaction_terms, home_interaction_term, away_interaction_term, sep= " + " )
  
}

#We perfrom logistical regression using the logit function
reg_list = list()
reg_list$reg_no_interact = glm(home_team_win ~ . - id,
                data = prediction_data_fin,
                family = binomial(link = "logit"))
reg_list$reg_mean = glm(prediction_data_fin$home_team_win ~ .*.,
                data = Principal_Components_Means_DT,
                  family = binomial(link = "logit"))
reg_list$reg_interact = glm(formula = interaction_terms,
                data = prediction_data_fin,
                family = binomial(link = "logit"))
#We get out the fitted values
fitted_values_list = list()
for (i in 1:3)  fitted_values_list[[(names(reg_list)[i])]] =  reg_list[[i]][["fitted.values"]] 
#We test auc for the fitted models: attention - no test train split
#We use auc from pROC package
sapply(X = fitted_values_list,FUN = auc, response = merge_df_clean$home_team_win) 
#We need the functions of the quantlet 7
source("Quantlet7_Prediction_Functions/Quantlet7_Prediction_Functions.R")
###Applying the ridge ression to our other problem
#Use the ridge function to perform naive prediction model
ridge_model = ridge_regression(home_team_win ~ . - id,
                               data = prediction_data_fin, lambda = 0)
optimise()
#Now we use crossvalidation to return the optimal error and thrashold

ridge_cv_err = cross_validation(fct = ridge_regression, formula = home_team_win ~ . - id,
                                ntimes = 4, data = prediction_data_fin,
                                beta_location = "Beta", lambda = 0,return_mean_PPC = F)

ridge_cv_err

optimise(cross_validation )

lambdas = c(c(1,4,7) %o% 10^(0:4))

pcc_array = c()
for (i in lambdas
     # seq(from = 0, to= 100,length.out = 10 )
     ){
  set.seed(123)
  pcc = cross_validation(fct = ridge_regression, formula = home_team_win ~ . - id, ntimes = 3, data = prediction_data_fin,
                   beta_location = "Beta", lambda = i)
  pcc_array = c(pcc_array,pcc )
  }
qplot(lambdas,pcc_array)


prop.table(table(prediction_data_fin$home_team_win))
optimise(calc_missclaf_errof,interval = c(0,1),maximum = T,
         tol = 0.001, prob = reg_no_interact$fitted.values, truth = prediction_data_fin$home_team_win)
optimise(calc_missclaf_errof,interval = c(0,1),maximum = T,
         tol = 0.001, prob = reg_interact$fitted.values, truth = prediction_data_fin$home_team_win)



#C

#Test for the presentation


cross_validation_predict(glm,formula = interaction_terms,
                         data = prediction_data_fin,
                         family = binomial(link = "logit"))




cross_validation(fct = glm, formula = home_team_win ~ . - id,
                 ntimes = 3, data = prediction_data_fin,
                 beta_location = "coefficients", family = binomial(link = "logit"))
