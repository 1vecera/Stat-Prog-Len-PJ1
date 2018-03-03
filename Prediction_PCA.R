#Analysis of the prediction power
library(RSQLite)
library(dplyr)
library(stringr)
library(data.table)
library(pROC)

#First we need to make sure we have the right data
con = dbConnect(SQLite(), dbname="database.sqlite")
Match = tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
  
Match = as.data.table(Match)
View(Match)
#Get out just subset of columns we will use
#Mostly only the player ids and other parts
Match_filter = Match[,c(1,2,4,6,7,10,11,44,55,56:77)]
Match_filter = na.omit(Match_filter) #Get rid of matches with missing values and player ides
#We are talking about only 4000 rows
#We decided to leave the first season out of our analysis
Match_filter = Match_filter[season != "2008/2009"]

#Now we can start joining in the results of the pca analysis
Match_filter
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
#Add other columns to the data frame
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
merge_df_clean$home_team_win = as.factor(merge_df_clean$home_team_win)
#In order to calculate the synergy effects, we also calculte the intern term for each teams for each subpart

subset_RC_Columns = merge_df_clean[,grepl("RC_",names(merge_df_clean)),with = F]
subset_RC_Columns_home = subset_RC_Columns[,grepl("home_",names(subset_RC_Columns)),with = F]
subset_RC_Columns_away = subset_RC_Columns[,!grepl("home_",names(subset_RC_Columns)),with = F]

rowMeans(x = subset_RC_Columns_home[,grepl("4_1",names(subset_RC_Columns_home)),with = F])

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

View(Principal_Components_Means_DT)
#Join the final data together, want just the result, the prediction columns and id so we have something to check if it does make sense
prediction_data_fin = data.table(merge_df_clean[,c(1,33:length(merge_df_clean)),with = F])


first_reg = glm(home_team_win ~ . - id,
                data = prediction_data_fin,
                family = binomial(link = "logit"))

second_reg = glm(prediction_data_fin$home_team_win ~ .,
                 data = Principal_Components_Means_DT,
                 family = binomial(link = "logit"))

unlist(names(prediction_data_fin))

a = unlist(names(prediction_data_fin))[grepl("NrCol4_3",names(prediction_data_fin)) & grepl("home",names(prediction_data_fin))]
grepl("NrCol4_3",names(prediction_data_fin)) & grepl("home",names(prediction_data_fin))

interaction_terms = " home_team_win ~ ."

for (i in paste0("4_",1:4)){ #Getting one after each other the columns out we requre
 
  home_interaction_term = paste0(collapse = ":",
    unlist(names(prediction_data_fin))[grepl(i,names(prediction_data_fin)) &
                                        grepl("home",names(prediction_data_fin))]
  )
  away_interaction_term =  paste0(collapse = ":",
    unlist(names(prediction_data_fin))[grepl(i,names(prediction_data_fin)) &
                                         !grepl("home",names(prediction_data_fin))]
  )
  
  interaction_terms = paste(interaction_terms, home_interaction_term, away_interaction_term, sep= " + " )
  
}  


interaction_terms

paste0(a,collapse = ":")
paste()

thrid_reg = glm(prediction_data_fin$home_team_win ~ .*.,
                data = Principal_Components_Means_DT,
                family = binomial(link = "logit"))

fourth_reg = glm(formula = interaction_terms,
                data = prediction_data_fin,
                family = binomial(link = "logit"))


fourth_reg$coefficients


formula = formula(interaction_terms)


test= auc(response = merge_df_clean$home_team_win, predictor = first_reg$fitted.values)
test2= auc(response = merge_df_clean$home_team_win, predictor = second_reg$fitted.values)
test3= auc(response = merge_df_clean$home_team_win, predictor = thrid_reg$fitted.values)
test3;test2;test


#####Ridge Regression

a = rnorm(200)
b = a + rnorm(n =200)
c = a + rnorm(n =200)*0.8
d = a + rnorm(n =200)*0.6

e = ifelse(a < -0.5,"A",
  ifelse(a<0, "B","C"))



test_data = data.frame(b,c,d,e)

ridge_regression = function(formula, lambda = 0, data, mse_only = F) {

  X = model.matrix(object = formula, data = data)
  y = data[[formula[[2]]
            ]]
  Beta = solve(t(X) %*% X + diag(x = lambda, ncol = ncol(X), nrow = ncol(X))) %*% t(X) %*%  y
  predictions = X %*% Beta
  SSR= t((y- predictions)) %*% (y- predictions)
  MSE = SSR/length(y)
  results = list(Beta = Beta, predictions = predictions, SSR = SSR, MSE = MSE  )
  return(results)

}

i = ridge_regression(formula = b ~ d:c + d + c + e, data= test_data, lambda = 1000000 )



cross_validation = function(fct, measurement_location = "predictions", ntimes = 5, data,...){
  splits = sample(x = 1:ntimes, size = nrow(data), replace = T)
  res_array = c()
  for (i in 1:ntimes){
  train_data = data[!splits == i]  
  res_loc  = fct(..., data = split_data)
  res_array = c(res_glob,res_loc)
  }
return(mean(res_array))
  
}



a = sample(1:4, size = 100, replace = T)
i = 2
a == i














i


model.matrix()




typeof(quote(x * 10))
class(Principal_Components_Means_DT)


Beta = solve(t(X) %*% X + lambda) %*% t(X) %*%  y
Predictions = X %*% Beta
SSR= t((y- Predictions)) %*% (y- Predictions)
SSR/length(y)


x = matrix()
y = c(1:100)


quote(x*10)
