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
#In order to calculate the synergy effects, we also calculte the intern term for each teams for each subpart

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
test4= auc(response = merge_df_clean$home_team_win, predictor = fourth_reg$fitted.values)
test4;test3;test2;test




#Calculate log likelyhood

set.seed(123, "")
ridge_regression = function(formula, lambda = 0, data) {

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

i = ridge_regression(formula = b ~ d:c + d + c + e, data= test_data, lambda = 10 )
i2 = cross_validation(fct = ridge_regression ,formula = b ~ d:c + d + c + e, data= test_data, lambda = 2)



a = optimize(cross_validation, interval = c(0,10000), fct = ridge_regression,
         formula = b ~ d:c + d + c + e, data= test_data,beta_location = "Beta", tol = 0.000001, ntimes = 5 )


res=c() 
for (i in 0:100){
set.seed(123)
mse = cross_validation(fct = ridge_regression ,formula = b ~ d:c + d + c + e, data= test_data, lambda = i)
res = c(res,mse )
}
plot(res)

calc_missclaf_errof = function(threshold_int, prob, truth) {
  predictions_test_response  =  ifelse(prob > threshold_int, 1,0 ) 
  correctly_classified       =  sum(diag(
    prop.table(table(predictions_test_response,truth)))) 
  return(correctly_classified)
}


cross_validation = function(fct, beta_location = "Beta", ntimes = 5, data, formula,...){
  splits = sample(x = 1:ntimes, size = nrow(data), replace = T)
  res_array = c()
  for (i in 1:ntimes){
    train_data = data[!splits == i,]  
    res_loc    = fct(formula = formula,..., data = train_data)
    test_data  = data[splits == i,]  
    X_test = model.matrix(object = formula, data = test_data)
    y_test = test_data[[formula[[2]]
              ]]
    predictions_test = X_test %*% res_loc[[beta_location]]
    opt_pcc = optimise(calc_missclaf_errof,interval = c(0,1), maximum = T,
                       tol = 0.01, prob = predictions_test, truth = y_test)
    res_array = c(res_array, opt_pcc[[2]])
  # SSR_test= t((y_test- predictions_test)) %*% (y_test - predictions_test)
  # MSE_test = SSR_test/length(y_test)
  # res_array = c(res_array,MSE_test)
  }
return(mean(res_array))
}







###Applying the ridge ression to our other problem

ridge_model = ridge_regression(home_team_win ~ . - id,
                               data = prediction_data_fin, lambda = 0)

optimise(calc_missclaf_errof,interval = c(0,1),maximum = T,
                   tol = 0.01, prob = ridge_model$predictions, truth = prediction_data_fin$home_team_win)

calc_missclaf_errof(prob = ridge_model$predictions,
                    truth = prediction_data_fin$home_team_win, threshold_int = 0.7)

ridge_cv_err = cross_validation(fct = ridge_regression, formula = home_team_win ~ . - id, ntimes = 4, data = prediction_data_fin,
                                beta_location = "Beta", lambda = 0)
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

library(ggplot2)

prop.table(table(prediction_data_fin$home_team_win))
optimise(calc_missclaf_errof,interval = c(0,1),maximum = T,
         tol = 0.001, prob = first_reg$fitted.values, truth = prediction_data_fin$home_team_win)
optimise(calc_missclaf_errof,interval = c(0,1),maximum = T,
         tol = 0.001, prob = fourth_reg$fitted.values, truth = prediction_data_fin$home_team_win)
optimise(calc_missclaf_errof,interval = c(0,1),maximum = T,
         tol = 0.001, prob = second_reg$fitted.values, truth = prediction_data_fin$home_team_win)
