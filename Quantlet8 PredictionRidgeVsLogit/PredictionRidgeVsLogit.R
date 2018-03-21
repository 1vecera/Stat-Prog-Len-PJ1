#Analysis of the prediction power

  # If you instead want to use the database.sqlite file, please connect it
  # Else use the Match and Data Players tables
  # library(RSQLite)
  # con = dbConnect(SQLite(), dbname="database.sqlite")
  # Match = tbl_df(dbGetQuery(con,"SELECT * FROM Match"))

#Loading packages and/or installing them

for (i in c("data.table","pROC","ggplot2")){
  if (!require(i,character.only = T)) {
    install.packages(i, dependencies = T)
    library(i,character.only = T)
  }

}


# Data Reading ------------------------------------------------------------

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

# Data Joins --------------------------------------------------------------

#We use the different columns to get the data together
merge_list = list()
for (i in names(Match_filter)[10:31]) {
  #For each play in the match, we join the 3 PCA columns, we use Data.Tables merge
  merge_loop      = merge(x = Data_players, y = Match_filter,by.x= "player_api_id", by.y = i, all.y = T)
  merge_loop      = merge_loop[order(match_api_id),] #Order everything in the same order
  merge_loop      = merge_loop[,c(1:5,10)] #We want just the RC and the information about the players 
  merge_list[[i]] = merge_loop
}
merge_df_raw = do.call(cbind,merge_list)

#get rid of columns with api_id 
id_columns_location = grepl("api_id",names(merge_df_raw)) #Gets out the ID Columns names locatoin
merge_df_clean      = merge_df_raw[,-id_columns_location,with = F]

#Add the original columns from the match data
Match_filter= Match_filter[order(match_api_id)] #make sure we don't missmatch the data
if (identical(
  merge_df_raw$home_player_3.match_api_id,
  Match_filter$match_api_id)){ #Sample check if the ids are synchhronized
  merge_df_clean = cbind(Match_filter, merge_df_clean )
  print("Columns added, please do not rerun.")}

merge_df_clean = data.table(na.omit(merge_df_clean)) #Get rid of circa 1000 rows which are not complete

#Add column showing if the home has won using the data.table syntax :=
merge_df_clean[home_team_goal  > away_team_goal, home_team_win:= T ]
merge_df_clean[!home_team_goal > away_team_goal, home_team_win:= F ]

# Creating Team Means -----------------------------------------------------

#We wanted to see if the means themselves can be used as predictions factors instead the ones of the single players
subset_RC_Columns      = merge_df_clean[,grepl("RC_",names(merge_df_clean)),with = F] #Only rotated copomnents
subset_RC_Columns_home = subset_RC_Columns[,grepl("home_",names(subset_RC_Columns)),with = F]
subset_RC_Columns_away = subset_RC_Columns[,!grepl("home_",names(subset_RC_Columns)),with = F]

#Calculating the means of the components
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
Principal_Components_Means_DT = 
  Principal_Components_Means_DT[,sort(names(Principal_Components_Means_DT),decreasing = T)]

#Join the final data together, want just the result, the prediction columns and id so we have something to check if it does make sense
prediction_data_fin = data.table(merge_df_clean[,c(1,33:length(merge_df_clean)),with = F])
#We add the also the home team win to the compoments means for easier regression aftewards
Principal_Components_Means_DT$home_team_win = prediction_data_fin$home_team_win


# Creation of interaction tersm -------------------------------------------

#We want to test interaction terms for different teams
#For each team and each principal component, we want to create the name
interaction_terms = " home_team_win ~ . - id"  #We start by string which we used for the first regressiion
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


# Logistic Regression -----------------------------------------------------

#We perform logistical regression using the logit function
reg_list = list()
reg_list$reg_no_interact = glm(home_team_win ~ . - id,
                data = prediction_data_fin,
                family = binomial(link = "logit"))
reg_list$reg_mean = glm(home_team_win ~ .*.,
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
source("Quantlet7 PredictionFunctions/PredictionFunctions.R")
#Proving that threshold optimization does not bring a lot of value with logit models
threshold_opt  = sapply(X = 0:100/101,FUN = calc_ppc,
                        prob = reg_list$reg_no_interact$fitted.values, truth = prediction_data_fin$home_team_win )
qplot(0:100/101, threshold_opt, geom = "line" ) + theme_bw() +
  xlab("Classification Probability Threshold") + ylab("Percantage Correctly Classfied") + scale_y_continuous(limits = c(0.3,0.7))


# Ridge Regression --------------------------------------------------------
#Use the ridge function to perform naive prediction model

#We search for optimal lamda

#We try different lambdas to reduce to coefficent in search of best PCC
lambdas = c(c(1,4,7) %o% 10^(0:4)) #Outer product to try wide range of lambdas
pcc_array = c()
for (i in lambdas
     # seq(from = 0, to= 100,length.out = 10 )
     ){
  set.seed(123)
  pcc = cross_validation(fct = ridge_regression, formula = home_team_win ~ . - id, ntimes = 3, data = prediction_data_fin,
                   beta_location = "Beta", lambda = i, return_mean_PCC = T)
  pcc_array = c(pcc_array,pcc )
}

qplot(lambdas,pcc_array) + theme_bw() + xlab("Lambda") + ylab("Percantage Correctly Classified")
#We see that optimal landa will be somewhere between 0 and 30 000
#We create search for the best lambda
set.seed(123)
opt_lam = optimise(f = cross_validation,fct = ridge_regression, formula = home_team_win ~ . - id,
                   ntimes = 3, data = prediction_data_fin,
                   beta_location = "Beta", tol = 0.001, interval = c(0,40000), return_mean_PCC = T,
                   maximum = T)
#we take the best lambda and look at the results
ridge_model = ridge_regression(home_team_win ~ . - id,
                               data = prediction_data_fin, lambda = opt_lam$maximum)

# Comparing Ridge Regression and Logistic Regression ----------------------
#Now we use crossvalidation to return the optimal  thrashold and associated PCC
set.seed(123)
list_cv_err = list()
list_cv_err$ridge_cv_err = cross_validation(fct = ridge_regression, formula = home_team_win ~ . - id,
                                            ntimes = 4, data = prediction_data_fin, 
                                            beta_location = "Beta", lambda = opt_lam$maximum,return_mean_PCC = F)
#Check the train set AUC
auc(response = prediction_data_fin$home_team_win, predictor = as.numeric(ridge_model$predictions))
#Check number of prediction higher than one or zero 
ridge_model$predictions[ridge_model$predictions<0 | ridge_model$predictions>1,]
sum(ridge_model$predictions<0 | ridge_model$predictions>1)/nrow(ridge_model$predictions)*100
#We perfrom the cross validation also for 2 of the regressions
#As a last step, calculate 
set.seed(123)
list_cv_err$reg_no_interact =
  cross_validation(fct = glm, formula = home_team_win ~ . - id,
                   ntimes = 3, data = prediction_data_fin,
                   beta_location = "coefficients", family = binomial(link = "logit"),
                   return_mean_PCC = F)
list_cv_err$reg_no_interact
set.seed(123)
list_cv_err$reg_mean        =
  cross_validation(fct = glm, formula = home_team_win ~ .*.,
                   ntimes = 3, data = Principal_Components_Means_DT,
                   beta_location = "coefficients", family = binomial(link = "logit"),
                   return_mean_PCC = F)
list_cv_err$reg_mean
