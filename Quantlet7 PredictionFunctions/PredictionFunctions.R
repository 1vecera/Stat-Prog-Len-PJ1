# Ridge Regression Model --------------------------------------------------

ridge_regression = function(formula, lambda = 0, data) {
  X = model.matrix(object = formula, data = data) #creates a data matrix which can be used
  y = data[[formula[[2]] #gets out the response
            ]]
  Beta = solve(t(X) %*% X +
                 diag(x = lambda, ncol = ncol(X), nrow = ncol(X))) %*% #αdd something to the diag of X'X
                 t(X) %*%  y
  predictions = X %*% Beta #Predictions are just X times the beta coefficient
  SSR         = t((y- predictions)) %*% (y- predictions) #Sum of square residuals 
  MSE         = SSR/length(y) #Means square error
  results     = list(Beta = Beta, predictions = predictions, SSR = SSR, MSE = MSE  )
  return(results)  }

# #Funcitonto calculate the Percante correctly specified ------------------

calc_ppc = function(threshold_int, prob, truth) {
  predictions_test_response  =  ifelse(prob > threshold_int, 1,0 ) 
  #Sum the true positive and true negative 
  correctly_classified       =  sum(diag(
    prop.table(table(predictions_test_response,truth)))) 
  return(correctly_classified)}

# Cross Validation --------------------------------------------------------
#Function to return crossvalidation error, functions for any additive model with formula interface 

cross_validation = function(fct, beta_location = "Beta", ntimes = 5, data, formula,...,
                            return_mean_PPC= T){
  splits          = sample(x = 1:ntimes, size = nrow(data), replace = T) #Creates indizes for number of splists
  res_array       = c()
  thrashold_array = c()
  #Create test train split, perfrom the function, made predictions and calculate PPC
  for (i in 1:ntimes){
    train_data = data[!splits == i,]  
    res_loc    = fct(formula = formula,..., data = train_data) #Model Creation
    test_data  = data[splits == i,]  
    X_test     = model.matrix(object = formula, data = test_data)
    y_test     = test_data[[formula[[2]]
                        ]]
    predictions_test = X_test %*% res_loc[[beta_location]] #We need to know where the betas are stored
    opt_pcc          = optimise(calc_ppc,interval = c(0,1), maximum = T, #find ideal tharshold, and return associated PPC
                          tol = 0.01, prob = predictions_test, truth = y_test)
    res_array        = c(res_array, opt_pcc[[2]]) #Save the tharshold
    thrashold_array  = c(thrashold_array, opt_pcc[[1]]) }#Save the ppc  
  #For optimazations purposes, we want to get a single number with mean PPC
  #Other times, we want to get the list of the results
  if  (return_mean_PPC) res = mean(res_array) else res = list("PPC" = res_array, "Opt_TH" = thrashold_array)
  return(res)}
