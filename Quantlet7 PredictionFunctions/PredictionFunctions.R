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

# #Function to calculate the percentage correctly classified ------------------

calc_pcc = function(threshold_int, prob, truth) {
  predictions_test_response  =  ifelse(prob > threshold_int, 1,0 ) 
  #Sum the true positive and true negative 
  correctly_classified       =  sum(diag(
    prop.table(table(predictions_test_response,truth)))) 
  return(correctly_classified)}

# Cross Validation --------------------------------------------------------
#Function to return crossvalidation error, function for any additive model with formula interface 

cross_validation = function(fct, beta_location = "Beta", ntimes = 5, data, formula,...,
                            return_mean_PCC= T){
  splits          = sample(x = 1:ntimes, size = nrow(data), replace = T) #Creates indizes for number of splists
  res_array       = c()
  threshold_array = c()
  #Create test train split, perform the function, make predictions and calculate PCC
  for (i in 1:ntimes){
    train_data = data[!splits == i,]  
    res_loc    = fct(formula = formula,..., data = train_data) #Model Creation
    test_data  = data[splits == i,]  
    X_test     = model.matrix(object = formula, data = test_data)
    y_test     = test_data[[formula[[2]]
                        ]]
    predictions_test = X_test %*% res_loc[[beta_location]] #We need to know where the betas are stored
    opt_pcc          = optimise(calc_pcc,interval = c(0,1), maximum = T, #find ideal threshold, and return associated PCC
                          tol = 0.01, prob = predictions_test, truth = y_test)
    res_array        = c(res_array, opt_pcc[[2]]) #Save the threshold
    threshold_array  = c(threshold_array, opt_pcc[[1]]) }#Save the ppc  
  #For optimizations purposes, we want to get a single number with mean PCC
  #Other times, we want to get the list of the results
  if  (return_mean_PCC) res = mean(res_array) else res = list("PCC" = res_array, "Opt_TH" = threshold_array)
  return(res)}
