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

calc_missclaf_errof = function(threshold_int, prob, truth) {
  predictions_test_response  =  ifelse(prob > threshold_int, 1,0 ) 
  correctly_classified       =  sum(diag(
    prop.table(table(predictions_test_response,truth)))) 
  return(correctly_classified)
}


cross_validation = function(fct, beta_location = "Beta", ntimes = 5, data, formula,...,
                            return_mean_PPC= T){
  splits = sample(x = 1:ntimes, size = nrow(data), replace = T)
  res_array = c()
  thrashold_array = c()
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
    thrashold_array = c(thrashold_array, opt_pcc[[1]])
  }
  res = ifelse(test = return_mean_PPC ,
               mean(res_array),
               list(PPC = res_array, Opt_TH = thrashold_array))
  return(res)
}
