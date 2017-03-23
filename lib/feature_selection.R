######################################################################

## feature selection using lasso coefficients

######################################################################

library(dplyr)

feature_selection = function(df){
  library(glmnet)
  
  #scale data
  x = scale(df[,-1])
  y = df[,1]
  
  #fit lasso
  fit.lasso = cv.glmnet(x,y,alpha = 1,nfolds = 10)
  
  #predict coef and select relevant variables only
  coef.lasso = predict(fit.lasso,type = "coefficients",s = fit.lasso$lambda.min)
  coef_df = data.frame(coef = rownames(as.matrix(coef.lasso)),values = as.matrix(coef.lasso)[,1])
  coef_df = coef_df[abs(coef_df$values) > 0,][-1,]
  return(coef_df)
}

feature_selection(train_features)

