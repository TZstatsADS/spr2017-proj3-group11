#since NN Classifier needs extra scaling of the data while SVM does not,
#we wrote 2 separate functions of data preprocess and feature selection 

#Fea is the Feature table, where row is obs and column is feature
#y is the label table, where ncol = 1
feature_selection_mlp = function (Fea, y)

{

  Data = cbind(y,Fea)
  Data = as.data.frame(Data)
  
  #Data[,1] = as.factor(Data[,1])
  # change colnames
  colnames(Data) = c("y",paste0("V",1:(ncol(Data)-1)))
  ######################################################################
  ## feature reduction ##
  
  x = scale(Data[,2:5001])
  
  # alpha = 1 is LASSO
  fit.lasso = cv.glmnet(x,y$V1,alpha = 1,nfolds = 10)
  coef.lasso = predict(fit.lasso,type = "coefficients",s = fit.lasso$lambda.min)
  coef_df = data.frame(coef = rownames(as.matrix(coef.lasso)),values = as.matrix(coef.lasso)[,1])
  coef_df = coef_df[abs(coef_df$values) > 0,][-1,]
  write.csv(coef_df, "../data/lasso_coefficients.csv")
  index = as.character(coef_df$coef)
  index = c("y",index) #add the y column, drop the intercept
  Data = Data[,index]                   
  ######################################################################
  ## scale the data for neural network ##
  #use the min-max method
  maxs <- apply(Data, 2, max) 
  mins <- apply(Data, 2, min)
  scaled <- as.data.frame(scale(Data, center = mins, scale = maxs - mins))
  
  ## divide train-test(80%-20%) ##
  set.seed(400)
  sam = sample(1:2000,1600)
  Train = scaled[sam,]
  Test = scaled[-sam,]
  write.csv(Train,"../data/Train_nn.csv",row.names = F)
  write.csv(Test,"../data/Test_nn.csv",row.names = F)

}



#Fea is the Feature table, where row is obs and column is feature
#y is the label table, where ncol = 1
feature_selection_svm = function(Fea, y)
  
{
  Data <- cbind(y,Fea)
  Data <- as.data.frame(Data)
  Data[,1] <- as.factor(Data[,1])
  colnames(Data) = c("y",paste0("V",1:(ncol(Data)-1)))
  #feature reduction
  index <-read.csv("../data/lasso_coefficients.csv",as.is = T,header = T)
  index <-c("y",as.matrix(index)[,1])
  Data <- Data[,index]     
  set.seed(400)
  index = sample(1:2000,1600)
  Train = Data[index,]
  Test = Data[-index,]
  write.csv(Train,"../data/Train_svm.csv",row.names = F)
  write.csv(Test,"../data/Test_svm.csv",row.names = F)
  
}
