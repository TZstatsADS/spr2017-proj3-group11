######################################################################
## Data Preparation ##
Fea = read.csv("sift_features.csv",as.is = T, header = T)
Fea = t(Fea) #the features as column, data object as row

# for randomForest to use classification unstead of regression,
# we need to factorize y
y = read.csv("labels.csv",as.is = T) #0 for chicken(not a dog), 1 for dog
Data = cbind(y,Fea)
Data = as.data.frame(Data)

#Data[,1] = as.factor(Data[,1])
# change colnames
colnames(Data) = c("y",paste0("V",1:(ncol(Data)-1)))
######################################################################
## feature reduction ##

library(glmnet)
x = scale(Data[,2:5001])
fit.lasso = cv.glmnet(x,y$V1,alpha = 1,nfolds = 10)
coef.lasso = predict(fit.lasso,type = "coefficients",s = fit.lasso$lambda.min)
coef_df = data.frame(coef = rownames(as.matrix(coef.lasso)),values = as.matrix(coef.lasso)[,1])
coef_df = coef_df[abs(coef_df$values) > 0,][-1,]

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
write.csv(Train,"Train_nn.csv",row.names = F)
write.csv(Test,"Test_nn.csv",row.names = F)