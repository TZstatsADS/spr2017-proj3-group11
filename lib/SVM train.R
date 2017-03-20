setwd("D:/study/Applied Data Sci/spr2017-proj3-group11/data/training_data/training_data/sift_features")
######################################################################

## Data Preparation
Fea = read.csv("sift_features.csv",as.is = T, header = T)
Fea = t(Fea) #the features as column, data object as row

y = read.csv("../labels.csv",as.is = T) #0 for chicken(not a dog), 1 for dog
Data = cbind(y,Fea)
Data = as.data.frame(Data)
Data[,1] = as.factor(Data[,1])
# change colnames
names = c("y",paste0("Fea",1:dim(Fea)[2]))
colnames(Data) = names

#delete the feature that has 0 variance  
M=apply(Data,2,mean) #mean of M = 3e-4  
V = apply(Data,2,var)  
sum(sqrt(V)>3e-4) #3773 features  
Data = Data[,sqrt(V)>3e-4]  


#install.packages("e1071")
library("e1071")
###################################################################
# divide train-test(80%-20%)
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

#############################

#start the clock
ptm <- proc.time()
## SVM linear tune (cross validation)
tune.linear <- tune.svm(x =Train[,-1] ,y =Train[,1], cost = c(1,10),type="C-classification", kernel = "linear",sampling="cross",cross=10)
best.svm.linear <- tune.linear$best.model
print(best.svm.linear)
# Stop the clock
proc.time() - ptm #50sec

##predict
pred.svm.linear <- predict(best.svm.linear, Test[,-1]) 
# test error
error.linear <- sum(pred.svm.linear != Test[,1]) / length(Test[,1]) 
print(error.linear)

####################################

#start the clock
ptm <- proc.time()
## SVM nonlinear RBF tune (cross validation)
tune.rbf <- tune.svm(x =Train[,-1] ,y =Train[,1], cost = c(1,5),gamma =seq(0.01,0.1,0.03),type="C-classification", kernel = "radial",sampling="cross",cross=10)
best.svm.rbf <- tune.rbf$best.model
print(best.svm.rbf)
# Stop the clock
proc.time() - ptm 

##predict
pred.svm.rbf <- predict(best.svm.rbf, Test[,-1]) 
# test error
error.rbf <- sum(pred.svm.rbf != Test[,1]) / length(Test[,1]) 
print(error.rbf)

