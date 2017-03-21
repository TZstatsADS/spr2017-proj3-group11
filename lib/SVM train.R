

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

#install.packages("e1071")
library("e1071")
###################################################################
# divide train-test(80%-20%)
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

#############################
## SVM linear tune (cross validation)
## tune.linear <- tune.svm(x =Train[,-1] ,y =Train[,1], cost = seq(100,1000,100),type="C-classification", kernel = "linear",sampling="cross",cross=5)
## best.svm.linear <- tune.linear$best.model
## print(best.svm.linear)
#############################

#start the clock
ptm <- proc.time()
## SVM linear 
linear.svm<-svm(x =Train[,-1] ,y =Train[,1], cost = 100,type="C-classification", kernel = "linear",sampling="cross",cross=5)
# Stop the clock
proc.time() - ptm 

##Train error
pred.svm.linear <- predict(linear.svm, Train[,-1]) 
# test error
train.error.linear <- sum(pred.svm.linear != Train[,1]) / length(Train[,1]) 
print(train.error.linear)

##predict
pred.svm.linear <- predict(linear.svm, Test[,-1]) 
# test error
error.linear <- sum(pred.svm.linear != Test[,1]) / length(Test[,1]) 
print(error.linear)



