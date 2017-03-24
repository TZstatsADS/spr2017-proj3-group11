
setwd("C:/Users/rp2815/Downloads/training_data/training_data/sift_features")

######################################################################

## Data Preparation
Fea = read.csv("sift_features.csv",as.is = T, header = T)
Fea = t(Fea) #the features as column, data object as row
# for randomForest to use classification unstead of regression,
# we need to factorize y
y_1 = read.csv("../labels.csv",as.is = T) #0 for chicken(not a dog), 1 for dog
Data = cbind(y_1,Fea)
Data = as.data.frame(Data)
Data[,1] = as.factor(Data[,1])
# change colnames
names = c("y",paste0("Fea",1:5000))
colnames(Data) = names

#install.packages("randomForest")
library("randomForest")
######################################################################

## divide train-test(80%-20%)
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

######################################################################

## try RF

#start the clock
ptm <- proc.time()
##
RF = randomForest(y~.,data = Train,ntrees = 80,nodesize = 100,mtry = 10,
                  xtest = Test[,-1],ytest = Test[,1]) 

# Stop the clock
proc.time() - ptm #117sec
######################################################################
##predict time
ptm <- proc.time()
f.predict = RF$predicted
train_rate = mean(f.predict!= Train[,1]) #0.2925
t.predict = RF$test[[1]]
test_rate = mean(t.predict!= Test[,1]) #0.2675
proc.time() - ptm #0 sec
######################################################################
