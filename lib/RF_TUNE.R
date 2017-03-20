######################################################################
## Data Preparation ##
Fea = read.csv("sift_features.csv",as.is = T, header = T)
Fea = t(Fea) #the features as column, data object as row

# for randomForest to use classification unstead of regression,
# we need to factorize y
y = read.csv("labels.csv",as.is = T) #0 for chicken(not a dog), 1 for dog
Data = cbind(y,Fea)
Data = as.data.frame(Data)

#delete the feature that has 0 variance
M=apply(Data,2,mean) #mean of M = 3e-4
V = apply(Data,2,var)
sum(sqrt(V)>3e-4) #3773 features
Data = Data[,sqrt(V)>3e-4]

Data[,1] = as.factor(Data[,1])
# change colnames
colnames(Data)[1] = "y"

######################################################################
## library packages ##

#install.packages("randomForest")
#install.packages("caret")
library("caret")
library("randomForest")
######################################################################
## divide train-test(80%-20%) ##
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

######################################################################
## Tuning Random Forest ##

## Tuning step 0 ##

# build our own RF to tune multiple parameter at the same time
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
                                 ## Put what to tune in the df below
customRF$parameters <- data.frame(parameter = c("nodesize", "ntree","mtry"), class = rep("numeric", 3), label = c("nodesize", "ntree","mtry"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, nodesize = param$nodesize, mtry = param$mtry,ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
customRF

######################################################################
## Tuning step 1 ##

## use package to tune num of sample in terminal nodes (nodesize),
# number of tree to use(ntree),Number of featuresas candidates at each split(mtry),

#install.packages("caret")
library(caret)
mtryGrid <-  expand.grid(nodesize = seq(10,100,by = 10),ntree = seq(1,1000,by =50),mtry = 1000)

ctrl <- trainControl(## 10-fold CV
          method = "repeatedcv",
          number = 10,
          ## repeated ten times
          repeats = 10)

rfFit1<- train(y ~ .,data = Train,method = customRF,trControl = ctrl,tuneGrid = mtryGrid,importance = TRUE)
rfFit1
best_nodesize = #
best_ntree = #
######################################################################
## Tuning step 2 ##
## Number of featuresas candidates at each split(mtry)
mtryGrid <-  expand.grid(nodesize = best_nodesize,ntree =best_ntree,
                        mtry = 2^(1:10))

rfFit2<- train(y ~ .,data = Train,method = customRF,trControl = ctrl,tuneGrid = mtryGrid,importance = TRUE)
rfFit2
best_mtry = #
######################################################################
## Evaluate Performance of the tuned RF ##

#start the clock
ptm <- proc.time()
##
RF = randomForest(y~.,data = Train,ntrees = best_ntree,nodesize = best_nodesize,mtry = best_mtry,
                  xtest = Test[,-1],ytest = Test[,1]) 

# Stop the clock
proc.time() - ptm # sec
######################################################################
##predict time
ptm <- proc.time()
f.predict = RF$predicted
train_rate = mean(f.predict!= Train[,1]) #
t.predict = RF$test[[1]]
test_rate = mean(t.predict!= Test[,1]) #
proc.time() - ptm # sec

