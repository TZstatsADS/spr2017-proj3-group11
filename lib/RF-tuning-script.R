
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

## Tuning Random Forest
## Tuning step 1 ##
## use package to tune num of sample in terminal nodes (nodesize),
# number of tree to use(ntree),Number of featuresas candidates at each split(mtry),
# num of sample in terminal nodes (nodesize)
gbmGrid <-  expand.grid(nodesize = seq(20,500,length.out = 20),ntree = 100)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

rfFit1<- train(y ~ .,
                data = Train,
                method = "rf", 
                trControl = fitControl, 
                verbose = FALSE, 
                ## Now specify the exact models 
                ## to evaluate:
                tuneGrid = gbmGrid)
rfFit1
best_nodesize = 
######################################################################
## Tuning step 2 ##
# number of tree to use(ntree)

gbmGrid <-  expand.grid(nodesize = best_nodesize,ntree = (1:30)*50)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

rfFit2<- train(y ~ .,
               data = Train,
               method = "rf", 
               trControl = fitControl, 
               verbose = FALSE, 
               ## Now specify the exact models 
               ## to evaluate:
               tuneGrid = gbmGrid)
rfFit2
best_ntree = 
######################################################################
## Tuning step 3 ##
## Number of featuresas candidates at each split(mtry),
gbmGrid <-  expand.grid(nodesize = best_nodesize,ntree =best_ntree,
                        mtry = 2^(1:12))

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

rfFit3<- train(y ~ .,
               data = Train,
               method = "rf", 
               trControl = fitControl, 
               verbose = FALSE, 
               ## Now specify the exact models 
               ## to evaluate:
               tuneGrid = gbmGrid)
rfFit3
best_mtry = 
######################################################################