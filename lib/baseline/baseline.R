
setwd("C:/Users/rp2815/Downloads/training_data/training_data/sift_features")

######################################################################

## Data Preparation
Fea = read.csv("sift_features.csv",as.is = T, header = T)
Fea = t(Fea) #the features as column, data object as row
y = read.csv("../labels.csv",as.is = T) #0 for chicken(not a dog), 1 for dog
Data = cbind(y,Fea)
Data = as.data.frame(Data)
install.packages("gbm")
library("gbm")
######################################################################

## divide train-test(80%-20%)
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

######################################################################

## try gbm to build boosted decision stumps

#start the clock
ptm <- proc.time()
## PS. this version of ADABOOST needs response to be {0,1}
a = gbm(V1~.,data = Train,
        distribution = "adaboost",
        n.trees = 100,
        shrinkage=0.1,
        interaction.depth = 1,
        bag.fraction = 1,
        train.fraction = 1,
        cv.folds=5,
        keep.data = TRUE,
        verbose = "CV")
best_iter = gbm.perf(a, method="cv", plot=FALSE)
# Stop the clock
proc.time() - ptm #69 sec
######################################################################
##predict time
ptm <- proc.time()
y_adj = ifelse(y==0,-1,1)
f.predict = predict(a,Train,best_iter)
train_rate = mean(sign(f.predict)!= y_adj[index]) #0.21
t.predict = predict(a,Test,best_iter)
test_rate = mean(sign(t.predict)!= y_adj[-index]) #0.27
proc.time() - ptm #12 sec
######################################################################
