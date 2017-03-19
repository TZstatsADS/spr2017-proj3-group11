######################################################################
## Data Preparation ##
Fea = read.csv("sift_features.csv",as.is = T, header = T)
Fea = t(Fea) #the features as column, data object as row
# for randomForest to use classification unstead of regression,
# we need to factorize y
y_1 = read.csv("../labels.csv",as.is = T) #0 for chicken(not a dog), 1 for dog
Data = cbind(y_1,Fea)
Data = as.data.frame(Data)
# change colnames
names = c("y",paste0("Fea",1:5000))
colnames(Data) = names

## divide train-test(80%-20%)
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

######################################################################
library("gbm")
#start the clock
ptm <- proc.time()
## PS. this version of ADABOOST needs response to be {0,1}
b = gbm(y~.,data = Train,
        distribution = "adaboost",
        n.trees = 1200,
        shrinkage=0.01,
        interaction.depth = 1,
        bag.fraction = 0.8,
        train.fraction = 1,
        cv.folds=5,
        keep.data = TRUE,
        verbose = "CV")
best_iter = gbm.perf(b, method="cv", plot=FALSE)
# Stop the clock
proc.time() - ptm #228 sec
######################################################################
##predict time
ptm <- proc.time()
y_adj = ifelse(unlist(y_1)==0,-1,1)
f.predict = predict(b,Train,best_iter)
train_rate = mean(sign(f.predict)!= y_adj[index]) #0.195
t.predict = predict(b,Test,best_iter)
test_rate = mean(sign(t.predict)!= y_adj[-index]) #0.255
proc.time() - ptm # 12 sec
######################################################################