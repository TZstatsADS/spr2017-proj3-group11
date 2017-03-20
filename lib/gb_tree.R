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

M = apply(Data,2,mean)#Mean of M is 2.94e-4
V = apply(Data,2,var)
#Choose the features that have a variance larger than 1/1000 of the mean
Data = Data[,V > 2.94e-7]
## divide train-test(80%-20%)
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

######################################################################
#Gradient boosting tree
library("gbm")

#start the clock
ptm <- proc.time()
## PS. this version of ADABOOST needs response to be {0,1}
gbt = gbm(y~.,data = Train,
        distribution = "adaboost",
        n.trees = 100,
        shrinkage=0.1,
        n.minobsinnode = 50,#minimum number of observations in the trees terminal nodes
        interaction.depth = 3,
        bag.fraction = 0.8,
        train.fraction = 1,
        cv.folds=8,
        keep.data = TRUE,
        verbose = "CV")
best_iter_gbt = gbm.perf(gbt, method="cv", plot=FALSE)
# Stop the clock
proc.time() - ptm #96 sec
######################################################################
##predict time
ptm <- proc.time()
y = ifelse(unlist(y_1)==0,-1,1)
train_rate_gbt = mean(sign(gbt$fit)!= y[index]) #0.1
t.predict_gbt = predict(gbt,Test,best_iter_gbt)
test_rate_gbt = mean(sign(t.predict_gbt)!= y[-index]) #0.2625
proc.time() - ptm #12  sec
######################################################################