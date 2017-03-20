##################################
## Tuning  ## 

##Write function for customized tuning parameter
### Train
classifier_cv =  function(distribution = "adaboost",n.trees = 2,shrinkage=0.1,n.minobsinnode = 50,#minimum number of observations in the trees terminal nodes
                   interaction.depth = 3,bag.fraction = 0.8,train.fraction = 1,cv.folds=8){
  ptm <- proc.time()
  classifier = gbm(y~.,data = Train,distribution = distribution,n.trees = n.trees,shrinkage=shrinkage,n.minobsinnode = n.minobsinnode,#minimum number of observations in the trees terminal nodes
                  interaction.depth = interaction.depth,bag.fraction = bag.fraction,train.fraction = train.fraction,cv.folds = cv.folds,keep.data = TRUE,verbose = "CV")
  best_iter_classifier = gbm.perf(classifier, method="cv", plot=FALSE)
  # Stop the clock
  train_time = proc.time() - ptm 
  train_time = as.numeric(train_time[[3]])

### Test
  ##predict time
  ptm <- proc.time()
  y = ifelse(unlist(y_1)==0,-1,1)
  train_rate = mean(sign(gbt$fit)!= y[index]) 
  
  t.predict_gbt = predict(gbt,Test,best_iter_gbt)
  test_rate = mean(sign(t.predict_gbt)!= y[-index])
  # Stop the clock
  test_time = proc.time() - ptm 
  test_time = as.numeric(test_time[[3]])
  
  ## return parameter and performance
  perf = c(Train_rate=train_rate,Test_rate=test_rate,
           interaction.depth=classifier$interaction.depth,n.minobsinnode=classifier$n.minobsinnode,n.trees=classifier$n.trees,#tree related
           shrinkage=classifier$shrinkage,cv.folds=classifier$cv.folds,                                 #other para
           train.fraction=classifier$train.fraction,bag.fraction=classifier$bag.fraction,                        #train fraction
           Train_time=train_time,Test_time=test_time)
  return(perf)
}

####################################################################################################################
## Tuning step 1 ##
## use package to tune depth of tree, minimum number of observations in the trees terminal nodes at the same time
library(caret)
set.seed(200)
###Note: for classifcation tuning, y need to be a factor
Data[,1] = as.factor(Data[,1])
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

gbmGrid <-  expand.grid(interaction.depth = c(2,4,6,8),
                        n.trees = 100,
                        shrinkage = 0.1,
                        n.minobsinnode = seq(20,500,length.out = 20))
#nrow(gbmGrid) trying 80 models

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

gbmFit1<- train(y ~ .,
                 data = Train,
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit1
####################################################################################################################
## Tuning step 2 ## 
## use package to tune step size(shrinkage) and number of trees
gbmGrid <-  expand.grid(interaction.depth = ,n.minobsinnode = 
                        n.trees = (1:30)*50,
                        shrinkage = seq(0.1,0.05,0.01,0.001))

gbmFit2 <- train(y ~ .,
                 data = Train,
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2


####################################################################################################################
## Tuning step 3 ## 
library(plyr)
##Tune bag.fraction
#list of max_depth values
bag.fraction = c(0.6,0.7,0.75,0.8,0.85,0.9)
tune_1 = aaply(as.matrix(bag.fraction),1,function(t) classifier_cv(n.trees = #,shrinkage=###,n.minobsinnode = ##,
                                                                   interaction.depth = #,bag.fraction = bag.fraction))
tune_1[order(tune_1[,2]),]
best_depth = tune_1[1,"bag.fraction"]


# Compare performance with untuned one
classifier_cv()
classifier_cv(n.trees = #,shrinkage=###,n.minobsinnode = ##,interaction.depth = #,bag.fraction =#)


