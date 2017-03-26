RESULT = list()
CVFORSIZE = list()
set.seed(100)
folds = sample(1:4,2000,replace = T)
#change size = c(a) or maxit = a to tune either one
for(a in 10:20){
  for(i in 1:4){
    Train_cv = Data[folds ==i,]
    Test_cv = Data[folds !=i,]
    stm = proc.time()
    MLP = mlp(Train_cv[,2:ncol(Train_cv)], Train_cv[,1], 
              size = c(2), maxit = 14,
              initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
              learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
              updateFunc = "Topological_Order", updateFuncParams = c(0),
              hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
    etm = proc.time()-stm
    predictions_train <- predict(MLP,Train_cv[,2:ncol(Train_cv)])
    predictions_train = ifelse(predictions_train>0.5,1,0)
    TrainAccu = (1-mean(predictions_train!=Train_cv[,1]))*100
    
    predictions <- predict(MLP,Test_cv[,2:ncol(Test_cv)])
    predictions = ifelse(predictions>0.5,1,0)
    TestAccu = (1-mean(predictions!=Test_cv[,1]))*100
    cat(paste("It takes",etm[3],"sec to train.\n",
              "Accuracy on the Train set: ",TrainAccu,"%\n",
              "Accuracy on the Test set: ",TestAccu,"%\n"))
    #Train 94% Test 89.5%
    print(i)
    RESULT[[i]] = c(TrainAccu,TestAccu)
  }
  CVFORSIZE[[a]] = RESULT
  print(a)
}
#Examine the train/validation diff
for(i in 10:20){
  print(mean(unlist(lapply(CVFORSIZE[[i]],function(t) t[1])))-mean(unlist(lapply(CVFORSIZE[[i]],function(t) t[2]))))
}
for(i in 10:20){
  print(mean(unlist(lapply(CVFORSIZE[[i]],function(t) t[2]))))
}
