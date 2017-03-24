######################################################
### Fit the classification model with testing data ###
######################################################

### Author: Ruxue Peng, Raphaël Ruscassie, Yifei Tang, Connie Zhou, Hongyi Zhu
### Project 3 Group 11
### ADS Spring 2017

mlp_test <- function(MLP, Test, Train){
  stm = proc.time()
  predictions_train <- predict(MLP,Train[,2:ncol(Test)])
  predictions_train = ifelse(predictions_train>0.5,1,0)
  TrainAccu = (1-mean(predictions_train!=Train[,1]))*100
  etm = proc.time()-stm
  
  # Predict
  predictions <- predict(MLP,Test[,2:ncol(Test)])
  predictions = ifelse(predictions>0.5,1,0)
  TestAccu = (1-mean(predictions!=Test[,1]))*100
  cat(paste("It takes",etm[3],"sec to predict.\n",
            "Accuracy on the Train set: ",TrainAccu,"%\n",
            "Accuracy on the Test set: ",TestAccu,"%\n"))
  #Train 93% Test 90%
  
  return(predictions)
}

svm_test = function(linear.svm, Test, Train)
  
{
  stm = proc.time()
  
  ##Train error
  pred.svm.linear.train <- predict(linear.svm, Train[,-1]) 
  train.error.linear <- sum(pred.svm.linear.train != Train[,1]) / length(Train[,1]) 
  Train.Accuracy<-1-train.error.linear
  etm = proc.time()-stm
  
  
  # Predict
  pred.svm.linear <- predict(linear.svm, Test[,-1]) 
  # test error
  error.linear <- sum(pred.svm.linear != Test[,1]) / length(Test[,1]) 
  Test.Accuracy<-1-error.linear
  
  
  cat(paste("It takes",etm[3],"seconds to train.\n",
            "Accuracy on the Train set: ",Train.Accuracy*100,"%\n",
            "Accuracy on the Test set: ",Test.Accuracy*100,"%\n"))
  
  return(pred.svm.linear)
}