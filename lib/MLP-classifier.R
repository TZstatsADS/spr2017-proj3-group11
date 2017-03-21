#Multi-layer Perceptron
#Introduction on the basics of MLP:
#http://scikit-learn.org/stable/modules/neural_networks_supervised.html
#2 things about MLP:
#MLP requires tuning a number of hyperparameters such as the number of hidden neurons, layers, and iterations.
#MLP is sensitive to feature scaling.

########################################################################################
## (1)Scaling your data ##
#Multi-layer Perceptron is sensitive to feature scaling, 
#so it is highly recommended to scale your data. !remember to scale your test data to get meaningful result!
#For example, scale each attribute on the input vector X to [0, 1] or [-1, +1], 
#or standardize it to have mean 0 and variance 1. 
#Here we load the scaled data
Train = read.csv("Train_nn.csv",as.is = T,header = T)
Test = read.csv("Test_nn.csv",as.is = T,header = T)
########################################################################################
## (2)loading library ##

R.version
#if your R version is not 3.3.3(2017-03-06),update it first by running:
install.packages("installr")
installr::install.R()
install.packages("RSNNS")

library(RSNNS)
########################################################################################
## (3)Train Multilayer Perceptron Classifier ##
stm = proc.time()
MLP = mlp(Train[,2:ncol(Train)], Train[,1], 
          size = c(5), maxit = 20,
    initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
    learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
    updateFunc = "Topological_Order", updateFuncParams = c(0),
    hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
etm = proc.time()-stm
########################################################################################
## (4)Accuracy and time##
predictions_train <- predict(MLP,Train[,2:ncol(Test)])
predictions_train = ifelse(predictions_train>0.5,1,0)
TrainAccu = (1-mean(predictions_train!=Train[,1]))*100

predictions <- predict(MLP,Test[,2:ncol(Test)])
predictions = ifelse(predictions>0.5,1,0)
TestAccu = (1-mean(predictions!=Test[,1]))*100
cat(paste("It takes",etm[3],"sec to train.\n",
          "Accuracy on the Train set: ",TrainAccu,"%\n",
          "Accuracy on the Test set: ",TestAccu,"%\n"))
########################################################################################
## (5)tune parameter to avoid over-fit

#Since One hidden layer is always sufficient for the large majority of problems.
#Let's first tune the number of units in the hidden layer(size)
#a suitable size can tackle overfit
for(i in 5:20){
  stm = proc.time()
  MLP = mlp(Train[,2:ncol(Train)], Train[,1], 
            size = c(i), maxit = 20,
            initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
            learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
            updateFunc = "Topological_Order", updateFuncParams = c(0),
            hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
  etm = proc.time()-stm
  ########################################################################################
  ## (4)Accuracy and time##
  predictions_train <- predict(MLP,Train[,2:ncol(Test)])
  predictions_train = ifelse(predictions_train>0.5,1,0)
  TrainAccu = (1-mean(predictions_train!=Train[,1]))*100
  
  predictions <- predict(MLP,Test[,2:ncol(Test)])
  predictions = ifelse(predictions>0.5,1,0)
  TestAccu = (1-mean(predictions!=Test[,1]))*100
  cat(paste("It takes",etm[3],"sec to train.\n",
            "Accuracy on the Train set: ",TrainAccu,"%\n",
            "Accuracy on the Test set: ",TestAccu,"%\n"))
  
}

## tune the iteration number ##
for(i in 10:30){
  stm = proc.time()
  MLP = mlp(Train[,2:ncol(Train)], Train[,1], 
            size = c(20), maxit = i,
            initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
            learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
            updateFunc = "Topological_Order", updateFuncParams = c(0),
            hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
  etm = proc.time()-stm
  predictions_train <- predict(MLP,Train[,2:ncol(Test)])
  predictions_train = ifelse(predictions_train>0.5,1,0)
  TrainAccu = (1-mean(predictions_train!=Train[,1]))*100
  
  predictions <- predict(MLP,Test[,2:ncol(Test)])
  predictions = ifelse(predictions>0.5,1,0)
  TestAccu = (1-mean(predictions!=Test[,1]))*100
  cat(paste("It takes",etm[3],"sec to train.\n",
            "Accuracy on the Train set: ",TrainAccu,"%\n",
            "Accuracy on the Test set: ",TestAccu,"%\n"))
  
}

stm = proc.time()
MLP = mlp(Train[,2:ncol(Train)], Train[,1], 
          size = c(20), maxit = 17,
          initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
          learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
          updateFunc = "Topological_Order", updateFuncParams = c(0),
          hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
etm = proc.time()-stm
predictions_train <- predict(MLP,Train[,2:ncol(Test)])
predictions_train = ifelse(predictions_train>0.5,1,0)
TrainAccu = (1-mean(predictions_train!=Train[,1]))*100

predictions <- predict(MLP,Test[,2:ncol(Test)])
predictions = ifelse(predictions>0.5,1,0)
TestAccu = (1-mean(predictions!=Test[,1]))*100
cat(paste("It takes",etm[3],"sec to train.\n",
          "Accuracy on the Train set: ",TrainAccu,"%\n",
          "Accuracy on the Test set: ",TestAccu,"%\n"))
#Train 94% Test 89.5%
