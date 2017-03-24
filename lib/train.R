###################################################################
### Tune and Train a classification model with training images ####
###################################################################

### Author: Ruxue Peng, Raphaël Ruscassie, Yifei Tang, Connie Zhou, Hongyi Zhu
### Project 3 Group 11
### ADS Spring 2017


# Tune a Multi-layer perceptron model
mlp_tune = function(Train,Test)
{
  
  size={}
  iteration={}
  trainAcc={}
  testAcc={}  
  
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
  ########################################################################################
  ## (2)loading library ##
  
  # R.version
  # #if your R version is not 3.3.3(2017-03-06),update it first by running:
  # install.packages("installr")
  # installr::install.R()
  # install.packages("RSNNS")
  
  
  ########################################################################################
  # ## (3)Train Multilayer Perceptron Classifier ##
  # stm = proc.time()
  # MLP = mlp(Train[,2:ncol(Train)], Train[,1], 
  #           size = c(5), maxit = 20,
  #           initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
  #           learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
  #           updateFunc = "Topological_Order", updateFuncParams = c(0),
  #           hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
  # etm = proc.time()-stm
  # ########################################################################################
  # ## (4)Accuracy and time##
  # predictions_train <- predict(MLP,Train[,2:ncol(Test)])
  # predictions_train = ifelse(predictions_train>0.5,1,0)
  # TrainAccu = (1-mean(predictions_train!=Train[,1]))*100
  # 
  # predictions <- predict(MLP,Test[,2:ncol(Test)])
  # predictions = ifelse(predictions>0.5,1,0)
  # TestAccu = (1-mean(predictions!=Test[,1]))*100
  # cat(paste("It takes",etm[3],"sec to train.\n",
  #           "Accuracy on the Train set: ",TrainAccu,"%\n",
  #           "Accuracy on the Test set: ",TestAccu,"%\n"))
  ########################################################################################
  ## (5)tune parameter to avoid over-fit
  
  #Since One hidden layer is always sufficient for the large majority of problems.
  #Let's first tune the number of units in the hidden layer(size)
  ## and tune the iteration number ##
  #a suitable size can tackle overfit
  set.seed(129)
  for(i in 5:30 ){
    for(j in 10:30){
      stm = proc.time()
      MLP = mlp(Train[,2:ncol(Train)], Train[,1], 
                size = c(i), maxit = j,
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
      # cat(paste("It takes","C(",i,"),iter=",j,":",etm[3],"sec to train.\n",
      #           "Accuracy on the Train set: ",TrainAccu,"%\n",
      #           "Accuracy on the Test set: ",TestAccu,"%\n"))
      size=append(size,i)
      iteration=append(iteration,j)
      trainAcc=append(trainAcc,TrainAccu)
      testAcc=append(testAcc,TestAccu)
      
    }
  }
  
  x=rbind(size, iteration, testAcc,trainAcc)
  result=as.data.frame(t(x))
  result$difference=abs(trainAcc-testAcc)

  return(result)
  
}



mlp_train = function(size, maxit, Train)
{
  set.seed(129)
  stm = proc.time()
  MLP = mlp(Train[,2:ncol(Train)], Train[,1], 
            size = size, maxit = maxit,
            initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
            learnFunc = "Std_Backpropagation", learnFuncParams = c(0.1, 0),
            updateFunc = "Topological_Order", updateFuncParams = c(0),
            hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = FALSE)
  etm = proc.time()-stm
  return(MLP)
}


svm_tune = function (Train, Test)
{
  cost={}
  trainAcc={}
  testAcc={}  
  
  for (i in seq(0.0001,0.0020,0.0001))
  {
    stm <- proc.time()
    ## SVM linear 
    linear.svm<-svm(x =Train[,-1] ,y =Train[,1], cost = c(i),type="C-classification", kernel = "linear",sampling="cross",cross=5)
    # Stop the clock
    etm <- proc.time()-stm
    
    ##Train error
    pred.svm.linear.train <- predict(linear.svm, Train[,-1]) 
    train.error.linear <- sum(pred.svm.linear.train != Train[,1]) / length(Train[,1]) 
    Train.Accuracy<-1-train.error.linear
    
    
    ##predict
    pred.svm.linear <- predict(linear.svm, Test[,-1]) 
    # test error
    error.linear <- sum(pred.svm.linear != Test[,1]) / length(Test[,1]) 
    Test.Accuracy<-1-error.linear
    
    #cat(paste("It takes",etm[3],"seconds to train.\n",
     #         "Accuracy on the Train set: ",Train.Accuracy*100,"%\n",
      #        "Accuracy on the Test set: ",Test.Accuracy*100,"%\n"))
    
    
    
    
    
    cost=append(cost,i)
    trainAcc=append(trainAcc,Train.Accuracy)
    testAcc=append(testAcc,Test.Accuracy)
  }
  
  
  x=rbind(cost, testAcc,trainAcc)
  result=as.data.frame(t(x))
  result$difference=abs(trainAcc-testAcc)
  
  return(result)
  
}


svm_train = function(cost, Train)
{
  stm <- proc.time()
  ## SVM linear 
  linear.svm<-svm(x =Train[,-1] ,y =Train[,1], cost = cost,type="C-classification", kernel = "linear",sampling="cross",cross=5)
  # Stop the clock
  etm <- proc.time()-stm
  
  return(linear.svm)
}
