rm(list=ls())

library(data.table)
library(caret)
library(gbm)
library(e1071)
library(pROC)

set.seed(100)
# load sift features into memory
mat=fread("./data//sift_features/sift_features.csv")
#mat=mat*1000
# swap the labels
cn=colnames(mat)
rn=rownames(mat)
# transpose the matrix
mat=transpose(mat)
colnames(mat)=rn
rownames(mat)=cn
rm(cn,rn)
#mat=as.data.frame(mat)




#$chicken=fread("data/labels.csv",header=T)
#mat$chicken=as.vector(mat$chicken)
#
#mat$chicken=as.factor(mat$chicken)
#


####################### feature selection #############################

# # find features that are highly correlated
# correlationMatrix = cor(mat)
# highlyCorrelated=findCorrelation(correlationMatrix,cutoff=0.75)
# 
# # removing the highly correlated columns
# mat=subset(mat,select=-highlyCorrelated)
# rm(highlyCorrelated)
# 
# # find near zero variance
# x=as.data.frame(mat)
# nzv_cols <- nearZeroVar(x)
# if(length(nzv_cols) > 0) mat <- x[, -nzv_cols]
# 
# # removing features with column sum <0.005
# cond=mat[, colSums(mat)>0.005]
# mat=mat[,cond,with=F]
# rm(cond)

# # calculate column sum by class
# ck=mat[1:1000,]
# dog=mat[1001:2000,]
# csck=colSums(ck)
# csdog=colSums(dog)
# # quotient might be a better indicator
# quo=abs(csck/csdog)
# rm(csck,csdog,ck,dog)
# 
# # rbind the quotient row to the end of the matrix
# quo=t(as.data.frame(quo))
# mat=rbind(mat,quo)
# rm(quo)
# 
# # get rid of features with quotient within 0.01 of 1
# throwaway=mat[,(mat[2001]<=0.99 | mat[2001]>1.01)]
# mat=mat[,throwaway,with=F]
# # remove the last row of ratios
# mat=mat[-2001,]
# rm(throwaway)

#########################

mat$chicken=as.factor(c(rep(1,1000),rep(0,1000)))
mat=as.data.frame(mat)
mat1=mat

mat1=mat1[sample(nrow(mat1)),]

split <- floor(nrow(mat1)/3)
ensembleData <- mat1[0:split,]
blenderData <- mat1[(split+1):(split*2),]
testingData <- mat1[(split*2+1):nrow(mat1),]



# set label name and predictors
labelName <- 'chicken'
predictors <- names(ensembleData)[names(ensembleData) != labelName]
#predictors=as.numeric(predictors)
# create a caret control object to control the number of cross-validations performed
myControl <- trainControl(method='cv', number=3, returnResamp='none')

model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method='gbm', trControl=myControl)

model_glmboost <- train(ensembleData[,predictors], ensembleData[,labelName], method='glmboost', trControl=myControl)

model_LogitBoost <- train(ensembleData[,predictors], ensembleData[,labelName], method='LogitBoost', trControl=myControl)

preds <- predict(object=model_LogitBoost, testingData[,predictors])
auc <- roc(testingData[,labelName], as.numeric(preds))
x=print(auc$auc)


# ensemble
blenderData$gbm_PROB <- predict(object=model_gbm, blenderData[,predictors])
blenderData$glmboost_PROB <- predict(object=model_glmboost, blenderData[,predictors])
blenderData$LogitBoost_PROB <- predict(object=model_LogitBoost, blenderData[,predictors])

testingData$gbm_PROB <- predict(object=model_gbm, testingData[,predictors])
testingData$glmboost_PROB <- predict(object=model_glmboost, testingData[,predictors])
testingData$LogitBoost_PROB <- predict(object=model_LogitBoost, testingData[,predictors])


# see how each individual model performed on its own
auc <- roc(testingData[,labelName], as.numeric(testingData$gbm_PROB ))
print(auc$auc) # Area under the curve: 0.7544


auc <- roc(testingData[,labelName], as.numeric(testingData$glmboost_PROB ))
print(auc$auc) # Area under the curve: 0.7258

auc <- roc(testingData[,labelName], as.numeric(testingData$LogitBoost_PROB ))
print(auc$auc) # Area under the curve: 0.6679




predictors <- names(blenderData)[names(blenderData) != labelName]
final_blender_model <- train(blenderData[,predictors], blenderData[,labelName], method='gbm', trControl=myControl)


preds <- predict(object=final_blender_model, testingData[,predictors])

auc <- roc(testingData[,labelName], as.numeric(preds))
x=c(auc$auc, "ensemble overall")
print(x) 
# 0.7440 : remove near zero variance
# 0.7696 : remove highly correlated
# 0.7721 : remove colSums < 0.005
# 0.7443 : remove features with quotient w.r.t. outcome within 0.01 of 1

#write.table(x, "results1.csv", append=TRUE)






#User ID:  hz2412
#Password:  576X47DX
#Application:  AWS Account 835715003829