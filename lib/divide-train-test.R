y = read.csv("labels.csv",as.is = T,header = T)
X = read.csv("sift_features.csv",as.is = T,header = T)
X = t(X)
Data = as.data.frame(cbind(y,X))
write.csv(Data,"Data.csv",row.names = F)

#use 80%-20% train-test
set.seed(200)
index = sample(1:2000,1600)
Train = Data[index,]
Test = Data[-index,]

write.csv(Train,"Train.csv",row.names = F)
write.csv(Test,"Test.csv",row.names = F)
