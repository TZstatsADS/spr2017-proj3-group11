library(jpeg)

# read data 
temp=list.files(pattern="*jpg.jpg")
result = list()
for (i in 1:length(temp)) 
  result[[length(result)+1]] = readJPEG(temp[i])
mat = list()
for (i in 1:length(temp)) 
  mat[[length(mat)+1]] = data.frame(rowMeans(data.frame(result[i])), na.rm = FALSE, dims = 1)

a = data.frame(rowMeans(data.frame(result[1])), na.rm = FALSE, dims = 1)
a = a[,1]
#save(mat, file = "result.RData") #save data in case
#load("result.RData")
#delete colum
res = list()
for (i in 1:length(mat)) 
  res[i] = lapply(mat[i], function(x) { x["dims"] <- NULL; x })

r = list()
for (i in 1:length(mat)) 
  r[i] = lapply(res[i], function(x) { x["dims"] <- NULL; x })

a = lapply(r, as.matrix)
a = lapply(a, mean)

greyscale = t(data.frame(a))

write.table(greyscale, file = "greyscale.csv", sep = ",", col.names = NA,
            qmethod = "double")



