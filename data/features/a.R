library(jpeg)

# testing for one 
img = readJPEG("img_zip_train_1.jpg")
image(img,col=grey((0:12)/12))

# read data 
temp=list.files(pattern="*.jpg")
result = list()
for (i in 1:length(temp)) 
  result[[length(result)+1]] = readJPEG(temp[i])

mat = data.frame(t(sapply(result,c)))
greyscale = rowMeans(mat, na.rm = FALSE, dims = 1)
greyscale = data.frame(image)

qchisq(p=0.05,df=2)

