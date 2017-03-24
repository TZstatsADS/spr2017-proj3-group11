feature_trimmer <- function(mat)
  {
    # Preprocess
    # swap the labels
    cn=colnames(mat)
    rn=rownames(mat)
    # transpose the matrix
    mat=transpose(mat)
    colnames(mat)=rn
    rownames(mat)=cn
  
    # find features that are highly correlated and removing them
    correlationMatrix = cor(mat)
    highlyCorrelated=findCorrelation(correlationMatrix,cutoff=0.75)
    mat=subset(mat,select=-highlyCorrelated)
    
    # removing features with column sum <0.005
    mat=as.data.frame(mat)
    mat=mat[, colSums(mat)>0.005]
    
    # find near zero variance
    nzv_cols <- nearZeroVar(mat)
    if(length(nzv_cols) > 0) mat <- mat[, -nzv_cols]
    
    # calculate column sum by class
    ck=mat[1:1000,]
    dog=mat[1001:2000,]
    csck=colSums(ck)
    csdog=colSums(dog)
    # quotient might be a better indicator
    quo=abs(csck/csdog)
    rm(csck,csdog,ck,dog)
    
    # rbind the quotient row to the end of the matrix
    quo=t(as.data.frame(quo))
    
    # get rid of features with quotient within 0.01 of 1
    mat=mat[,(quo<=0.99 | quo>1.01)]
    
  
    return(mat)
  }