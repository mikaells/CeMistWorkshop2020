### multi level data in high dimensions ####


# FDR correct vs bonferoni correction
library(lme4)
library(R.matlab)#read matlab 
dat <- readMat(file.path('sand.mat'))
X=dat$X[,-1297] # list structure of x
Y=dat$Y  # list structure of y

p_value=c() #vector to hold all p values 
for (j in 1:(dim(X)[2])){
  YX=data.frame(Y,X[,j]) # create a dataset
  fm1=lm(formula = Y ~ X[,j], data = YX) # fit linear model 
  modelSummary <- summary(fm1)  # capture model summary as an object
  modelCoeffs <- modelSummary$coefficients  # model coefficients
  beta.estimate <- modelCoeffs[2, "Estimate"]  # get beta estimate for X[,j]
  std.error <- modelCoeffs[2, "Std. Error"]  # get std.error for X[,j]
  t_value <- beta.estimate/std.error  # calc t statistic
  p_value[j] <- 2*pt(-abs(t_value), df=nrow(YX)-ncol(YX)) # calculate p values
}

# using Bonferroni
p_value=sort(p_value)
a=sum(p_value<(0.05/2016)) #counts how many features are below alpha = 0.05/2016 test level?

# using Benjamini-Hochberg
fdr=p.adjust(p_value, method = "BH", n = length(p_value)) # adjust p-values 
fdr=sort(fdr)
b= sum(fdr<0.15) # counts how many are below q = 0.15? hence the first b-number of values are sginifcant 

# the higher q, the more are potententially false negativ - ex q=0.10 vil 10% være falske negativer
