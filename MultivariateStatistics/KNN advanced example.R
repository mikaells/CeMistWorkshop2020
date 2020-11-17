# Simple KNN base script workshop 2020 ###

# 2.1) KKN classification 

library('pander') # Printing of vectors
library('shiny') # Interactive plots
library('pracma') # load pracma library for 'logspace'
library('cvTools') # load cvTools library (includes cvFolds)
library('ggplot2') # ggplot
library('psych') # trace
library('cvTools') # load cvTools library (includes cvFolds)
library('class') # load library class (includes knn)
library('MASS') # LDA

# read in data (fra work space)
class=class
X=X

n = dim(X)[1] # number of observations
p = dim(X)[2] # number of paramter estimates

# plot data
plot(X[1,1:65],X[1,66:p])

# 2.2) + 2.3) size of fold - trial and error 

# K-fold cross-validation - if samll dataset k=5 , larger k=10 - trial and error!

# trying from k=1 t0 15 
K = n; # leave-one-out cross-validation
k = seq(from = 1, to = 15, by=2); # define m values for number of nearest neighbours (uenven)
m = length(k); # try m values of k in knn
miscl = matrix(0,m,K); # prepare vector for miss classification

folds <- cvFolds(n, K = K, R = 1, type = "random") # get the random folds for the CV (hvor mange obs der bliver left out, kan ændre via R=1)

for (j in 1:K){ # K fold cross-validation
  x_tr = X[folds$subsets[folds$which!=j],] # training data
  x_tst = t(as.matrix(X[folds$subsets[folds$which==j],])) # test data
  
  x_tr <- scale(x_tr)
  x_tst <- scale(x_tst,center = attr(x_tr, "scaled:center"), scale = attr(x_tr, "scaled:scale"))
  c_tr = class[folds$subsets[folds$which!=j]] # training data
  c_tst = class[folds$subsets[folds$which==j]] # test data
  
  for (i in 1:m){ # repeat m times
    cl <- knn(train = x_tr, test = x_tst, cl = c_tr, k = k[i]) # perform knn
    miscl[i,j] = sum(cl!=c_tst)/length(cl) # misclassificaiton error
  }
}
meanMiscl = apply(miscl,1,mean) # find the mean mse over the K cv folds
Imin = which.min(meanMiscl) # find the optimal value
cat('Optimal number (k) of nearest neighbours:',k[Imin])

# plot the results (log x-axis, linear y-axis), exclude the intercept
plot(k, meanMiscl, type='l',xlab="k (number of nearest neighbours)",ylab="misclassification rate") # plot mean misclasifications for test

# plot the optimal lambda
lines(c(k[Imin],k[Imin]), c(0,1), lty = 2)





