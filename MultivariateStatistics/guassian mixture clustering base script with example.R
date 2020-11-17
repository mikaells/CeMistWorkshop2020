#### clustering base script 2020 workshop ####
library("mclust")#mixture clustering package

# load data
Tab=iris #iris data is alredy in R base
X=iris[,-5] # Take out species

#from 1 to 10 cluster #compare different models with different covariance structure
GMM.model <- Mclust(X, G=1:10)

mclustAIC<-function(g,x){
  IC <- Mclust(x, G=g)
  aic <- 2*IC$df - 2*IC$loglik
  return(aic)
}
AIC=sapply(1:10,mclustAIC,X)
BIC=mclustBIC(X,G=1:10,modelNames=GMM.model$modelName)
par(mfrow = c(1,1))
{
  plot(1:10,AIC,type="o",pch=20,col="red",cex=1.5)
  lines(1:10,abs(BIC),type="o",pch=18,col="blue",cex=1.5)
  lines(rep(which.min(AIC),2),range(AIC),lty=2,col="red")
  lines(rep(which.max(BIC),2),range(AIC),lty=2,col="blue")
  
  legend(8, 700, c("AIC","BIC","best AIC","best BIC"), cex=1.2,lty=c(1,1,2,2), col=c("red","blue","red","blue"),
         pch=c(19,18,NA,NA))
}

summary(GMM.model, parameters = TRUE) # outcomment to view summary
plot(GMM.model, what = "classification")
