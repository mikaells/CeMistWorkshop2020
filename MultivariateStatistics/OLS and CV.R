#### simple OLS & CV ####
library(caret)

# read in the data
data("swiss")
dat=as.data.frame(swiss)

# ols by hand

# solve the OLS via solve
X=as.matrix(dat[,2:6])
Y=as.matrix(dat[,1])

b_sol=solve(t(X)%*%X)%*%t(X)%*%Y # solve function to get reg coefs

#b) include the intercept
x0=rep(1,length(X[,1]))
X_int=cbind(x0,X)

b=solve(t(X_int)%*%X_int)%*%t(X_int)%*%Y
b=cbind(b)

#c,d) mean squared error
n=dim(dat)[1]
p=dim(dat)[2]

mse=1/n*sum((Y-X_int%*%b)^2)
rmse=sqrt(mse)

rss=sum((Y-X_int%*%b)^2 )
TSS=sum( (Y-mean(Y) )^2 )
R2=1-(rss/TSS)

# ols in R
fit1=lm(Fertility~.,data=dat) # only main effect
summary(fit1)

pred=predict(fit1,dat[,-1]) # predictions found from r
mse=sum((pred-dat[,1])^2/n)
mse=sqrt(mse)

# find the best lm model via stepwise algorithmn
res=step(fit1,direction = "both")
summary(res)

pred=predict(res,dat[,-1]) # predictions found from r
mse=sum((pred-dat[,1])^2/n)
mse=sqrt(mse)


fit2=lm(Fertility~.^2,data=dat)#include interactions 
res2=step(fit2,direction = "both")
summary(res2)

pred2=predict(fit2,dat[,-1]) # predictions found from r
mse2=sum((pred2-dat[,1])^2/n)
mse2=sqrt(mse2)

# include higher order interactions 3 and above - you can play with this 
fit3=lm(Fertility~.^3,data=dat)#include interactions 
res3=step(fit3,direction = "both")
summary(res3)

pred3=predict(fit3,dat[,-1]) # predictions found from r
mse3=sum((pred3-dat[,1])^2/n)
mse3=sqrt(mse3)


rbind(AIC(res),AIC(res2),AIC(res3))# compare models - choose lower AIC
rbind(mse,mse2,mse3) # compares models choose loweste mse

# test model assumtions
par(mfrow=c(2,2))
plot(res)
plot(res2)
plot(res3)

#it seems the higher interactions inclusion is the best model of the traning data (blue)

# try the model using cross-validation

X=dat
# split data randomly into testing and traning data
smp_size <- floor(0.75 * nrow(X)) # Randomly choose 75% of data as traning data 
train_in<- sample(seq_len(nrow(X)), size = smp_size) 

train <- X[train_in, ] # split in traing 
test <- X[-train_in, ] # split in test

# use the caret package for CV - most simple choice - other options exisis tho
train.control <- trainControl(method = "LOOCV") # leave one out CV - try with k-fold-cv and repeated k-fold cv

fit1_cv <- train(Fertility ~., data = train, method = "lm",
               trControl = train.control)

fit2_cv <- train(Fertility ~.^2, data = train, method = "lm",
                 trControl = train.control)

fit3_cv <- train(Fertility ~.^3, data = train, method = "lm",
                 trControl = train.control)

print(fit1_cv)
print(fit2_cv)
print(fit3_cv)

# try on testing data
pred1_cv=predict(fit1_cv,test)
pred2_cv=predict(fit2_cv,test)
pred3_cv=predict(fit3_cv,test)

n_test=length(test)

mse1_cv=sum((pred1_cv-test[,1])^2/n_test)
rmse1_cv=sqrt(mse1_cv)

mse2_cv=sum((pred2_cv-test[,1])^2/n_test)
rmse2_cv=sqrt(mse2_cv)

mse3_cv=sum((pred3_cv-test[,1])^2/n_test)
rmse3_cv=sqrt(mse3_cv)





