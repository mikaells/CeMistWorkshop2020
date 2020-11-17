#### PLS workshop 2020 script ####

#### PCR workshop 2020 ####

# data read
library(chemometrics, warn.conflicts = FALSE)
data(PAC)

# center and scale data
dat=as.data.frame(PAC)
dat_scale=scale(dat)

# split data randomly into testing and traning data
smp_size <- floor(0.75 * nrow(dat_scale)) # Randomly choose 75% of data as traning data 
train_in<- sample(seq_len(nrow(dat_scale)), size = smp_size) 

train <- dat_scale[train_in, ] # split in traing 
test <- dat_scale[-train_in, ] # split in test

train=as.data.frame(train)

X=train[,-1]
y=train[,1]

# PCR (princple component regression)
library(caret)

model <- train(
  y~., data = train, method = "pls",
  scale = FALSE,
  trControl = trainControl("cv", number = 10 ),
  tuneLength = 100
)

plot(model)

PC_opt=model$bestTune

pred=predict(model,X)

# traning data
data.frame(
  RMSE = caret::RMSE(pred, y),
  Rsquare = caret::R2(pred, y)
)

# testing data
X=test[,-1]
y=test[,1]

pred=predict(model,X_test)

data.frame(
  RMSE = caret::RMSE(pred, y_test),
  Rsquare = caret::R2(pred, y_test)
)

# resampling to orignal space via jack-knifing
library(pls)
X=train[,-1]
y=train[,1]

mod <- plsr(y ~ . , ncomp = PC_opt$ncomp, data = train,validation="LOO", scale = FALSE, jackknife = TRUE)
obsfit <- predplot(mod, labels = rownames(train), which = "validation")
abline(lm(obsfit[,2] ~ obsfit[,1]))
plot(mod, "validation", estimate = c("train", "CV"), val.type = "R2",legendpos = "bottomright")
coefplot(mod, se.whiskers = TRUE, labels = prednames(mod), cex.axis = 0.5)
biplot(mod)

jack.test(mod, ncomp = PC_opt$ncomp)
