#### KNN simple example workshop 2020 ####
library(caret)

X=iris

X=scale(X)

# split data randomly into testing and traning data
smp_size <- floor(0.75 * nrow(X)) # Randomly choose 75% of data as traning data 
train_in<- sample(seq_len(nrow(X)), size = smp_size) 

train <- X[train_in, ] # split in traing 
test <- X[-train_in, ] # split in test

# use the caret lib for cross validaiton of the knn model
ctrl <- trainControl(method="repeatedcv",repeats = 3) # try also the loo and k-fold non repeated cv

knnFit <- train(Species ~.,data=train, method = "knn",trControl=ctrl)
knnFit

plot(knnFit)

knnPredict <- predict(knnFit,newdata = test )
confusionMatrix(knnPredict, test$Species )

mean(knnPredict == test$Species)
