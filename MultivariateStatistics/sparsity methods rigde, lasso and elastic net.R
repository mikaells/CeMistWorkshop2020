#### sparsity methods workshop 2020 ####
library(glmnet)

# data read
library(chemometrics, warn.conflicts = FALSE)
data(PAC)

# center and scale data
dat=as.data.frame(PAC)
dat_scale=scale(dat)

# split into testing and traning data

# split data randomly into testing and traning data
smp_size <- floor(0.75 * nrow(dat_scale)) # Randomly choose 75% of data as traning data 
train_in<- sample(seq_len(nrow(dat_scale)), size = smp_size) 

train <- dat_scale[train_in, ] # split in traing 
test <- dat_scale[-train_in, ] # split in test

X=train[,-1]
y=train[,1]

# rigde regression
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
plot(ridge_cv)

# find the best lambda value
lambda_cv <- ridge_cv$lambda.min # chose the +one std

# Fit final model, get its sum of squared residuals and multiple R-squared
model_cv <- glmnet(X, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)

# traning error
y_hat_cv <- predict(model_cv, X)
mse=sum((y_hat_cv-y)^2/length(X))
sqrt(mse)

# testing error
X_test=test[,-1]
y_test=test[,1]

y_hat_cv_test <- predict(model_cv, X_test)
mse_test_rigde=sum((y_hat_cv_test-y_test)^2/length(X_test))
sqrt(mse_test_rigde)

# lasso
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)

# Plot cross-validation results
plot(lasso_cv)

# get lasso lambda
lambda_cv <- lasso_cv$lambda.min

# Fit final model, get its sum of squared residuals and multiple R-squared
model_cv2 <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
y_hat_cv2 <- predict(model_cv2, X)
mse2=sum((y_hat_cv2-y)^2/length(X))
sqrt(mse2)

# testing error
y_hat_cv_test2 <- predict(model_cv2, X_test)
mse2_test_lasso=sum((y_hat_cv_test2-y_test)^2/length(X_test))
sqrt(mse2_test_lasso)


# elastic net
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              verboseIter = TRUE)

elastic_net_model <- train(y ~ .,
                           data = cbind(y, X),
                           method = "glmnet",
                           tuneLength = 25,
                           trControl = train_control)
# traning error
y_hat_enet <- predict(elastic_net_model, X)
mse3=sum((y_hat_enet-y)^2/length(X))
sqrt(mse3)

# testin error
y_hat_enet_test <- predict(elastic_net_model, X_test)
mse3_test_enet=sum((y_hat_enet_test-y_test)^2/length(X_test))
sqrt(mse3_test_enet)


# compare the models
rbind(sqrt(mse2_test_lasso),sqrt(mse_test_rigde),sqrt(mse3_test_enet))

tmp_coeffs <- coef(ridge_cv, s = "lambda.min")
data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
