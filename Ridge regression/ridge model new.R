#ridge model new - after imputing NA values with median values

# load packages
library(data.table) # used for reading and manipulation of data
library(dplyr)	 # used for data manipulation and joining
library(glmnet)	 # used for regression
library(ggplot2) # used for ploting
library(caret)	 # used for modeling
library(xgboost) # used for building XGBoost model
library(e1071)	 # used for skewness
library(cowplot) # used for combining multiple plots

# Loading datasets
train = read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2016_new.csv")
test = read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2017_new.csv")


# Model Building : Ridge Regression
set.seed(123)
control = trainControl(method ="cv", number = 5)
Grid_reg = expand.grid(alpha = 0, lambda = seq(0.001, 0.1,
                                               by = 0.0002))
x = data.matrix(train[ , c(5,6,7,8,9,10,11,12,13)])
y = train$Happiness.Score

model=glmnet(x,y,alpha=0)

cv_model=cv.glmnet(x,y, alpha=0)
best_lambda=cv_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

# Training Ridge Regression model
Ridge_model = train(x,
                    y,
                    method = "glmnet",
                    trControl = control,
                    tuneGrid = Grid_reg
)
Ridge_model

# mean validation score
mean(Ridge_model$resample$RMSE)
y_predicted <- predict(model, s = best_lambda, newx = x)
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq

#Accuracy
error=(sum(abs(y_predicted-y)))/sum(y)*100
accuracy=100-error
print(accuracy)

# Plot
plot(Ridge_model, main="Ridge Regression")

