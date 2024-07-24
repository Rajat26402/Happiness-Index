# Loading Data
train<-read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2016_new.csv")
test<-read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2017_new.csv")

# Viewing data
head(train,5)
head(test,5)

# Dividing columns into y and x of training data.
y<-train$Happiness.Score
x<-data.matrix(train[,c(6,7,8,9,10,11,12,13,14)])

# Loading Library
library(glmnet)

# Modeling using training.
cv_model<-cv.glmnet(x,y,alpha=1)
best_lambda<-cv_model$lambda.min
best_lambda

# Ploting the Model
plot(cv_model)

#Finding Model
best_model<-glmnet(x,y,alpha = 1,lambda = best_lambda)
coef(best_model)

# Predicting data for testing data
Y<-test$Happiness.Score
X<-data.matrix(test[,c(5,6,7,8,9,10,11,12,13)])
y_predicted<-predict(best_model,s=best_lambda,newx = X)

# Finding R2
sst<-sum((Y-mean(Y))^2)
sse<-sum((y_predicted-Y)^2)
rsq<-1-sse/sst
rsq

# Finding Accuracy
acc=100-(sum(y_predicted-Y)/length(y_predicted))*100
acc

