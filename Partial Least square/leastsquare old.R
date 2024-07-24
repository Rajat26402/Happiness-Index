#install.packages("pls")
library(pls)
data1 <- read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2016_withNA.csv", header=TRUE, stringsAsFactors=FALSE)
data1<-na.omit(data1)
head(data1)
set.seed(1)    #fit PCR model
model <- plsr(Happiness.Score~Lower.Confidence.Interval+Upper.Confidence.Interval+Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity+Dystopia.Residual, data=data1, scale=TRUE, validation="CV")
#view summary of model fitting
summary(model)
#visualize cross-validation plots
validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")
#define training and testing sets
data2 <- read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2017_withNA.csv", header=TRUE, stringsAsFactors=FALSE)
data2<-na.omit(data2)
train <- data2[ , c("Upper.Confidence.Interval","Lower.Confidence.Interval","Economy..GDP.per.Capita." ,"Family","Health..Life.Expectancy." , "Freedom" ,"Trust..Government.Corruption.","Generosity","Dystopia.Residual")]
y_test <- data2[  , c("Happiness.Score")]
#use model to make predictions on a test set
pcr_pred <- predict(model, train, ncomp=2)
acc=100-(sum(pcr_pred-y_test)/length(pcr_pred))*100
print(paste("Accuracy=",acc,"%"))

#R2
sst<-sum((y_test-mean(y_test))^2)
sse<-sum((pcr_pred-y_test)^2)
rsq<-1-sse/sst
rsq

