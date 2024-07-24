df=read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2016_withNA.csv")
df=subset(df,select= -c(Country,Region,Happiness.Rank,Year))
df<-na.omit(df)
colnames(df)



str(df)


model <- lm(Happiness.Score~Lower.Confidence.Interval+Upper.Confidence.Interval+Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity+Dystopia.Residual, data = df)
print(model)

require(ggplot2)
ggplot(df,aes(y=Happiness.Score,x=Trust..Government.Corruption.,color=Economy..GDP.per.Capita.))+geom_point()+stat_smooth(method="lm",se=FALSE)

ggplot(df,aes(y=Happiness.Score,x=Lower.Confidence.Interval,color=Upper.Confidence.Interval))+geom_point()+stat_smooth(method="lm",se=FALSE)
cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)


for( i in 2:length(coef(model))){
  print(coef(model)[i])
}

df=read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2017_withNA.csv")
df=subset(df,select= -c(Country,Happiness.Rank,Year))
df<-na.omit(df)
colnames(df)

X=subset(df,select = -c(Happiness.Score))
Y=subset(df,select = c(Happiness.Score))

Yp=c()
sum=0
for(i in 1:dim(X)[1]){
  sum=coef(model)[1]
  for( j in 2:length(coef(model))){
    sum=sum+coef(model)[j]*X[i,j-1]
    
  }
  
  Yp=c(Yp,sum)
}
Yp=data.frame(Happiness.Score.predict=Yp)
str(Y)
str(Yp)

Y_mean=sum(Y)/nrow(Y)
numerator=sum((Y-Yp)^2)
denominator=sum((Y-Y_mean)^2)
msr=1-numerator/denominator
print(paste("R square: ",msr))
error=(sum(abs(Yp-Y)))/sum(Y)*100
accuracy=100-error

print(paste("Accuracy: ",accuracy,"%"))
write.csv(cbind(Y,Yp),"Y_vs_YPredictedwithNA.csv")

#Preprocessed
df=read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2016_new.csv")
df=subset(df,select= -c(Country,Region,Happiness.Rank,Happiness.Rank.1,Year))
colnames(df)



str(df)


model <- lm(Happiness.Score~Lower.Confidence.Interval+Upper.Confidence.Interval+Economy..GDP.per.Capita.+Family+Health..Life.Expectancy.+Freedom+Trust..Government.Corruption.+Generosity+Dystopia.Residual, data = df)
print(model)
require(ggplot2)
ggplot(df,aes(y=Happiness.Score,x=Trust..Government.Corruption.,color=Economy..GDP.per.Capita.))+geom_point()+stat_smooth(method="lm",se=FALSE)

ggplot(df,aes(y=Happiness.Score,x=Lower.Confidence.Interval,color=Upper.Confidence.Interval))+geom_point()+stat_smooth(method="lm",se=FALSE)

cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)

for( i in 2:length(coef(model))){
  print(coef(model)[i])
}

df=read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2017_new.csv")
df=subset(df,select= -c(Country,Happiness.Rank,Happiness.Rank.1,Year))
colnames(df)

X=subset(df,select = -c(Happiness.Score))
Y=subset(df,select = c(Happiness.Score))

Yp=c()
sum=0
for(i in 1:dim(X)[1]){
  sum=coef(model)[1]
  for( j in 2:length(coef(model))){
    sum=sum+coef(model)[j]*X[i,j-1]
    
  }
  
  Yp=c(Yp,sum)
}
Yp=data.frame(Happiness.Score.predict=Yp)
str(Y)
str(Yp)
Y_mean=sum(Y)/nrow(Y)
numerator=sum((Y-Yp)^2)
denominator=sum((Y-Y_mean)^2)
r2=1-numerator/denominator
print(paste("R square: ",r2))
error=(sum(abs(Yp-Y)))/sum(Y)*100
accuracy=100-error
print(accuracy)
write.csv(cbind(Y,Yp),"Y_vs_YPredicted.csv")