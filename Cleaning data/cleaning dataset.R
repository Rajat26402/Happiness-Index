#Imputing missing values using median for 2016 year

data1= read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2016_withNA.csv")

data2<- subset(data1,select=c(Country,Region,Happiness.Rank))
data1=subset(data1,select=-c(Country,Region,Happiness.Rank))
all_column_median <- apply(data1,2,median,na.rm=TRUE)

for(i in colnames(data1))
  data1[,i][is.na(data1[,i])]<- all_column_median[i]
data1
data1<-cbind(data2,data1)
write.csv(data1, file = "2016_new.csv")


#Imputing missing values using median for 2017 year

data3= read.csv("D:/VIT Chennai/SEM 6/Essentials of Data Analytics/Codes/Codes/Lasso Regression/2017_withNA.csv")
data4 <- subset(data3,select=c(Country,Happiness.Rank))
data3=subset(data3,select=-c(Country,Happiness.Rank))
all_column_median <- apply(data3,2,median,na.rm=TRUE)

for(i in colnames(data3))
  data3[,i][is.na(data3[,i])]<- all_column_median[i]
data3

data3<-cbind(data4,data3)
write.csv(data3, file = "2017_new.csv")
