# Graph - Accuracy and R2 between each model
# With NA (Omitting NA)	
# Without NA (Imputing NA)

#install.packages("ggplot2")
library(ggplot2)

accuracywithNA <- c(93.94,96.14,98.58,99.99)
accuracywithoutNA <- c(93.20,97.58,99.43,98.07)
model <- c("PLS","Lasso","Ridge","MLR")

# creating plot using the above data
ggplot(acc1, aes(model, accuracywithNA)) +
  geom_bar(stat="identity",fill="yellow") +
  labs(title="Accuracy between each model with NA")

ggplot(acc2, aes(model, accuracywithoutNA)) +
  geom_bar(stat="identity",fill="green") +
  labs(title="Accuracy between each model without NA")

#R2 Plot
rwithNA <- c(0.99,0.99,0.99,0.99)
rwithoutNA <- c(0.97,0.97,0.98,0.97)
model <- c("PLS","Lasso","Ridge","MLR")

# creating plot using the above data
ggplot(acc1, aes(model, rwithNA)) +
  geom_bar(stat="identity",fill="yellow") +
  labs(title="R2 between each model with NA")

ggplot(acc2, aes(model, rwithoutNA)) +
  geom_bar(stat="identity",fill="green") +
  labs(title="R2 between each model without NA")