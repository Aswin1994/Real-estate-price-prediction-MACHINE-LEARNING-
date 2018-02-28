library(mlbench)
setwd("G:/")
Adm<-read.csv("refined.csv",header=T,sep = ",")
names(Adm)
str(Adm)
summary(Adm)
lm.fit <- lm(median_house_value ~ ., data = Adm)
lm.predict <- predict(lm.fit)
Adm$median_house_value
plot(Adm$median_house_value, lm.predict)
plot(Adm$median_house_value, lm.predict,
     main = "linear regression predictions vs actual",
     xlab = "actual")
