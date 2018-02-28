# Assessing R2 shrinkage using 10-Fold Cross-Validation 

library(randomForest)
#multiple regression code -------
library(nlme)
setwd("G:/")
Adm<-read.csv("refined.csv",header=T,sep = ",")

summary(Adm)
######med 10, house ,value 400000,popu 3000,totbed 2000,rooms 10000,  
hist(Adm$ï..housing_median_age)
hist(Adm$population)
hist(Adm$median_income)


Admdata<-subset(Adm,households<2000 & house_value<400000
                & population<3000 & total_bedrooms<2000
                &total_rooms<10000)
summary(Admdata)

fit <- lm(house_value ~ ï..housing_median_age+total_rooms+total_bedrooms+population+households+median_income+ocean_proximity,data=Admdata) 
summary(fit)
cor(Admdata$total_rooms,Admdata$total_bedrooms,Admdata$households,method="pearson")
confint(fit, conf.level=0.95)
plot(fit)
plot(fit)
summary(fit)
Pred<-predict(fit,Adm,type = 'response')
Pred
