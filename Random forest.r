
library(randomForest)
library(nlme)
setwd("G:/")
Adm<-read.csv("refined.csv",header=T,sep = ",")
names(Adm)
str(Adm)
#Adm$housing_median_age
#Adm<-Adm$ï..housing_median_age
#Adm1
summary(Adm)
######med 10, house ,value 400000,popu 3000,totbed 2000,rooms 10000,  
hist(Adm$ï..housing_median_age)
hist(Adm$population)
hist(Adm$median_income)


Admdata<-subset(Adm,households<2000 & median_house_value<400000
                & population<3000 & total_bedrooms<2000
                &total_rooms<10000)
summary(Admdata)

###############################################################

## checking assumption 
head(Adm)
pairs(Admdata[5:7])


##
set.seed(12)
str(Admdata)
dim(Admdata)
apply(Admdata,2,function(x) length(unique(x)))
hist(Admdata$total_rooms)
cols <-c("ocean_proximity")

for(i in cols) {
  Admdata[,i]=as.factor(Admdata[,i])
}
ram<-subset(Admdata[1:8])
str(ram)
#str(Adm)
summary(ram)
summary(Admdata)
library(caTools)

smp_size <- floor(0.75 * nrow(ram))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ram)), size = smp_size)

trainAdm <- ram[train_ind, ]
testAdm <- ram[-train_ind, ]

modelRandom<-randomForest(median_house_value~.,data = trainAdm,mtry=3,ntree=200,na.action =na.exclude)
plot(modelRandom)

#checking important variables
importance(modelRandom)
varImpPlot(modelRandom)

Pred<-predict(modelRandom,testAdm,type = 'response')
Pred

# checking the percentage(not so useful)

#sum(diag(t))/sum(t)

##library(pROC)
#PredictionProb<-as.numeric(predict(modelRandom, testAdm, type = 'response'))
#auc<-auc(testAdm$house_value,PredictionProb)
#plot(multiclass.roc(testAdm$house_value,PredictionProb))

##
#plotting 

sqrt( sum( (Pred - testAdm$median_house_value)^2 , na.rm = TRUE ) / nrow(testAdm) )
plot(Pred,testAdm$median_house_value)
#, xlab = "Predicted Price", ylab = "Actual Price", pch = 3)
cor(Pred,testAdm$median_house_value)^2
difference <-Pred - testAdm$median_house_value
mse <-(mean(difference)/ nrow(testAdm))
rmse <-(mse)^2
rmse
summary(modelRandom)

