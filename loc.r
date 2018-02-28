library("ggmap")
dir()
setwd("G:/")
#House<-read.csv("adm.csv",header=T,sep = ",")
#students<-read.table(file.choose("Adm dat set .txt"),header=TRUE,sep="\t")
House<-read.csv("adm.csv",header = T)
str(House)

library(ggmap)
result <- do.call(rbind,
                  lapply(1:nrow(House),
                         function(i)revgeocode(as.numeric(House[i,3:2]))))
data1 <- cbind(House,result)
data1