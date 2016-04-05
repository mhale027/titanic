#Kaggle Titanic modeling prediction competition

setwd("~/Projects/kaggle/titanic")

library(randomForest)
library(dplyr)
library(rpart)
library(caret)
library(e1071)

train <- read.csv("train.csv", na.strings=c(""," ", "NA (NULL)", "NA", "<NA>"))
test <- read.csv("test.csv", na.strings=c(""," ", "NA (NULL)", "NA", "<NA>"))



str(train)
str(test)

apply(train, 2, function(x){sum(is.na(x))})
apply(test, 2, function(x){sum(is.na(x))})

#create whole dataset and reset factor variables

alldata <- bind_rows(train, test)

#clean up Fare NA by grabbing the mean of the fares for 
#four digit tickets, starting with a 3, and where Pclass 
#is the same. 

mean.sample <- alldata[grep("^....$",alldata$Ticket),]
alldata[is.na(alldata$Fare),]$Fare <- mean(mean.sample[mean.sample$Pclass==3,]$Fare, na.rm=TRUE)

#clean up NAs in Embarked by training a random forest on the subset which is not NA

data <- select(alldata, -c(Ticket, Cabin, Name, Survived, Age))
data.train.em <- data[!is.na(data$Embarked),]
data.em <- data[c(62,830),]

rf <- randomForest(factor(Embarked)~., data.train.em, ntree=500, nodesize=2, importance=TRUE)
pred <- predict(rf, data.train.em)

table(pred, data.train.em$Embarked)

pred.em <- predict(rf, data.em)

alldata[c(62,830),]$Embarked <- factor("C")
rm(data)




#create fam_size variable

alldata$fam_size <- alldata$Parch + alldata$SibSp + 1

#create Title variable

alldata$Title <- gsub("(.*,.)|(\\..*)", "", alldata$Name)

#create family name variable

alldata$fam_name <- gsub(",.*", "", alldata$Name)

#create single parent variables

for (i in nrow(alldata)) {
    
}





#remove variables

data <- select(alldata, -c(Name, Cabin, PassengerId, Ticket))


#create predicitons for age NAs






#
#create new columns for siblings, son&daughter:1st:9th, parent, single mom, single dad, grandmother, grandfather
#
#
#
#


