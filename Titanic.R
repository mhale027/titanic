#Kaggle Titanic modeling prediction competition

library(randomForest)
library(dplyr)
library(rpart)

train <- read.csv("train.csv", na.strings=c(""," ", "NA (NULL)", "NA"))
test <- read.csv("test.csv", na.strings=c(""," ", "NA (NULL)", "NA"))



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


