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

#use a random forest to estimate the ages of the NAs

data <- select(alldata, -c(Ticket, Cabin, Name, Survived))
data.train.age <- data[!is.na(data$Age),]
data.age <- data[is.na(data$Age),]

rf <- randomForest(factor(Age)~., data.train.age, ntree=500, nodesize=2, importance=TRUE)
pred <- predict(rf, data.train.age)

table(pred, data.train.age$Age)

pred.age <- predict(rf, data.age)

alldata[is.na(alldata$Age),]$Age <- pred.age
rm(data)

#clean up the Ticket vairable of letters and spaces

alldata$Ticket <- gsub("[a-zA-z]|[.]|[,]|[\\/]|[\\ ]", "", alldata$Ticket)

#remove Cabin variable since it is way more than half empty

alldata <- select(alldata, -Cabin)

#create fam_size variable

alldata$fam_size <- alldata$Parch + alldata$SibSp + 1

#create Title variable

alldata$Title <- gsub("(.*,.)|(\\..*)", "", alldata$Name)

#create family name variable

alldata$fam_name <- gsub(",.*", "", alldata$Name)

alldata$nuclear <- 0
alldata$married <- 0
alldata$nochild <- 0
alldata$numchild <- 0
alldata$single <- 0
alldata$singmom <- 0
alldata$singdad <- 0
alldata$child1 <- 0
alldata$child2 <- 0
alldata$child3 <- 0
alldata$child4 <- 0
alldata$child5 <- 0
alldata$child6 <- 0
alldata$child7 <- 0
alldata$child8 <- 0
alldata$child9 <- 0


for (i in unique(alldata$fam_name)) {
    family <- alldata[alldata$fam_name == i,]
    
    if (all(family$Parch ==0 ) & all(family$SibSp == 0)) { 
        family$nochild <- 1
        family$single <- 1
    }
    
    if (all(family$Parch ==0 ) & all(family$SibSp == 1)) { 
        family$nochild <- 1
        family$married <- 1
    }
    
    
    for (j in 1:nrow(family)){
        if (family[j,]$SibSp == 0 & family[j,]$Parch > 0)
        
        
        if (with(family, fam_size[j] == Parch[j] + SibSp[j] + 1)){
        family[j,]$nuclear <- 1
            if (family[j,]$SibSp + 1 == family[family[j,]$SibSp==1,]$Parch) {
                
            }
        } else { 
        family[j,]$nuclear <- 0
        family[j,]$fam_name <- paste0(family[j,]$fam_name, "I")
    }
        if (nrow(family) == family[j,]$fam_size & nrow(family[family[j,]$SibSp==1,])==2) {
            if (family[j,]$SibSp==1 & family[l,]$Age > 18) { family[j,]$married <- 1 }

        }
    }
}
    









#create predicitons for age NAs






#
#create new columns for siblings, son&daughter:1st:9th, parent, single mom, single dad, grandmother, grandfather
#
#
#
#


