#Kaggle Titanic modeling prediction competition

setwd("~/Projects/kaggle/titanic")

library(randomForest)
library(dplyr)
library(rpart)
library(caret)
library(e1071)
library(modeest)

train <- read.csv("train.csv", na.strings=c(""," ", "NA (NULL)", "NA", "<NA>"))
test <- read.csv("test.csv", na.strings=c(""," ", "NA (NULL)", "NA", "<NA>"))



#str(train)
#str(test)

#apply(train, 2, function(x){sum(is.na(x))})
#apply(test, 2, function(x){sum(is.na(x))})

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

#table(pred, data.train.em$Embarked)

pred.em <- predict(rf, data.em)

alldata[c(62,830),]$Embarked <- factor("C")
rm(data)

#use a random forest to estimate the ages of the NAs

data <- select(alldata, -c(Ticket, Cabin, Name, Survived))
data.train.age <- data[!is.na(data$Age),]
data.age <- data[is.na(data$Age),]

rf <- randomForest(factor(Age)~., data.train.age, ntree=500, nodesize=2, importance=TRUE)
pred <- predict(rf, data.train.age)

# table(pred, data.train.age$Age)

pred.age <- predict(rf, data.age)

alldata[is.na(alldata$Age),]$Age <- pred.age
rm(data)

#clean up the Ticket vairable of letters and spaces

alldata$Ticket <- gsub("[a-zA-z]|[.]|[,]|[\\/]|[\\ ]", "", alldata$Ticket)

#create fam_size variable

alldata$fam_size <- alldata$Parch + alldata$SibSp + 1

#create Title variable

alldata$Title <- gsub("(.*,.)|(\\..*)", "", alldata$Name)

#create family name variable

alldata$fam_name <- gsub(",.*", "", alldata$Name)

alldata <- select(alldata, -Cabin)

alldata$nuc.fam.size <- 0
alldata$married <- 0
alldata$nochild <- 0
alldata$numchild <- 0
alldata$single <- 0
alldata$parent <- 0
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

alldata[alldata$SibSp == 0 & alldata$Age > 18 & alldata$fam_size == 1,]$single <- 1
alldata[alldata$single == 1 & alldata$Parch == 0,]$fam_name <- paste0(alldata[alldata$single == 1 & alldata$Parch == 0,]$fam_name, "(I)")
alldata[alldata$Parch == 0,]$nochild <- 1
alldata[alldata$Parch > 0 & alldata$SibSp == 0 & alldata$Sex == "male" & alldata$Age > 18,]$singdad <- 1
alldata[alldata$Parch > 0 & alldata$SibSp == 0 & alldata$Sex == "female" & alldata$Age > 18,]$singmom <- 1
alldata[alldata$singdad == 1 | alldata$singmom == 1,]$parent <- 1
alldata[alldata$Title == "Mrs" & alldata$Age > 14,]$married <- 1

#save alldata
survived <- alldata$Survived

alldata <- select(alldata, -Survived)



#split families

for (i in unique(alldata$fam_name)) {

    family <- arrange(alldata[alldata$fam_name == i,], desc(Age))
    
    
    
    if (all(family$fam_size == nrow(family))) { next } 
    else if (all(family$single == 1)) { next } 
    else if (all(family$fam_size == 1)) { next } 
    else if (nrow(family) == 1) { next } 
    else if (all(family$Ticket == family$Ticket[1])) { next } 
    else {
        ticket1 <- mlv(as.numeric(family$Ticket))[[1]]
        
        if (ticket1 %% 1 > 0) { ticket1 <- family[1,]$Ticket }
        
        family1 <- family[family$Ticket == ticket1 ,]
        rename <- paste0(family1$fam_name[1], "()")
        family1$fam_name <- rename
       
        for (j in 1:nrow(family1)) {
            id <- family1[j,]$PassengerId
            family[family$PassengerId == id,] <- family1[j,]
        }
        
        family2 <- family[!(family$Ticket == ticket1) ,]         
        family2$fam_name <- paste0(family2$fam_name[1], "(i)")
        family2$nuc.fam.size <- nrow(family2)
        for (j in 1:nrow(family2)) {
            id <- family2[j,]$PassengerId
            family[family$PassengerId == id,] <- family2[j,]
        }
    }
    
    for (z in 1:nrow(family)) {
        id <- family[z,]$PassengerId
        alldata[alldata$PassengerId == id,] <- family[z,]
    }
}