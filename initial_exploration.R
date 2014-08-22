# Clean up workspace
rm(list=ls())

setwd("~/Documents/Data Science 2014/kaggle/criteo")
train <- read.csv("~/Documents/Data Science 2014/kaggle/criteo/train.csv")
test <- read.csv("~/Documents/Data Science 2014/kaggle/criteo/test.csv")

table(train$Label)

########### Always predict "no click" ###########
test$Predicted <- rep(0, 6042)

submit <- data.frame(Id = test$Id, Predicted = test$Predicted)
write.csv(submit, file = "submit.csv", row.names = FALSE)

########### Initial guess: I10  ###########
table(train$I10, train$Label)

test$Predicted <- 0
test$Predicted[train$I10 == 1] <- 1

submit <- data.frame(Id = test$Id, Predicted = test$Predicted)
write.csv(submit, file = "submit.csv", row.names = FALSE)

########### Combination of multiple features ###########
summary(train$I1)
table(train$I1, train$Label)

# Calculate F1 and F2
train$F1 <- 'I1 != one'
train$F1[train$I1 == 1] <- 'I1 is one'

train$F2 <- 'I10 != one'
train$F2[train$I10 == 1] <- 'I10 is one'

# find the number of ad clicks for multiple combinations of F1 and F2
aggregate(Label ~ F1 + F2, data=train, FUN=sum)

# total users on each subset
aggregate(Label ~ F1 + F2, data=train, FUN=length)

# Propotion of clicks by subset
aggregate(Label ~ F1 + F2, data=train, FUN=function(x) {sum(x)/length(x)})

########### Predicty by I1 and I10 ###########

test$Predicted <- 0
test$Predicted[test$I1 != 1 & test$I10 == 1] <- 1

submit <- data.frame(Id = test$Id, Predicted = test$Predicted)
write.csv(submit, file = "submit.csv", row.names = FALSE)

########### First Decision Tree model ###########
library(rpart)

fit <- rpart(Label ~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 + I12 + I12 + I3
             , data=train, method="class")

plot(fit)
text(fit)

# Better plots
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(ID = test$Id, Predicted = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

########### Feature Engineering ###########

test$Label <- NA
combi <- rbind(train, test)

# Cast back to factor data type
combi$C1 <- factor(combi$C1)

# Break apart train and test data sets.
# Dont manipulate the data sets in isolation, once the factor levels wont be the 
# same and it will cause errors when training our model.
train <- combi[1:4584,]
test <- combi[4585:10626,]

fit <- rpart(Label ~ I1 + I2 + I3 + I4 + I5 + I6 + I7 + I8 + I9 + I10 + I12 + I12 + I3 +
               C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 +
               C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 +
               C21 + C22 + C23 + C24 + C25 + C26
             , data=train, method="class")

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(Id = test$Id, Predicted = Prediction)
write.csv(submit, file = "featuresengineer.csv", row.names = FALSE)

########### Random Forests ########### <- STOPED HERE

### Clean missing data, part A: Age
summary(combi$I1)
summary(combi$C22)

# Train decision tree to model continous (ANOVA) age number from all rows with proper age value
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")

# Predict ages of passangers without age value
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi$Age) # Age distribuition is the basically the same, but no NA values now

### Clean missing data, part B: Embarked
summary(combi$Embarked)

# Majority boarded in Southampton, let’s replace blank with ‘S’
which(combi$Embarked == '')

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

### Clean missing data, part C: Fare
summary(combi$Fare)

# Only one passenger with a NA, so let’s find out which one it is 
which(is.na(combi$Fare))

# and replace it with the median fare.
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Random Forests in R can only digest factors with up to 32 levels
combi$FamilyID # 61 Levels

# Increase our cut-off to be a “Small” family from 2 to 3 people.
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

# FamilyID2 now has 22 Levels
length(levels(combi$FamilyID2))

# Random Forest package
library(randomForest)

# This makes your results reproducible next time you load the code up, 
# otherwise you can get different classifications for each run.

# The number inside isn’t important, you just need to ensure you use the 
# same seed number each time so that the same random numbers are generated 
# inside the Random Forest function.
set.seed(415)

# Training the new model
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)

# Importance of each variable in the forest
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# There’s more than one ensemble model. Let’s try a forest of conditional inference trees.
library(party)
set.seed(415)

# Conditional inference trees are able to handle factors with more levels than Random Forests can, 
# so let’s go back to out original version of FamilyID.

# We also have to manually set the number of variables to sample at each node as the default of 5 
# is pretty high for our dataset.
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "conditionalforest.csv", row.names = FALSE)
