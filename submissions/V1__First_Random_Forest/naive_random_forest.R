# Clean up workspace
rm(list=ls())

setwd("~/Documents/Data Science 2014/kaggle/criteo_challenge")
train <- read.csv("/data/criteo/train_01perc.csv")
test <- read.csv("/data/criteo/full_test.csv")

########### Clean Missing Data ###########

summary(train$I1) # Too many NA's, dont use it
summary(train$I2) # OK
summary(train$I3)
train$I3[is.na(train$I3)] <- median(train$I3, na.rm=TRUE)

summary(train$I4) # Too many NA's, dont use it
summary(train)
train$I5[is.na(train$I5)] <- median(train$I5, na.rm=TRUE)

summary(train$I6) # Too many NA's, dont use it
summary(train$I7)
train$I7[is.na(train$I7)] <- median(train$I7, na.rm=TRUE)

summary(train$I8)
train$I8[is.na(train$I8)] <- median(train$I8, na.rm=TRUE)

summary(train$I9)
train$I9[is.na(train$I9)] <- median(train$I9, na.rm=TRUE)

summary(train$I10) # Too many NA's, dont use it
summary(train$I11)
train$I11[is.na(train$I11)] <- median(train$I11, na.rm=TRUE)

summary(train$I12) # Too many NA's, dont use it
summary(train$I13) # Too many NA's, dont use it

########### Combine data sets and fix factor levels ###########
test$Label <- 0
combi <- rbind(train, test)

combi$C6 <- factor(combi$C6)
combi$C9 <- factor(combi$C9)
combi$C14 <- factor(combi$C14)
combi$C17 <- factor(combi$C17)
combi$C20 <- factor(combi$C20)
combi$C22 <- factor(combi$C22)
combi$C23 <- factor(combi$C23)

train <- combi[1:458406,]
test <- combi[458407:6500541,]

########### Create Random Forest ###########
rxForest <- rxDForest(Label ~ I2 + I3 + I5 + I7 + I8 + I9 + I11 + 
                        C6 + C9 + C14 + C17 + C20 + C22 + C23
                      , data=train, importance=TRUE, nTree=2000)

########### Make Submission File ###########
Prediction <- rxPredict(rxForest, data=test, type="prob", overwrite=TRUE)

# Backup predictions into binary file
rxDataStep(inData=Prediction, outFile="PredictionBkp", overwrite=TRUE)

# Create CSV file with probabilities
submit <- data.frame(Id=test$Id, Predicted=Prediction$Label_Pred)
write.csv(submit, file="submit.csv", row.names=FALSE)