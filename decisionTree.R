# Clean up workspace
rm(list=ls())

setwd("~/Documents/Data Science 2014/kaggle/criteo")
train <- read.csv("~/Documents/Data Science 2014/kaggle/criteo/samples/train_sample_45K.csv")
test <- read.csv("~/Documents/Data Science 2014/kaggle/criteo/samples/train_sample_4K.csv")

########### First Decision Tree model ###########
combi <- rbind(train, test)

# Cast back to factor data type
combi$C1 <- factor(combi$C1)

# Break apart train and test data sets.
# Dont manipulate the data sets in isolation, once the factor levels wont be the 
# same and it will cause errors when training our model.
train <- combi[1:45840,]
test <- combi[45841:50424,]

library(rpart)
fit <- rpart(Label ~ ., data=train, method="class")
Prediction <- predict(fit, test, type = "class")

library(caret)
confusionMatrix(Prediction, test$Label)
