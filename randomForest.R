# Clean up workspace
rm(list=ls())
setwd("~/Documents/Data Science 2014/kaggle/criteo")

train = read.csv("~/Documents/Data Science 2014/kaggle/criteo/samples/train_sample_45K.csv", encoding = "UTF-8")
test = read.csv("~/Documents/Data Science 2014/kaggle/criteo/samples/train_sample_4K.csv", encoding = "UTF-8")

summary(train$Label)

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

combi <- rbind(train, test)
combi$C6 <- factor(combi$C6)
combi$C9 <- factor(combi$C9)
combi$C14 <- factor(combi$C14)
combi$C17 <- factor(combi$C17)
combi$C20 <- factor(combi$C20)
combi$C22 <- factor(combi$C22)
combi$C23 <- factor(combi$C23)

train <- combi[1:45840,]
test <- combi[45841:50424,]

library(randomForest)
set.seed(415)

fit <- randomForest(as.factor(Label) ~ I2 + I3 + I5 + I7 + I8 + I9 + I11 + 
                      C6 + C9 + C14 + C17 + C20 + C22 + C23
                      , data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)

library(caret)

Prediction <- predict(fit, test)
confusionMatrix(Prediction, test$Label)

# Confusion Matrix and Statistics
#
#          Reference
#Prediction    0    1
#         0 2385  682
#         1  136  205
#                                          
#               Accuracy : 0.76            
#                 95% CI : (0.7453, 0.7742)
#    No Information Rate : 0.7397          
#    P-Value [Acc > NIR] : 0.003527        
#                                          
#                  Kappa : 0.2213          
# Mcnemar's Test P-Value : < 2.2e-16       
#                                          
#            Sensitivity : 0.9461          
#            Specificity : 0.2311          
#         Pos Pred Value : 0.7776          
#         Neg Pred Value : 0.6012          
#             Prevalence : 0.7397          
#         Detection Rate : 0.6998          
#   Detection Prevalence : 0.8999          
#      Balanced Accuracy : 0.5886          
#                                          
#       'Positive' Class : 0
