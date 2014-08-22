# Clean up workspace
rm(list=ls())

setwd("~/Documents/Data Science 2014/kaggle/criteo_challenge")
train <- read.csv("~/Documents/Data Science 2014/kaggle/criteo_challenge/samples/train_sample_45K.csv")
test <- read.csv("~/Documents/Data Science 2014/kaggle/criteo_challenge/samples/train_sample_4K.csv")

########### First Decision Tree ###########
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

# Confusion Matrix and Statistics
#
#          Reference
# Prediction    0    1
#          0 2927  914
#          1  426  317
#                                           
#                Accuracy : 0.7077          
#                  95% CI : (0.6943, 0.7208)
#     No Information Rate : 0.7315          
#     P-Value [Acc > NIR] : 0.9999          
#                                           
#                   Kappa : 0.1492          
#  Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.8729          
#             Specificity : 0.2575          
#          Pos Pred Value : 0.7620          
#          Neg Pred Value : 0.4266          
#              Prevalence : 0.7315          
#          Detection Rate : 0.6385          
#    Detection Prevalence : 0.8379          
#       Balanced Accuracy : 0.5652          
#                                           
#        'Positive' Class : 0