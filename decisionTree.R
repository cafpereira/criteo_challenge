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
# 1) root 45840 11764 0 (0.74336824 0.25663176)  
#   2) C3=,0002ea7c,0006017d,000cdc14,000ee79d,00148a74,0019ab81,00233185,002f81c3,... 21126   271 0 (0.98717220 0.01282780) *
#     5) C3=,00148a74,0033392a,010a5a81,... 15644  3880 0 (0.75198159 0.24801841)  
#      10) C10=000e2f4b,00223894,004b452f,0057f985,0065486b,00809996,... 13246  2208 0 (0.83330817 0.16669183)  
#        20) C7=0008893e,00239ba4,0026c72e,003baf94,004536d7,...,0 2398   726 1 (0.30275229 0.69724771)  
#        22) C7=00ee719f,019bb335,01ad6a22,01c31e6c,01fef71a,... 922   362 0 (0.60737527 0.39262473)  
#          44) C15=039402bf,089dd1eb,0af7c64c,0b2f72c4,0c67c4ca,... 481    52 0 (0.89189189 0.10810811) *
#          45) C15=022c81dc,031c4708,05619ef0,05f2a790,06373944,075f843b,... 441   131 1 (0.29705215 0.70294785) *
#        23) C7=00239ba4,0054a97c,00c46cd1,00ed31f1,... 1476   166 1 (0.11246612 0.88753388) *
#   3) C3=0017c534,00186a9a,001cdd84,0021f343,00285b26,0031fd65,... 9070  1457 1 (0.16063947 0.83936053)  
#     6) C3=00186a9a,00351e59,017bf871,01d27782,01de8a33,0253bbf5,... 3187  1442 1 (0.45246313 0.54753687)  
#      12) C7=0008893e,00583f05,00ed31f1,014a5bde,01b4b465,... 969    90 0 (0.90712074 0.09287926) *
#      13) C7=00239ba4,002fdf0c,00cc0b0f,0163e10f,01c31e6c,... 2218   563 1 (0.25383228 0.74616772)  
#        26) C10=00f2b452,012bac1e,015ac893,015dc527,017de947,... 477   144 0 (0.69811321 0.30188679) *
#        27) C10=000e2f4b,0042ccac,0065486b,02bf81ed,02dfebd5,... 1741   230 1 (0.13210798 0.86789202) *
#     7) C3=0017c534,001cdd84,0021f343,00285b26,0031fd65,... 5883    15 1 (0.00254972 0.99745028) *


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