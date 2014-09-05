# Clean up workspace
rm(list=ls())

setwd("~/criteo_challenge")
train <- read.csv("/data/criteo/train_01perc.csv")
test <- read.csv("/data/criteo/full_test.csv")

# to ensure reproducable results
set.seed(415)

## check levels; gbm has a limit of 1024 per factor;
dim(train); dim(test)
sapply(train[,16:41],function(x) length(levels(x)))

# 16    17    18    19    20    21    22    23    24   25    26    27    28    29    30    31    32    33    34    35    36    37    38    39    40    41
# C1    C2    C3    C4    C5    C6    C7    C8    C9   C10   C11   C12   C13   C14   C15   C16   C17   C18   C19   C20   C21   C22  C23   C24   C25   C26 
# 1002  527 166431  72578 230   14  10042   475   3  22493   4459 144344 3027  26   7739 114451  10   3475   1706   4   131324 14   15   28337   61  21597

# In the future we should create bins for those features to reduce numerosity
c_excluded <- c(18, 19, 22, 25, 26, 27, 28, 30, 31, 33, 34, 36, 39, 41)
train <- train[,-c_excluded]

sapply(test[,15:40],function(x) length(levels(x)))
# 15    16    17    18    19    20    21    22    23   24    25    26    27    28    29    30    31    32    33    34    35    36    37    38    39    40
# C1    C2    C3    C4    C5    C6    C7    C8    C9   C10   C11   C12   C13   C14   C15   C16   C17   C18   C19   C20   C21   C22  C23   C24   C25   C26
# 1351 554 1654604  496952 280  23   11815  596   3   55575  4852 1426969 3129 27   12147 1029668  10  4866  2077  4   1254819 18   14  107746  90   69136

c_excluded <- c(17, 18, 21, 24, 25, 26, 27, 29, 30, 32, 33, 35, 38, 40)
test <- test[,-c_excluded]

## Fit a GBM model to search the best features
library(gbm)
GBM_NTREES = 3000

feature_model <- gbm(Label~. ,data=train[,-c(1)]
                     ,var.monotone=NULL # which vars go up or down with target
                     ,distribution="bernoulli"
                     ,n.trees=GBM_NTREES
                     ,shrinkage=0.05
                     ,interaction.depth=3
                     ,bag.fraction = 0.5
                     ,train.fraction = 1
                     ,n.minobsinnode = 10
                     ,cv.folds = 10
                     ,keep.data=TRUE
                     ,verbose=TRUE)

best.iter <- gbm.perf(feature_model, method="cv") ##the best iteration number
print(pretty.gbm.tree(feature_model, best.iter))
summary(feature_model, n.trees=best.iter)

# 1      2  3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26
# Label  I1  I2  I3	I4	I5	I6	I7	I8	I9	I10	I11	I12	I13	C1	C2	C5	C6	C8	C9	C14	C17	C20	C22	C23	C25

# var     rel.inf
# C2   C2 46.27364362 - 16
# I6   I6 30.77519350 - 7
# I11 I11  6.95886203 - 12
# I13 I13  4.31791464 - 14
# C1   C1  4.15908372 - 15
# I5   I5  2.28242117 - 6
# C14 C14  1.97579587 - 21
# C17 C17  1.23798402 - 22
# C23 C23  0.93071655 - 25
# I3   I3  0.88941680 - 4
# C8   C8  0.11059903 - 19
# I7   I7  0.08836905 - 8

top_features <- c(2,16,7,12,14,15,6,21,22,25,4,19,8)

## Build final GBM model
gbm_model <- gbm(Label ~ . 
                 ,data=train[,top_features]
                 ,var.monotone=NULL # which vars go up or down with target
                 ,distribution="bernoulli"
                 ,n.trees=GBM_NTREES
                 ,shrinkage=0.05
                 ,interaction.depth=3
                 ,bag.fraction = 0.5
                 ,train.fraction = 1
                 ,n.minobsinnode = 10
                 ,cv.folds = 10
                 ,keep.data=TRUE
                 ,verbose=TRUE)

## Predict probabilities
Prediction <- predict(gbm_model, test, GBM_NTREES, type="response")
summary(Prediction)

# Create submission file
submit <- data.frame(Id=test$Id, Predicted=Prediction)
write.csv(submit, file="submit.csv", row.names=FALSE)
