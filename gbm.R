library(Metrics)  ##load evaluation package

# Clean up workspace
rm(list=ls())

setwd("~/criteo_challenge")
train <- read.csv("~/criteo_challenge/samples/train_sample_45K.csv")
test <- read.csv("~/criteo_challenge/samples/train_sample_4K.csv")

## check levels; gbm has a limit of 1024 per factor;
dim(train); dim(test)
sapply(train[,16:41],function(x) length(levels(x)))
# 16    17    18    19    20    21    22    23    24   25    26    27    28    29    30    31    32    33    34    35    36    37    38    39    40    41
# C1    C2    C3    C4    C5    C6    C7    C8    C9   C10   C11   C12   C13   C14   C15   C16   C17   C18   C19   C20   C21   C22   C23   C24   C25   C26 
# 385   495 23295 14128   106    13  6420   183     3  7961  3416 21615  2635    25  4060 18515     9  2090  1034     4 20315    11    14  7519    43  5808 

c_excluded <- c(18, 19, 22, 25, 26, 27, 28, 30, 31, 33, 34, 36, 39, 41)

xTrain <- train[,-c_excluded]
xTest <- test[,-c_excluded]

xTrain <- xTrain[,3:ncol(xTrain)]
xTest <- xTest[,3:ncol(xTest)]

yTrain <- train[,2]
yTest <- test[,2]

set.seed(415) # to ensure reproducable results

library(gbm)
GBM_NTREES = 4000
GBM_SHRINKAGE = 0.005
GBM_DEPTH = 20
GBM_MINOBS = 5

##	Fit model
g<-gbm.fit(x=xTrain, y=yTrain, distribution = "bernoulli",n.trees = GBM_NTREES,shrinkage = GBM_SHRINKAGE,
           interaction.depth = GBM_DEPTH, n.minobsinnode = GBM_MINOBS,
           train.fraction = 1.0,
           bag.fraction = 0.5,
           verbose = TRUE,
           keep.data = FALSE)

##  Set up parameters to pass in; there are many more hyper-parameters available, but these are the most common to control
GBM_NTREES = 400
##	400 trees in the model; can scale back later for predictions, if desired or overfitting is suspected
GBM_SHRINKAGE = 0.05
##	shrinkage is a regularization parameter dictating how fast/aggressive the algorithm moves across the loss gradient
##	0.05 is somewhat aggressive; default is 0.001, values below 0.1 tend to produce good results
##		decreasing shrinkage generally improves results, but requires more trees, so the two should be adjusted in tandem
GBM_DEPTH = 4
##	depth 4 means each tree will evaluate four decisions; 
##		will always yield [3*depth + 1] nodes and [2*depth + 1] terminal nodes (depth 4 = 9) 
##		because each decision yields 3 nodes, but each decision will come from a prior node
GBM_MINOBS = 30
##	regularization parameter to dictate how many observations must be present to yield a terminal node
##	higher number means more conservative fit; 30 is fairly high, but good for exploratory fits; default is 10

g<-gbm.fit(x=xTrain,y=yTrain,distribution = "bernoulli",n.trees = GBM_NTREES,shrinkage = GBM_SHRINKAGE,
           interaction.depth = GBM_DEPTH,n.minobsinnode = GBM_MINOB)

## gbm fit; provide all remaining independent variables in xTrain; provide targets as yTrain;
##	gaussian distribution will optimize squared loss; 

## get predictions; first on train set, then on unseen test data
tP1 <- predict.gbm(object = g,newdata = xTrain,GBM_NTREES)
hP1 <- predict.gbm(object = g,newdata = xTest,GBM_NTREES)

## compare model performance to default (overall mean)
summary(hP1)
rmse(yTrain,tP1)			##  9452.742 on data used for training
rmse(yTest,hP1)				##  9740.559 ~3% drop on unseen data; does not seem to be overfit
rmse(yTest,mean(yTrain))	## 24481.08  overall mean; cut error rate (from perfection) by 60%

## look at variables
summary(g)	## summary will plot and then show the relative influence of each variable to the entire GBM model (all trees)


## Investigate actual GBM model
pretty.gbm.tree(g,1)	##	show underlying model for the first decision tree
summary(xTrain[,6])	##	underlying model showed variable 5 to be first point in tree (5 with 0 index = 6th column)
g$initF					##	view what is effectively the "y intercept"
mean(yTrain)			##	equivalence shows gaussian y intercept is the mean
t(g$c.splits[1][[1]]) 	##	show whether each factor level should go left or right
plot(g,6)				##	plot I6, the variable with the highest rel.inf
plot(g,11)				##	plot I11, continuous variable with 2nd highest rel.inf
interact.gbm(g,xTrain,c(6,11))
##	compute H statistic to show interaction; integrates 

library(caret)
# Creating CSV for Kaggle Submission
Prediction <- vector()
Prediction = ifelse(hP1 > 0.5, 1, 0)

confusionMatrix(Prediction, test$Label)

train2 <- train[,-c_excluded]
test2 <- test[,-c_excluded]

train2 <- train2[,2:ncol(train2)]
test2 <- test2[,2:ncol(test2)]

genmod<-gbm(train2$Label~.
                         ,data=train2[,-c(1)] ## registered,casual,count columns
                         ,var.monotone=NULL # which vars go up or down with target
                         ,distribution="gaussian"
                         ,n.trees=1200
                         ,shrinkage=0.05
                         ,interaction.depth=3
                         ,bag.fraction = 0.5
                         ,train.fraction = 1
                         ,n.minobsinnode = 10
                         ,cv.folds = 10
                         ,keep.data=TRUE
                         ,verbose=TRUE)

best.iter <- gbm.perf(genmod,method="cv") ##the best iteration number
print(pretty.gbm.tree(genmod, best.iter))
summary(genmod, n.trees=best.iter)

pred.test <- predict(genmod, test2[,-c(1)], best.iter, type="response")
summary(pred.test)

NewPrediction <- vector()
NewPrediction = ifelse(pred.test > 0.5, 1, 0)

confusionMatrix(NewPrediction, test$Label)

set.seed(245)

# 1      2  3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26
# Label  I1	I2	I3	I4	I5	I6	I7	I8	I9	I10	I11	I12	I13	C1	C2	C5	C6	C8	C9	C14	C17	C20	C22	C23	C25

small_features <- c(2,4,6,7,12,14,15,16,21,22,25)

train2[,small_features]

combi <- rbind(train2, test2)
combi$C1 <- factor(combi$C1)
combi$C2 <- factor(combi$C2)
combi$C14 <- factor(combi$C14)
combi$C17 <- factor(combi$C17)
combi$C23 <- factor(combi$C23)


train2 <- combi[1:45840,]
test2 <- combi[45841:50424,]

genmod_small<-gbm.fit(x=train2[,small_features], y=train2[,1], distribution = "bernoulli",n.trees = 200,shrinkage = 0.05,
           interaction.depth = 3, n.minobsinnode = 10,
           train.fraction = 1.0,
           bag.fraction = 0.5,
           verbose = TRUE,
           keep.data = TRUE)

best.iter_sml <- gbm.perf(genmod_small,method="cv") ##the best iteration number
print(pretty.gbm.tree(genmod_small, best.iter_sml))
summary(genmod_small, n.trees=best.iter_sml)

pred.test_sml <- predict(genmod_small, test2[,small_features], 200, type="response")
summary(pred.test_sml)

PredictionSml <- vector()
PredictionSml = ifelse(pred.test_sml > 0.5, 1, 0)

confusionMatrix(PredictionSml, test2$Label)

#------------------------

genmod_last<-gbm(Label ~ I1 + I3 + I5 + I6 + I11 + I13 + C1 + C2 + C14 + C17 + C23
                  ,data=train2[,c(1,2,4,6,7,12,14,15,16,21,22,25)] ## registered,casual,count columns
                  ,var.monotone=NULL # which vars go up or down with target
#                  ,distribution="gaussian"
                  ,distribution="bernoulli"
                  ,n.trees=2000
                  ,shrinkage=0.05
                  ,interaction.depth=3
                  ,bag.fraction = 0.5
                  ,train.fraction = 1
                  ,n.minobsinnode = 10
                  ,cv.folds = 10
                  ,keep.data=TRUE
                  ,verbose=TRUE)

best.iter_last <- gbm.perf(genmod_last,method="cv") ##the best iteration number
print(pretty.gbm.tree(genmod_last, best.iter_last))
summary(genmod_last, n.trees=best.iter_last)

pred.test_last <- predict(genmod_last, test2[,c(1,2,4,6,7,12,14,15,16,21,22,25)], 2000, type="response")
summary(pred.test_last)

LastPrediction <- vector()
LastPrediction = ifelse(pred.test_last > 0.5, 1, 0)

confusionMatrix(LastPrediction, test2$Label)
