#------------------------------------------------------------------------
# REVOLUTION ANALYTICS WEBINAR: INTRODUCTION TO R FOR DATA MINING
# February 14, 2013
# Joseph B. Rickert
# Technical Marketing Manager
#
#### BUILD A TREE MODEL WITH RPART AND EVALUATE #####
#
# Copyright: Revolution Analytics
# This script is licensed under the GPLv2 license
# http://www.gnu.org/licenses/gpl-2.0.html
#-------------------------------------------------------------------------
# This script divides the data into training, validation and testing data,
# builds two different decision trees (rpart) using the training data and
# evaluates their performance using the test data set
# An ROC curve is produced for the better model
#------------------------------------------------------------------------
library(rattle)
library(rpart)
library(ROCR)
library(caret)
# -----------------------------------------------------------------------
# Read in the data from disk
#	name <- "weather.csv"
#	path <- file.path(getwd(),name)
#	weather <- read.csv(path,header=TRUE)
#	Show weather on the IDE editor
data(weather)
head(weather)
#------------------------------------------------------------------------
# Select variables for the model
weather <- subset(weather,select=c(MinTemp:RainTomorrow))
set.seed(42)											# Set seed
#-------------------------------------------------------------------------
# Determined the observations for the training,validate and test datasets.
N <- nrow(weather)										# 366 observations
train <- sample(N, 0.8*N)								# 292 observations
test <-  setdiff(seq_len(N),train)                      #  74 observations not intrain
#-------------------------------------------------------------------------
# Build the model
M <- ncol(weather)
input  <- names(weather)[1:(M-2)]						# names of input variables
target <- "RainTomorrow"								# name of target variable							
form <- formula(RainTomorrow ~ .)						# Describe the model to R
tree.m <- rpart(RainTomorrow ~ .,
			    data=weather[train, c(input,target)],
				method="class",
				parms=list(split="information"),
				control=rpart.control(usesurrogate=0, maxsurrogate=0))
#---------------------------------------------------------------------------
# Look at the textual description of the tree.
tree.m						# print the model
printcp(tree.m)				# print the table of optimal prunings based on the complexity parameter
#----------------------------------------------------------------------------
# Plot the tree
drawTreeNodes(tree.m)
title(main="Weather Data tree.m",
    sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
#----------------------------------------------------------------------------		
# Evaluate performance
# Run the tree model on the validate set
  pred <- predict(tree.m, weather[test, c(input,target)], type="class")
  levels(pred) <- c("Yes","No")								# change order of levesl to match documentation for confusionMatrix
# Generate the confusion matrix
  actual <- weather[test, c(input,target)]$RainTomorrow
  levels(actual) <- c("Yes","No")							# change order of levels to match documantation for confusion matrix
  AP <- c("Predicted","Actual")								# row names for CM
  CM <- table(pred,actual,dnn=AP)							# CM counts
  confusionMatrix(CM)										# from the caret package		
  ?confusionMatrix											# Look at meaning of confusionMatrix outputs
	
# Notes
# The\no-information rate"shown on the output is the largest proportion of the observed classes
# A one-sided hypothesis test is computed to evaluate whether the overall accuracy rate is greater
# than the rate of the largest class. This is helpful for data sets where there is a large imbalance
# between the classes.
#
# The kappa statistic yields a measure of how well the actual and predicted values agree
# See http://www.chestx-ray.com/statistics/kappa.html or
# http://en.wikipedia.org/wiki/Cohen%27s_kappa
#
# The null hypothesis for McNemar's chi squared test is that the actual and predicted
# probabilities are the same
# See http://en.wikipedia.org/wiki/McNemar%27s_test
#
#--------------------------------------------------------------------------------------------
# Try another model using different variables
form <- formula(RainTomorrow ~ Cloud9am + Pressure9am + WindDir9am + Temp9am + Humidity9am)
tree.m2 <- rpart(form,
			    data=weather[train, c(input,target)],
				method="class",
				parms=list(split="information"),
				control=rpart.control(usesurrogate=2, 
									  maxsurrogate=0,
									  minsplit=30,
									  maxdepth=20))
#----------------------------------------------------------------------------------------------
# Plot the new tree
drawTreeNodes(tree.m2)
title(main="Weather Data tree.m2",
    sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

tree.mod.p <- as.party(tree.m2)		# make the tree.mod object into a party object
plot(tree.mod.p)

#----------------------------------------------------------------------------		
# Evaluate performance of the new model on the test set
pred2 <- predict(tree.m2, weather[test, c(input,target)], type="class")	
levels(pred2) <- c("Yes","No")
CM2 <- table(pred2,actual,dnn=AP)
confusionMatrix(CM2)
# -----------------------------------------------------------------------------------
#
# GENERATE THE ROC CURVE FOR THE BEST MODEL
prROC <- predict(tree.m, weather[test, c(input,target)])[,2]
# 
# Get vector RainTommorrow in test data set
testRT <- weather[test, c(input,target)]$RainTomorrow
pr <- prediction(prROC, testRT)
#------------------------------------------------------------------------------------
# Plot the ROC curve
plot(performance(pr, "tpr", "fpr"), col="#CC0000FF", lty=1, lwd=2,add=FALSE)
	#fpr: False positive rate. P(Yhat = + | Y = -). Estimated as: FP/N.
    #tpr: True positive rate. P(Yhat = + | Y = +). Estimated as: TP/P.
segments(0,0,1,1,col="blue",lwd=2)	
# Add a legend to the plot.
legend("bottomright", c("tree.m"), col=rainbow(1, 1, .8), lty=1:1, title="Models", inset=c(0.05, 0.05))
# Add decorations to the plot.
title(main="ROC Curve  weather.csv [test data]",
      sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
#

