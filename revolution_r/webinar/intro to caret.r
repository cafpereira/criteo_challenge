 
#------------------------------------------------------------------------------
# REVOLUTION ANALYTICS WEBINAR: INTRODUCTION TO R FOR DATA MINING
# February 14, 2013
# Joseph B. Rickert
# Technical Marketing Manager
#
# DATA MINING with CARET
#
# Copyright: Revolution Analytics
# This script is licensed under the GPLv2 license
# http://www.gnu.org/licenses/gpl-2.0.html
#------------------------------------------------------------------------------
# INTRODUCTION TO THE CARET PACKAGE
# caret is a feature rich package for doing data mining in R.
# This script explores caret's capabilities using data included in the 
# package that was described in the paper:
# Hill et al "Impact of image segmentation on high-content
# screening data quality for SK-BR-3 cells"
# BMC fioinformatics (2007) vol 8 (1) pp. 340
#
# Background
# Well-segmented cells are cells for which location and size may be accurrately detremined
# through optical measurements. Cells that are not Well-segmented (WS) are said to be 
# Poorly-segmented (PS).
#
# Problem
# Given a set of optical measurements can we predict which cells will be PS?
# This is a classic classification problem
#---------------------------
library(ada)				# Boosting algorithms
library(caret)
library(rpart)				# CART algorithm for decision trees
library(partykit)			# Plotting trees		
library(doParallel)			# parallel processing
                            # by default
							#     Multicore functionality on Unix (single machine only)
							#     Snow functionality on Windows (cluster)
							
							
library(pROC)				# plot the ROC curve
library(corrplot)			# plot correlations

#---------------------------
# data(package="caret")
data(segmentationData)		# Load the segmentation data set
dim(segmentationData)
head(segmentationData)		# Have a look at the data
#[1] 2019   61
trainIndex <- createDataPartition(segmentationData$Case,p=.5,list=FALSE)
trainData <- segmentationData[trainIndex,]
dim(trainData)
 #1010   61
testData  <- segmentationData[-trainIndex,]
dim(testData)
 #1009   61
#-------------------------------------------------------------------------------------
# VISUALIZE CORRELATIONS
trainV <- trainData[,4:61]
corrplot(cor(trainV),order="hclust",tl.cex=.5,method="ellipse")

#-----------------------------------------------------------------
# BUILD AN ADABOOST MODEL WITH ADA
form <- formula(Class ~ .)
control <- rpart.control(maxdepth=30,   # the maximum depth of any node of the final tree
	                     cp = 0.01,     # complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
						 minsplit=20,   # the minimum number of observations that must exist in a node in order for a split to be attempted
						 xval=10)       # number of cross-validations
					
ada.model <- ada(formula=form,
	             data=trainData,
				 control=control,
				 nu = .01,				# shrinkage parameter for boosting
				 iter=50)

ada.model$model[[1]]					# Look at the trees in the model
ada.model								# look at the model performance
plot(ada.model,TRUE)					# Plot error rate vs. iterations of the model
varplot(ada.model)						# Variable importance plot
#----------------------------------------------------------------------
# FIND THE "BEST" MODEL
#
# This is an interesting model, but how do you select the best values for the
# for the three tuning parameters?
#		nu
#		iter
#		maxdepth
#---------------------------------------------------------------------------------
# Algorithm for training the model:
# for each resampled data set do
#		hold out some samples
#		for each combination of the three tuning parameters
#			do
#				Fit the model on the resampled data set
#				Predict the values of class on the hold out samples
#			end
#		Calculate AUC: the area under the ROC for each sample
#		Select the combination of tuning parmeters that yields the best AUC
#
# caret provides the "train" function to do all of this
#
# The trainControl function to set the training method
# Note the default method of picking the best model is accuracy and Cohen's Kappa
#
#-----------------------------------------------------------------------------------
# Set up the parameters to run the boosting function
ctrl <- trainControl(method="repeatedcv",					# use repeated 10fold cross validation
						number=5,							# the number of folds
						repeats=2,							# do 2 repititions of 5-fold cv
						summaryFunction=twoClassSummary,	# Use AUC to pick the best model
						classProbs=TRUE)
# Use the expand.grid to specify the search space	
# Note that the default search grid selects 3 values of each tuning parameter
#
grid <- expand.grid(.nu=c(.1,1), # 
					.iter=c(20,50),
					.maxdepth=c(20,30))			# 
#	
set.seed(1)
#names(trainData)
trainX <-trainData[,4:61]
#-----------------------------------------------------------------
# PARALLEL COMPUTING
# vignette("gettingstartedParallel")

cl <- makeCluster(4)		 # Use this to manually create a cluster
                             # But, since I only have a single Windows machine
							 # all I am doing is passing the number of cores to use to 
							 # registerDoParallel()
registerDoParallel(cl)		 # Registrer a parallel backend for train
getDoParWorkers()

system.time(ada.tune <- train(x=trainX,y=trainData$Class,
				method = "ada",
				metric = "ROC",
				trControl = ctrl,
				control=control,
				tuneGrid=grid))
#
stopCluster(cl)
 
#user  system elapsed 
#14.33    0.02  206.25 
#-------------------------------------------------------------------------------
# ADA RESULTS
ada.tune						# Look at the results for the training grid				
ada.tune$finalModel				# Look at the performance of the final model
plot(ada.tune)					# Plot the performance of the training models
#--------------------------------------------------------------------------------
# ADA PREDICTIONS
testX <- testData[,4:61]
ada.pred <- predict(ada.tune,testX)
#
confusionMatrix(ada.pred,testData$Class)
#-----------------------------------------------------------------
# DRAW THE ROC CURVE
# Use roc function from the pROC package
ada.probs <- predict(ada.tune,testX,type="prob")
ada.ROC <- roc(predictor=ada.probs$PS,
				response=testData$Class,
				levels=rev(levels(testData$Class)))
plot(ada.ROC,col=2)
ada.ROC$auc		# Get the area under the curve
#------------------------------------------------------------------------------------
#
# SUPPORT VECTOR MACHINE MODEL
#
set.seed(1)
registerDoParallel(4,cores=4)
getDoParWorkers()
system.time(
	svm.tune <- train(x=trainX,
					y= trainData$Class,
					method = "svmRadial",
					tuneLength = 5,					# 5 values of the cost function
					preProc = c("center","scale"),
					metric="ROC",
					trControl=ctrl)					# same as for ada above
)	
	
#user  system elapsed 
   #2.40    0.14   26.10 


#--------------------------------------------------------------
# SVM RESULTS
svm.tune						# Look at the results for the training grid				
svm.tune$finalModel				# Look at the performance of the final mode
plot(svm.tune,
	metric="ROC",
	scales=list(x=list(log=2)))
#---------------------------------------------------------------
# SVM PREDICTIONS
svm.pred <- predict(svm.tune,testX)
confusionMatrix(svm.pred,testData$Class)
#
#----------------------------------------------------------------
# COMPARE THE SVM AND ADA MODELS USING RESAMPLING
#
# Because we set the same seed before running the models we can compare the models using resampling
# See Hothorn at al, "The design and analysis of benchmark experiments"
# Journal of Computational and Graphical Statistics (2005) vol 14 (3) pp 675-699
# for comparing models using resampling.
#
# The resamples function in caret collates the resampling results from the two models
rValues <- resamples(list(svm=svm.tune,ada=ada.tune))		
rValues$values					# Look at the resample values	
summary(rValues)				# Summarize the resamples

#---------------------------------------------
xyplot(rValues,metric="ROC")		# scatter plot
bwplot(rValues,metric="ROC")		# boxplot
parallel(rValues,metric="ROC")		# parallel plot
dotplot(rValues,metric="ROC")		# dotplot
#
