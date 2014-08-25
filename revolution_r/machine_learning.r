#------------------------------------------
# SET THE COMPUTE CONTEXT
#-------------------------------------------
rxGetComputeContext()
rxSetComputeContext("RxLocalParallel")
#-------------------------------------------
# LOAD THE DATA CREATED BY HADOOP PROCESSING
#-------------------------------------------
dataDir <- "C:/DATA/Airlines_87_08"
working.file <- file.path(dataDir,"subAir")
# Look at the meta-data
rxGetInfoXdf(working.file,getVarInfo=TRUE)
#--------------------------------------------
# CREATE A NEW VARIABLE FOR RANDOM SAMPLING
#--------------------------------------------
#
# Create a new data file having a variable with uniform random numbers
# going from 1 to 10. This variable will be used to create the training and test
# data sets.
# A little note on how the random numbers are created:
# A transform should work on an arbitrary chunk of data. Typically
# RevoScaleR functions will test transforms on a small chunk before
# fully processing. The internal variable (.rxNumRows) gives the size 
# of the chunk. 
rxDataStep(inData = working.file, outFile = working.file,
 transforms=list(urns = as.integer(runif(.rxNumRows,1,11))),
 overwrite=TRUE,
 blocksPerRead=10)
rxGetInfo(working.file,getVarInfo=TRUE)
rxHistogram(~urns,data=working.file) #check to see if things look random
#-----------------------------------------------
# SOME DATA EXPLORATION
#-----------------------------------------------
# Look at ArrDelay
rxSummary(~ArrDelay,data=working.file)
rxHistogram(~ ArrDelay, data=working.file, 
 rowSelection=(ArrDelay>-60) & (ArrDelay<250), 
 numBreaks=5000,xNumTicks=20,
 blocksPerRead=10)
# Look at urns, the variable for random sampling
rxHistogram(~urns,data=working.file)
#----------------------------
# BUILD THE TRAINING FILE
#----------------------------
rxDataStepXdf(inFile = working.file, outFile = "airTrain",
 rowSelection = urns < 9, 
 varsToKeep = c("Late","UniqueCarrier","DayOfWeek","Distance","urns"),
 blocksPerRead=20,
 overwrite=TRUE )
##
rxGetInfo("airTrain",getVarInfo=TRUE,numRows=5)
rxHistogram(~Distance,data="airTrain")
#-------------------------
# BUILD THE TEST FILE
#-------------------------
rxDataStepXdf(inFile = working.file, outFile = "airTest",
 rowSelection = urns >= 9, 
 varsToKeep = c("Late","UniqueCarrier","DayOfWeek","Distance","urns"),
 blocksPerRead=20,
 overwrite=TRUE )
##
rxGetInfo("airTest",getVarInfo=TRUE,numRows=5)
rxHistogram(~Distance,data="airTest")
#---------------------------------------------------------------------------
# BUILD A CLASSIFICATION MODEL USING LOGISTIC REGRESSION
#---------------------------------------------------------------------------
model <- rxLogit(Late ~ UniqueCarrier + DayOfWeek + Distance,data="airTrain",cube=TRUE)
#Elapsed computation time: 122.808 secs.
summary(model)
#----------------------------------------------------------------------------
# MAKE PREDICTIONS ON THE TEST DATA
#----------------------------------------------------------------------------
rxPredict(modelObject=model,data="airTest",outData="airTest",overwrite=TRUE)
rxGetInfo("airTest",getVarInfo=TRUE,numRows=5)
rxHistogram(~Late_Pred,data="airTest")
rxHistogram(~Late,data="airTest")
#-------------------------------------------------------------------------------
# GENERATE THE CONFUSION MATRIX
#-------------------------------
library(caret)
#library(e1071)
rxDataStep(inData = "airTest", outFile = "airTest",
 transforms=list(Late_Pred_B = ifelse(test = Late_Pred > .25, yes = 1, no = 0)),
 overwrite=TRUE)
rxGetInfo("airTest",getVarInfo=TRUE)
rxHistogram(~Late_Pred_B,data="airTest") #check to see if things look random
Table <- rxCrossTabs(~ F(Late):F(Late_Pred_B),data="airTest")
Table
class(Table)
TableR <- as.xtabs(Table)
class(TableR)
confusionMatrix(TableR)
#> confusionMatrix(TableR)
#Loading required package: class
#Confusion Matrix and Statistics
# #F_Late_Pred_B
#F_Late 0 1
 #0 13398235 22660
 #1 3380266 7776
 #
 #Accuracy : 0.7976 
 #95% CI : (0.7974, 0.7977)
 #No Information Rate : 0.9982 
 #P-Value [Acc > NIR] : 1 
 #
 #Kappa : 0.001 
 #Mcnemar's Test P-Value : <2e-16 
 #
 #Sensitivity : 0.798536 
 #Specificity : 0.255487 
 #Pos Pred Value : 0.998312 
 #Neg Pred Value : 0.002295 
 #Prevalence : 0.998189 
 #Detection Rate : 0.797090 
 #Detection Prevalence : 0.798438 
 #
 #'Positive' Class : 0 
#---------------------------------------------------------------
# BUILD A TREE MODEL
rxGetInfo(working.file,getVarInfo=TRUE,numRows=5)
system.time(
model.tree <- rxDTree(Late ~ UniqueCarrier + DayOfWeek + Distance,data="airTrain",
 maxDepth=5,
 cp = 0,
 blocksPerRead=10)
 )
model.tree
library(RevoTreeView)
plot(createTreeView(model.tree))
#--------------------------------------------------------