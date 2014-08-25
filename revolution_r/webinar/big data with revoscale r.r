
#------------------------------------------------------------ 
# REVOLUTION ANALYTICS WEBINAR: INTRODUCTION TO R FOR DATA MINING
# February 14, 2013
# Joseph B. Rickert
# Technical Marketing Manager
#
# BIG DATA with RevoScaleR
#
# Copyright: Revolution Analytics
# This script is licensed under the GPLv2 license
# http://www.gnu.org/licenses/gpl-2.0.html
# ----------------------------------------------------------------------
# LOOK AT THE MORTGATE DEFAULT DATA
#------------------------------------------------------------------------
dataDir <- "C:/Users/Joseph/Documents/DATA/Mortgage Data/mortDefault"
mdata <- file.path(dataDir,"mortDefault.xdf")
rxGetInfo(mdata,getVarInfo=TRUE)

#-----------------------------------------------------------------------------------
## Create a new data file having a variable with uniform random numbers
# going from 1 to 10. This variable will be used to create the training and test
# data sets.
# A little note on how the random numbers are created:
#		A transform should work on an arbitrary chunk of data.  Typically
#		RevoScaleR functions will test transforms on a small chunk before
#		fully processing. The internal variable (.rxNumRows) gives the size 
#		of the chunk.  

rxDataStep(inData = mdata, outFile = "mortDefault2",
      transforms=list(urns = as.integer(runif(.rxNumRows,1,11))),
      overwrite=TRUE)
rxGetInfo("mortDefault2",getVarInfo=TRUE,numRows=3)

#
#------------------------------------------------------------
# KMEANS ANALYSIS 
#------------------------------------------------------------
rxDataStep(inData="mortDefault2",outFile="mortDefault3",
	       varsToDrop="default",
		   overwrite=TRUE)
rxGetInfo("mortDefault3",getVarInfo=TRUE,numRows=5)

form <- formula(~ creditScore + houseAge + yearsEmploy + ccDebt + year)
md.km <- rxKmeans(formula=form, 
						data = "mortDefault3", 
						numClusters = 3,
 						outFile = "mortDefault3",
						algorithm = "lloyd",
						overwrite=TRUE)
rxGetInfo("mortDefault3",getVarInfo=TRUE,numRows=5)	
md.km
# Build a data frame to do a plot		
mdDf <- rxXdfToDataFrame(file="mortDefault3",
						 rowSelection=urns == 5,
                         maxRowsByCols = 1000)
			
plot(mdDf[,1:4],col=mdDf$.rxCluster)
title(main="Clusters in Mortgage Default Data",line=3)

###### SCRIPT TO BUILD LOGISTIC REGRESSION MODEL TO PREDICT MORTGAGE DEFAULTS #####
#---------------------------------------------------------------------------
# Some subsidary functions
#---------------------------------------------------------------------------
# Function to compute a "long form" of the confusion matrix
Cmatrix <- function(df){
	df <- as.data.frame(df)
	df$Result <- c("True Negative","False Negative","False Positive","True Positive")
	df$PCT <- round(df$Counts/sum(df$Counts),2)*100
	df$Rates <- round(c(df$Counts[1]/(df$Counts[1]+df$Counts[3]),
	               df$Counts[2]/(df$Counts[2]+df$Counts[4]),
				   df$Counts[3]/(df$Counts[1]+df$Counts[3]),
	               df$Counts[4]/(df$Counts[2]+df$Counts[4])),2)
	names(df) <- c("Actual","Predicted","Counts","Results","Pct","Rates")
	return(df)
}
#------------------------------------------------------------------------------
##### CREATE TRAINING AND TEST FILES
#-----------------------------------
#info <- rxGetInfo(mdata)
#N <- info$numRows
#

#-------------------------------------------------------------------------------
# BUILD THE TRAINING FILE
#------------------------
rxDataStepXdf(inFile = "mortDefault2", 
	          outFile = "mdTrain",
              rowSelection = urns < 9,
 			  transforms=list(CS = creditScore,
				              YR = year,
						      yrE = yearsEmploy,
						      HA = houseAge,
							  ccD = ccDebt),
			  blocksPerRead=20,
			  rowsPerRead=500000,
			  overwrite=TRUE )

rxGetInfo("mdTrain",getVarInfo=TRUE,numRows=5)
rxHistogram(~default,data="mdTrain")
#-------------------------
# BUILD THE TEST FILE
#-------------------------
rxDataStepXdf(inFile = "mortDefault2", 
	          outFile = "mdTest",
              rowSelection = urns > 8,
 			  transforms=list(CS = creditScore,
				              YR = year,
						      yrE = yearsEmploy,
						      HA = houseAge,
							  ccD = ccDebt),
			  blocksPerRead=20,
			  rowsPerRead=500000,
			  overwrite=TRUE )
#
rxGetInfo("mdTest",getVarInfo=TRUE,numRows=5)
rxHistogram(~default,data="mdTest")
#---------------------------------------------------------------------------
# BUILD A CLASSIFICATION MODEL USING LOGISTIC REGRESSION
#---------------------------------------------------------------------------
system.time(
model <- rxLogit(default ~ F(houseAge) + F(year)+ creditScore + yearsEmploy + ccDebt, 
	             data="mdTrain", 
				 reportProgress=rxGetOption("reportProgress") )
			)
#
#Elapsed computation time: 21.533 secs.
   #user  system elapsed 
  #56.15   12.02   21.55 

	
#Elapsed computation time: 23.149 secs.
   #user  system elapsed 
  #56.81   10.58   23.17 
#Elapsed computation time: 24.384 secs.
   #user  system elapsed 
  #59.29   10.31   24.48 

summary(model)

#----------------------------------------------------------------------
# MAKE PREDICTIONS ON THE TEST DATA USING THE MODEL CREATED ABOVE
#----------------------------------------------------------------------
rxPredict(modelObject=model,data="mdTest",outData="mdTest",overwrite=TRUE,predVarNames="LogitPred")
rxGetInfo("mdTest",getVarInfo=TRUE,numRows=5)
#rxSummary(~default_Pred,data="mdTest")
# Add a new prediction variable
rxDataStep(inData="mdTest",outFile="mdTest",
	            transforms=list(LogitPred.L = as.logical(round(LogitPred))),
				overwrite=TRUE)
#
rxGetInfo("mdTest",getVarInfo=TRUE,numRows=5)

#-------------------------------------------------------------------------------
# GENERATE THE CONFUSION MATRIX
#-------------------------------
conMc <- rxCube(~ F(default):F(LogitPred.L),data="mdTest")
Cmatrix(conMc)

# Examine the performance of the model
total.pct.correct <- round(100*(conMc$Counts[1]+conMc$Counts[4]) / sum(conMc$Counts),2)
total.pct.correct
#-----------------------------------------------------------------------------------
# Generate the ROC Curve
#
rxRocCurve(actualVarName="default",predVarName="LogitPred",data="mdTest")
#
#-------------------------------------------------------------------------------------

# BUILD A TREE MODEL
system.time(
model.tree <- rxDTree(default ~ HA + YR + CS + yrE + ccD, 
	             data="mdTrain", 
				 blocksPerRead = 1,
				 maxDepth=5,
				 reportProgress=rxGetOption("reportProgress") )
			)
##	

#Elapsed time for RxDTreeBase: 89.545 secs.
#
   #user  system elapsed 
 #245.13   12.50   89.57


#Elapsed time for RxDTreeBase: 403.785 secs.
# This was to fully build out the tree
#user  system elapsed 
#1092.37   75.89  403.83 

model.tree
#
#----------------------------------------------------------------
# Plot the Tree
plot(rxAddInheritance(model.tree),uniform=TRUE)
text(rxAddInheritance(model.tree),digits=2)
title(main="Classification Tree for Mortgage Data",
    sub=paste(format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
#-------------------------------------------------------------------

###### - END DEMO HERE - ###########







