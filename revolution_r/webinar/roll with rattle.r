##############################################################################
# REVOLUTION ANALYTICS WEBINAR: INTRODUCTION TO R FOR DATA MINING
# February 14, 2013
# Joseph B. Rickert
# Technical Marketing Manager
#
# ROLL with RATTLE
#
# Copyright: Revolution Analytics
# This script is licensed under the GPLv2 license
# http://www.gnu.org/licenses/gpl-2.0.html

#################################################################################
#
library(rattle)									# load the rattle package
rattle()										# start the rattle user interface
#
#data()											# see what data sets are available in all of the loaded packages
data(package="rattle")							# see what data sets are availeble in rattle
ls("package:rattle")							# See what functions are in the Rattle package
#lsf.str("package:rattle")						# see what functions are in rattle
#
# THE FOLLOWING INSTRUCTIONS SHOULD BE HELPFUL FOR EXPLORING THE RATTLE GUI
#
# LOAD THE WEATHER DATA SET.
# The weather data set consists of observations made at a weather monitoring station
# in Canberra, Australia. Each ovservation describs weather conditions on a particular day.
# See page 25 of Gram Willians' Data Mining with Rattle, The Art of Excavating Data for 
# Knowlwdge Discovery, Springer 2011
#
#		Go to the Data Tab and click on Execute
#       Rattle will ask if you want to use the weather data as default. Click yes.
# 
# SUMMARY STATISTICS
#		Go to the Explore Tab
#		Select summary and basics
#       Hit Execute
# 
# SCATTER PLOTS
#		Go to the Explore Tab
#		Select Distributions
#		Click on Execute
#
# LOOK AT A SINGLE VARIABLE
#		Go to Explore Tab
#		Select RainTomorrow Bar Plot
#		Hit Execute
#		This produces a bar plot of the target variable RainTomorrow
#		84% of the observations have no rain
#		A model that always predicts no rain should be about 84% accurate
#
# INVESTIGATE MULTIPLE VARIABLES
#		Go to the Explore Tab
#       In the upper panel select Box Plot and Histogram for both
#			MaxTemp
#			Sunshine
#		Click Execute
#
#		Boxplots top left: Temperature generally higher day before it rains
#		Boxplots top right: Less sunshine day before it rains
#
# CORRELATIONS
#		Go to the Explore Tab
#		Un select any variables that may be selected
#		Select Correlation
#		Click on Execute
#
#
# INTERACTIVELY EXPLORE DATA
#	Select Interactive and then Lattiscist
#	In bottom center panel
#		Select MaxTemp for y axis and	
#		Select Sunshine for x axis
#		Place crosshair on outlier and right click
#
# BUILD A TREE MODEL
#	Go to Model Tab 
#	Select Tree
#	Click Execute
#	Click on Draw button to get the graph
#	Click on Rules button to see rules
#	Select Log Tab to look at R code
#
# EVALUATE THE MODEL
#	Go to the Evaluate tab
#	Select 
#		Type = Error Matrix
#		Model = Tree
#		Data = Testing
#	Click on Execute
#
# Error matrix for the Decision Tree model on weather.csv [test] (counts):
#
      #Predicted
#Actual No Yes
   #No  35   6  False positive rate = FP/N =  6/(35+6) = .146   = negatives incorrectly classified / total negatives
   #Yes  5  10  True positive rate  = TP/P = 10/(10+5) = .667   = positives correctly classified / total positives
#                                   = sensitivity  = recall = hit rate
#               True negative rate  = TN / (FP + TN) = 1 - FP rate  = .854
#                                   = specificity
#
#               False positives = 6 = Type  I Error (Test rejects true null hypothesis)
#               False negatives = 5 = Type II Error (Test fails to reject false null hypothesis)

#Error matrix for the Decision Tree model on weather.csv [test] (%):
#
      #Predicted
#Actual No Yes
   #No  62  11    62% (35/56) of cases model predicts it won't rain and it didn't
   #Yes  9  18    18% (10/56) of cases model predicts it would rain and it did
#                 Accracy of test = 62% + 18% = 80%
#
#Overall error: 0.1964286