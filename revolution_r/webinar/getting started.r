#
#------------------------------------------------------------ 
# REVOLUTION ANALYTICS WEBINAR: INTRODUCTION TO R FOR DATA MINING
# February 14, 2013
# Joseph B. Rickert
# Technical Marketing Manager
#
# GETTING STARTED
#
# Copyright: Revolution Analytics
# This script is licensed under the GPLv2 license
# http://www.gnu.org/licenses/gpl-2.0.html

#---------------------------------------------------------------------
# Execute the following command to install all of the packages needed for the webinar
#install.packages(c( "ada","boot","caret","corrplot","doParallel","ellipse",
			        #"ISwR","partykit","pROC","rattle","RColorBrewer",
					#"rpart","Snowball","ROCR","tm","twitteR","wordcloud"))
#
#----------------------------------------------------------------------
# A First look at R
# A simple regression example from
# Statistics and Computing, Introductory Statistics with R
# Peter Dalgaard, Springer 2002
##
library(ISwR)			# Load a library
data()					# Have a look at what data sets are available
data(thuesen)			# Load thuesen into the environment
thuesen					# Have a look at it
class(thuesen)          # Find out what kind of object thuesen is
sapply(thuesen,class)   # See what kinds of animal the variables are
#
plot(short.velocity ~ blood.glucose, data=thuesen) #plot the data using the formula interface
#
plot(thuesen$blood.glucose,thuesen$short.velocity) # plot the data by indexining into the data frame
#
model <- lm(short.velocity ~ blood.glucose, data=thuesen) # build a linear model
summary(model)			# Look at the results
str(model)				# Look at the structure of the model object
# Build a fancier plot
plot(x=thuesen$blood.glucose,
	 y=thuesen$short.velocity,
	 xlab="blood glucose (mmol / l)",
	 ylab = "circumferential shortening velocity (%/s)",
	 main = "Thuesen Data set",
	 col="blue",
	 pch=19
	 )
abline(model,col="red")
#
par(mfrow=c(2,2))				# Set up for multiple plots
plot(model, col="blue")			# look at some diagnostics

#---------------------------------------------------------------------
#
# A FIRST LOOK AT FUNCTIONS
#
# Let's create a simple function
joe.stats <- function(data){
	           min <- min(data)
			   max <- max(data)
			   q <- quantile(data,probs=seq(0,1,0.25))
			   res <- list(min,max,q)
			   return(res)
		}
		
attach(thuesen)			 # make the columns of thuesen available
                         # in the global environment as variables
joe.stats(blood.glucose) # Run our function
			   
summary(blood.glucose)   # R does it better


# Set up for later
rm(list=ls())
load("WEBINAR_2-14-13_Intro_R_DM_caret .RData")
#--------------------------------------------------------------------------------
#SOME ADDITIONAL ONLINE RESOURCES
#An Introduction to R
#Notes on R: A Programming Environment for Data Analysis and Graphics
#Version 2.15.2 (2012-10-26)
#http://cran.r-project.org/doc/manuals/R-intro.pdf
#
#Using R for Data Analysis and Graphics
#Introduction, Code and Commentary
#J H Maindonald
#http://cran.r-project.org/doc/contrib/usingR.pdf