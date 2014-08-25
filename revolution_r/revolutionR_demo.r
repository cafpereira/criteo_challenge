sampleDataDir <- rxGetOption("sampleDataDir")
getwd() # This is the location where the files created will be saved.
inputFile <- file.path(sampleDataDir, "AirlineDemoSmall.csv")
airDS <- rxImport(inData = inputFile, outFile = "ADS.xdf", missingValueString = "M", 
  stringsAsFactors = TRUE)
colInfo <- list(DayOfWeek = list(type = "factor", levels = c("Monday", "Tuesday", 
  "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
system.time(airDS <- rxImport(inData = inputFile, outFile = "ADS.xdf", 
  missingValueString = "M", colInfo = colInfo, overwrite = TRUE))
nrow(airDS)
ncol(airDS)
head(airDS)
rxGetVarInfo(airDS)
print(adsSummary <- rxSummary(~ArrDelay+CRSDepTime+DayOfWeek, data = airDS))
# For curiosity, see how fast this executes...
system.time(adsSummary <- rxSummary(~ArrDelay+CRSDepTime+DayOfWeek, data = airDS))