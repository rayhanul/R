


source('C:/Users/x-man/Copy/R/Defect_Prediction/TimMenzies.R')

getPredictedDefects()

rm(list=ls())
#.................Entire system................

source('C:/Users/x-man/Copy/R/Defect_Prediction/EntireSystem.R')

predictDefectUsingEntireSystem()

rm(list=ls())


#.............DbScan.....................

source('C:/Users/x-man/Copy/R/Defect_Prediction/predictionModel.R')

predictDefect()

rm(list=ls())


#.............BorderFlow.............

source('C:/Users/x-man/Copy/R/Defect_Prediction/BorderFlowScript.R')

predictDefectUsingBorderFlow()
rm(list=ls())


#......................combining all results..................
source('C:/Users/x-man/Copy/R/Defect_Prediction/TimMenzies.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/EntireSystem.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/predictionModel.R')

path<-paste(sourcePath[["mainFile"]],'ResidualValForEntireSystem.csv',sep='.')
entire<-read.csv(path)
path<-paste(sourcePath[["mainFile"]],'ResidualValForDbScan.csv',sep='.')
dbscan<-read.csv(path)
path<-paste(sourcePath[["mainFile"]],'ResidualValForTimMenzies.csv',sep='.')
menzies<-read.csv(path)

