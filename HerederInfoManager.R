#source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')


correctingheaderInfo<-function(sourcePath,fileName){

combinedPath<-paste(sourcePath,fileName,sep="/")
tempData<-read.csv(combinedPath)
colnames(tempData) <- c("id", "fit","lwr","upr")
#writeDataFrameTo(temData,sourcePath[['dataPath']],fileName)
return(tempData)
}