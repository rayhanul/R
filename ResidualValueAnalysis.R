
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')

source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')



computeResidualValue<-function(){

	mainData<-read.csv(sourcePath[["mainFile"]])
	predictedData<-read.csv(sourcePath[["predictedFileForEntire"]])
	allResidualValues<-c()
	for(i in 1:nrow(predictedData)){
	predictedRowItem<-predictedData[i,]
	rowResidualValue<-c()
		for(j in 1:nrow(mainData)){
			mainRowItem<-mainData[j,]
			if(predictedRowItem[["id"]]==mainRowItem[["id"]]){
				rowResidualValue<-abs(predictedRowItem[["fit"]]-mainRowItem[["bug"]])
				rowResidualValue<-cbind(rowResidualValue,predictedRowItem[["id"]])
			}
		}
		allResidualValues<-rbind(allResidualValues,rowResidualValue)
	}
	writeDataFrameTo(allResidualValues,sourcePath[['dataPath']],"ResidualValForEntire.csv")
	return(allResidualValues)
}
