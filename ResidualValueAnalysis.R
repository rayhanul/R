
#source('C:/Users/xman/Copy/R/Defect_Prediction/FileManager.R')

#source('C:/Users/xman/Copy/R/Defect_Prediction/DataInfo.R')



computeResidualValue<-function(predictedFileName,ResidualValuesFileName){

	mainData<-read.csv(sourcePath[["mainFile"]])
  predictedFilePath<-paste(sourcePath[['dataPath']],predictedFileName,sep="/")
	predictedData<-read.csv(predictedFilePath)
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
	
  #..........
	allResidualValues<-data.frame(allResidualValues)
	names(allResidualValues)<-c("resVal","id")
  
	writeDataFrameWithOutRowIdColumn(allResidualValues,sourcePath[['dataPath']],ResidualValuesFileName)
	return(allResidualValues)
}
