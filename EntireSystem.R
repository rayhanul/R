
source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/HerederInfoManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/ResidualValueAnalysis.R')
getPredictedDefects<-function(trainData,testData){

	predictionModel<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
	predictedDefect<-predict(predictionModel,testData,interval="predict")
	return(predictedDefect)
}
# predicting defects..................main method
predictDefectUsingEntireSystem<-function(){

	combindedData<-c()
	#combindedPath<-paste(sourcePath[['dataPath']],fileName,sep="/")
	data<-read.csv(sourcePath[["mainFile"]])
	splittedData<-splitData(data)
	predictedDefect<-getPredictedDefects(splittedData$trainset,splittedData$testset)	
	combindedData<-rbind(combindedData,predictedDefect)
	
	writeDataFrameTo(combindedData,sourcePath[['dataPath']],"testDataWithResidualValuesForEntireSystem.csv")
    tempData<-correctingheaderInfo(sourcePath[['dataPath']],"testDataWithResidualValuesForEntireSystem.csv")
	writeDataFrameWithOutRowIdColumn(tempData,sourcePath[['dataPath']],"testDataWithResidualValuesForEntireSystem.csv")
  computeResidualValue("testDataWithResidualValuesForEntireSystem.csv","ResidualValForEntireSystem.csv")
return(combindedData)
}



