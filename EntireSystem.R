
source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
#sourcePath <- c(filePath="C:/Users/x-man/Copy/R/Data")
source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')

getPredictedDefects<-function(trainData,testData){

	predictionModel<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
	predictedDefect<-predict(predictionModel,testData,interval="predict")
	return(predictedDefect)
}

predictDefectUsingEntireSystem<-function(fileName){

	combindedData<-c()
	#combindedPath<-paste(sourcePath[['dataPath']],fileName,sep="/")
	data<-read.csv(sourcePath[["mainFile"]])
	splittedData<-splitData(data)
	predictedDefect<-getPredictedDefects(splittedData$trainset,splittedData$testset)	
	combindedData<-rbind(combindedData,predictedDefect)
	writeDataFrameTo(combindedData,sourcePath[['dataPath']],"predictedDefectsUsingEntire.csv")
return(combindedData)
}

