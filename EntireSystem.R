
source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
sourcePath <- c(filePath="C:/Users/x-man/Copy/R/Data")

getPredictedDefects<-function(trainData,testData){

	predictionModel<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
	predictedDefect<-predict(predictionModel,testData,interval="predict")
	return(predictedDefect)
}

predictDefectUsingBorderFlow<-function(fileName){

	combindedData<-c()
	combindedPath<-paste(sourcePath[['filePath']],fileName,sep="/")
	data<-read.csv(combindedPath)
	splittedData<-splitData(data)
	predictedDefect<-getPredictedDefects(splittedData$trainset,splittedData$testset)	
	combindedData<-rbind(combindedData,predictedDefect)
	writeDataFrameTo(combindedData,sourcePath[['filePath']],"RESIDUAL.csv")
return(combindedData)
}

