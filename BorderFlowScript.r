source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
source('C:/Users/x-man/Copy\R/Defect_Prediction/DataInfo.R')

#splitData<-function(myData){

	## 75% of the sample size
	#smp_size <- floor(0.90 * nrow(myData))

	## set the seed to make your partition reproductible
	#set.seed(1232343324)
	#train_ind <- sample(seq_len(nrow(myData)), size = smp_size)

	#train <- myData[train_ind, ]
	#test <- myData[-train_ind, ]
	#list(trainset=train, testset=test)
#}

getPredictedDefects<-function(trainData,testData){

	predictionModel<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
	predictedDefect<-predict(predictionModel,testData,interval="predict")
	return(predictedDefect)
}

predictDefectUsingBorderFlow<-function(){

	allFiles<-list.files(sourcePath[['BorderFlowfilePath']])
	combindedData<-c()
		for(i in 1: length(allFiles)){
		combindedPath<-paste(sourcePath[['BorderFlowfilePath']],allFiles[[i]],sep="/")
			data<-read.csv(combindedPath)
			splittedData<-splitData(data)
			predictedDefect<-getPredictedDefects(splittedData$trainset,splittedData$testset)	
			combindedData<-rbind(combindedData,predictedDefect)
		}
	writeDataFrameTo(combindedData,sourcePath[['BorderFlowfilePath']],"RESIDUAL")
return(combindedData)
}