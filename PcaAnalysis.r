source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')


data<-read.csv(sourcePath["mainFile"])





getComponentAnalysis<-function(data){

fit<-princomp(data$trainset[3:10],cor = TRUE)
comp<-fit$scores[,1]

testFit<-princomp(data$testset[3:10],cor=TRUE)
testCmp<-testFit$scores[,1]
return(list(mainComp=comp,testComp=testCmp))
}


getClusters<-function(data){

kmData<-getComponentAnalysis(data)

cluster<-kmeans(kmData,5)
predictedClster<-predict(cluster,data)
return(list(mainCluster=cluster,predictedCluster=predictedClster))
}

getPredictedDefects<-function(trainData,testData){

	predictionModel<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
	predictedDefect<-predict(predictionModel,testData,interval="predict")
	return(predictedDefect)
}

predictDefectUsingPCA<-function(){

	data<-read.csv(sourcePath["mainFile"])
	splittedData<-splitData(data)
	cluster<-getClusters(splittedData)
	
	combindedData<-c()
	
		for(i in 1: 5){
		#combindedPath<-paste(sourcePath[['BorderFlowfilePath']],allFiles[[i]],sep="/")
			#data<-read.csv(combindedPath)
			trainData<-splittedData$trainset[cluster$mainCluster$cluster==i,]
			predictionModel<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
			testDataPerCluster=splittedData$testset[cluster$predictedClster$cluster==i,]
			predictedDefect<-predict(predictionModel,testDataPerCluster,interval="predict")
			#predictedDefect<-getPredictedDefects(splittedData$trainset,splittedData$testset)	
			combindedData<-rbind(combindedData,predictedDefect)
		}
	writeDataFrameTo(combindedData,sourcePath[['PcaAnalysisPath']],"RESIDUALForPca")
return(combindedData)
}




