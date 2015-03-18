source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')

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

# predictDefectUsingBorderFlow<-function(){
# 
# 	allFiles<-list.files(sourcePath[['BorderFlowfilePath']])
# 	combindedData<-c()
# 		for(i in 1: length(allFiles)){
# 		combindedPath<-paste(sourcePath[['BorderFlowfilePath']],allFiles[[i]],sep="/")
# 			data<-read.csv(combindedPath)
# 			splittedData<-splitData(data)
# 			predictedDefect<-getPredictedDefects(splittedData$trainset,splittedData$testset)	
# 			combindedData<-rbind(combindedData,predictedDefect)
# 		}
# 	writeDataFrameTo(combindedData,sourcePath[['BorderFlowfilePath']],"RESIDUAL")
# return(combindedData)
# }

data<-read.csv(sourcePath[['mainFile']],stringsAsFactors=FALSE)
splittedData<-splitData(data)


getPredictedDefects<-function(trainData,testData,times){

	predictionModel<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
	
	# writing model summary....................
	fileName<-paste("ModelSummaryForBorderFlow",times,sep="_")
	fileName<-paste(fileName,"csv",sep=".")
	writeModelSummary(predictionModel,sourcePath[['dataPath']],fileName)
	
	predictedDefect<-predict(predictionModel,testData,interval="predict")
	return(predictedDefect)
}

getDataForClusterId<-function(mainData,clusterInfoList, clusterId){
  clusterTrainingData<-c()
	for(i in 1: nrow(clusterInfoList)){
		clusterInfoListRowItem<-clusterInfoList[i,]
		if(clusterInfoListRowItem[["jointClusterId"]]==clusterId){
			for(j in 1:nrow(mainData)){
				mainDataRowItem<-mainData[j,]
				a=mainDataRowItem["name"]
				b=clusterInfoListRowItem["name"]
				if(a == b){
					clusterTrainingData<-rbind(clusterTrainingData,mainDataRowItem)
				}
			}
		}
	}
 return(clusterTrainingData)
}

predictDefectUsingBorderFlow<-function(){

	clustersInfo<-read.csv(sourcePath[['BorderFlowClusterInfofile']],stringsAsFactors=FALSE)
	numberOfClusters<-unique(clustersInfo[,2])
	
	combindedData<-c()
		for(i in 1: length(numberOfClusters) ){		
			trainingData<-getDataForClusterId(splittedData$trainset,clustersInfo,numberOfClusters[i])
			testData<-getDataForClusterId(splittedData$testset,clustersInfo,numberOfClusters[i])
			predictedDefect<-getPredictedDefects(trainingData,testData,i)	
			combindedData<-rbind(combindedData,predictedDefect)
		}
	#adding header info to file............
	combindedData<-data.frame(combindedData)
	names(combindedData)<-c("resVal","id")
  
	writeDataFrameTo(combindedData,sourcePath[['dataPath']],"ResidualValForBorderFlow.csv")
return(combindedData)
}


