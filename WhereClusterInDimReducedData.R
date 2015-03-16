source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/multiplyCoefficient.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DbScanCluster.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/ResidualValueAnalysis.R')


data<-read.csv(sourcePath["mainFile"])

splittedData<-splitData(data)


getFirstQuadrantPosition<-function(dimReducedData){

#largestAndSmallest<-getlargestAndSmallestItem(splittedData$trainset)

    x<-median(dimReducedData[,1])
	y<-median(dimReducedData[,2])
	
	return(list(x=x,y=y))

}


divideDataIntoQuadrant<-function(position, dimReducedData){

	
	newdimReducedData<-c()
		for(i in 1: nrow(dimReducedData)){
			rowItem<-dimReducedData[i,]
			#...first quadrant....
			if(rowItem[1]>position["x"] & rowItem[2]>position["y"]){
		  
			  rowItem[3]<- 1
			}
			
			
			#...secend quadrant....
			if(rowItem[1]>position["x"] & rowItem[2]<position["y"]){
			 # newItem<-cbind(rowItem,c(2))
			  rowItem[3]<- 2
			}
			
			#...3rd quadrant....
			if(rowItem[1]<=position["x"] & rowItem[2]<=position["y"]){
			  #newItem<-cbind(rowItem,c(3))
			  rowItem[3]<-3
			}
			
			#... 4th quadrant....
			if(rowItem[1]<position["x"] & rowItem[2]>position["y"]){
			 # newItem<-cbind(rowItem,c(4))
			 rowItem[3]<-4
			}
		
			newdimReducedData<-rbind(newdimReducedData, rowItem)
		}
return(newdimReducedData)
}

getPredictedCluster<-function(position, testData){
		predictedClusters<- divideDataIntoQuadrant(position,testData)
	return(predictedClusters)
}

getRealClusteredDataForTrainPredictionModel<-function(dimReducedDataWithClusterInfo, clusterId){

	#reducedClusterData<-dimReducedDataWithClusterInfo[dimReducedDataWithClusterInfo[,3]==i]
	trainData<-c()
	for( i in 1: nrow(dimReducedDataWithClusterInfo)){
		newItem<-dimReducedDataWithClusterInfo[i,]
		
		if(newItem[3]==clusterId){
			d<-splittedData$trainset[i,]
			trainData<-rbind(trainData,d )
		}
	}
  
  return(trainData)
}

getRealClusteredDataForTestPredictionModel<-function(dimReducedDataWithClusterInfo, clusterId){

	#reducedClusterData<-dimReducedDataWithClusterInfo[dimReducedDataWithClusterInfo[,3]==i]
	testData<-c()
	for( i in 1: nrow(dimReducedDataWithClusterInfo)){
		newItem<-dimReducedDataWithClusterInfo[i,]
		
		if(newItem[3]==clusterId){
			d<-splittedData$testset[i,]
			testData<-rbind(testData,d )
		}
	}
  
  return(testData)
}
getPredictedDefects<-function(){

	csvData<-loadCsvFile(sourcePath[['mainFile']])
	data<-splitData(csvData)
	# 
	testData<-data$testset
	
	dataTrain<-getDataApplicableForDbScan(data$trainset)
	dataTest<-getDataApplicableForDbScan(data$testset)
	
	dbData<-getDbScanData(dataTrain)
	dbDataTest<-getDbScanData(dataTest)
	
	#dimReducedData<-getDimentionReducedData(dbData) 
	position<-getFirstQuadrantPosition(dbData)
	dimReducedDataWithClusterInfo<-divideDataIntoQuadrant(position,dbData)
	
	
	dimReducedpredictedClustersList<-getPredictedCluster(position,dbDataTest)

  numberOfClusters<-tail(sort(dimReducedpredictedClustersList[,3]),1)
  predictedDefects<-c()
  for(clster in 1: numberOfClusters){
  
  mainTrainData<-getRealClusteredDataForTrainPredictionModel(dimReducedDataWithClusterInfo,clster)
  testData<-getRealClusteredDataForTestPredictionModel(dimReducedpredictedClustersList,clster)
  
 model<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,mainTrainData)
 
 # writing model summary....................
 fileName<-paste("ModelSummaryForDimReducedWhereCluster",clster,sep="_")
 fileName<-paste(fileName,"csv",sep=".")
 writeModelSummary(model,sourcePath[['dataPath']],fileName)
 
 pDefect<-predict(model, testData,interval="predict")

 predictedDefects<-rbind(predictedDefects,pDefect)
  }

 combinedData<-getCombinedTestDataWithResidualValues(splittedData$testset,predictedDefects,"testDataWithResidualValuesForDimReducedWhereCluster.csv")
 writeDataFrameTo(combinedData,sourcePath[['dataPath']],"testDataWithResidualValuesForDimReducedWhereCluster.csv")
 computeResidualValue("testDataWithResidualValuesForDimReducedWhereCluster.csv","ResidualValForTimDimReducedWhereCluster.csv")
 return(predictedDefects)
}