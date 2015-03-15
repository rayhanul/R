source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/HerederInfoManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/ResidualValueAnalysis.R')

data<-read.csv(sourcePath["mainFile"])

splittedData<-splitData(data)


getDimentionReducedData<-function(splitteTrainData){

		fit<-princomp(splitteTrainData[3:10],cor = TRUE)
		dimReducedData<-cbind(fit$scores[,1],fit$scores[,2])
	return(dimReducedData)
}

#getlargestAndSmallestItem<-function(dimReducedData){

	#	for ( 1 in 0: length(dimReducedData)){
			#item<-dimReducedData[i,]
			#dis<- sqrt((item[,1])^2+(item[,2])^2)
			#dimReducedData[i,1]<-cbind(item,dis)
		#}
		#smallest<-tail(sort(dimReducedData[,3]),1)

		#largest<-head(sort(dimReducedData[,3]),1)
		
		
	#return(list(largest=largest,smallest=smallest))

#}


getFirstQuadrantPosition<-function(dimReducedData){

#largestAndSmallest<-getlargestAndSmallestItem(splittedData$trainset)

x<-median(dimReducedData[,1])
	y<-median(dimReducedData[,2])
	
	return(list(x=x,y=y))

}


divideDataIntoQuadrant<-function(dataTobeReduced){

	dimReducedData<-getDimentionReducedData(dataTobeReduced)
	position<-getFirstQuadrantPosition(dimReducedData)
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

getPredictedCluster<-function(testData){
		predictedClusters<- divideDataIntoQuadrant(testData)
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

	#dimReducedData<-getDimentionReducedData(splittedData$trainset) 
	#position<-getFirstQuadrantPosition(dimReducedData)
	dimReducedDataWithClusterInfo<-divideDataIntoQuadrant(splittedData$trainset)
	
	
	dimReducedpredictedClustersList<-getPredictedCluster(splittedData$testset)

  numberOfClusters<-tail(sort(dimReducedpredictedClustersList[,3]),1)
  predictedDefects<-c()
  for(clster in 1: numberOfClusters){
  
  mainTrainData<-getRealClusteredDataForTrainPredictionModel(dimReducedDataWithClusterInfo,clster)
  testData<-getRealClusteredDataForTestPredictionModel(dimReducedpredictedClustersList,clster)
  
 model<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,mainTrainData)
 
 # writing model summary....................
 fileName<-paste("ModelSummaryForTimMenzies",clster,sep="_")
 fileName<-paste(fileName,"csv",sep=".")
 writeModelSummary(model,sourcePath[['dataPath']],fileName)
 
 pDefect<-predict(model, testData,interval="predict")
 #dataFrame<-pDefect.frame()
 predictedDefects<-rbind(predictedDefects,pDefect)
  }
  #writeDataFrameTo(predictedDefects,sourcePath[['PcaAnalysisPath']],"residualTimMenzies")
 combinedData<-getCombinedTestDataWithResidualValues(splittedData$testset,predictedDefects,"testDataWithResidualValuesForMenzies.csv")
 writeDataFrameTo(combinedData,sourcePath[['dataPath']],"testDataWithResidualValuesForMenzies.csv")
 #............computing residual values...............
 #tempData<-correctingheaderInfo(sourcePath[['dataPath']],"testDataWithResidualValuesForMenzies.csv")
 #writeDataFrameWithOutRowIdColumn(tempData,sourcePath[['dataPath']],"testDataWithResidualValuesForMenzies.csv")
 computeResidualValue("testDataWithResidualValuesForMenzies.csv","ResidualValForTimMenzies.csv")
 return(predictedDefects)
}