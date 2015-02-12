
library('fpc')
source('C:/Users/xman/Copy/R/Defect_Prediction/FileManager.R')
# get cluster info ...
getClustersUsingDbScan<-function(data,epsValue){
	dbcluster<-dbscan(data,eps=epsValue,method="raw"); 

	#predictedCluster<-predict(dbcluster,data,dataTobeClassified)
	return(dbcluster)
}
# predict cluster of the new data... 
getPredictedCluster<-function(data,eps,dataTobeClassified){

	dbcluster<-dbscan(data, eps, method="raw"); 

	predictedCluster<-predict(dbcluster,data,dataTobeClassified)
	return(predictedCluster)
}
#maindata and cluster info...
grouplingCsvFileUsingClusterInfo<-function(mainData, cluster){
	jointClusters<-c()
	jointClusterData<-c()
	numberOfCluster<-tail(sort(cluster$cluster),1)
	for (n in 0: numberOfCluster){
		DataIds<-mainData[cluster$cluster==n,]
		if(nrow(DataIds)>8){
		fileName <- paste(n,"csv", sep=".")
		writeDataFrameTo(DataIds,sourcePath[["dataPath"]],fileName)
		}else{
		jointClusterData<-rbind(jointClusterData,DataIds)
		jointClusters<-c(n,jointClusters)
		}
		
	}
	if(is.null(jointClusterData)==FALSE){
		writeDataFrameTo(jointClusterData,sourcePath[["dataPath"]],'11111.csv')
	}
	
	return(jointClusters)
}

getEpsValue<-function(data){
	model<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,data)
	intercept<-model$coefficients[1]*4
	if(intercept<0){
		intercept<-intercept*(-1)
	}
	return(intercept)
}
# generate eps value...
getEpsValueFromAverageDistanceOfEachPoint<-function(dbScanData){
distanceList<-c()
for(n in 1:(nrow(dbScanData)-1)){
	for(i in (n+1):(nrow(dbScanData)-1)){
		firstItem<-dbScanData[n,]
		secendItem<-dbScanData[i,]
		distance<-sqrt((firstItem[1]-secendItem[1])^2+(firstItem[2]-secendItem[2])^2)
	  distanceInfo<-c(firstItem[3],distance)
	  distanceList<-rbind(distanceList,distanceInfo)
	}
}
	#return( mean(distanceList[,2]))
return(distanceList)
}

