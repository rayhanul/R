
library('fpc')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
# get cluster info ...
getClustersUsingDbScan<-function(data,eps){
	dbcluster<-dbscan(data,eps); 

	#predictedCluster<-predict(dbcluster,data,dataTobeClassified)
	return(dbcluster)
}
# predict cluster of the new data... 
getPredictedCluster<-function(data,eps,dataTobeClassified){

	dbcluster<-dbscan(data,eps); 

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
	if(nrow(jointClusterData)>1){
		writeDataFrameTo(jointClusterData,sourcePath[["dataPath"]],'11111.csv')
	}
	
	return(jointClusters)
}

getEpsValue<-function(dbScanData){

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

