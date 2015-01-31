
library('fpc')
source('C:/Users/xman/Copy/R/FileManager.R')
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
	writeDataFrameTo(jointClusterData,sourcePath[["dataPath"]],'11111.csv')
	return(jointClusters)
}

getEpsValue<-function(){

}

