
library('fpc')
source('C:/Users/x-man/Copy/R/Defect_Prediction/FileManager.R')
# get cluster info ...
getClustersUsingDbScan<-function(data,epsValue){
	dbcluster<-dbscan(data,eps=epsValue,method="raw"); 

	#predictedCluster<-predict(dbcluster,data,dataTobeClassified)
	return(dbcluster)
}
# predict cluster of the new data... 
getPredictedCluster<-function(data,epsValue,dataTobeClassified){

	dbcluster<-dbscan(data, eps=epsValue, method="raw"); 

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

getEpsValueFromInterceptFromSelectingEach<-function(data){
	modelWmc<-lm(bug~wmc,data)
	modelLoc<-lm(bug~loc,data)
	modelNpm<-lm(bug~npm,data)
	modelCbo<-lm(bug~cbo,data)
	modelLcom<-lm(bug~lcom,data)
	modelRfc<-lm(bug~rfc,data)
	modelDit<-lm(bug~dit,data)
	modelNoc<-lm(bug~noc,data)
eps<-modelWmc$coefficients[1]+modelLoc$coefficients[1]+modelNpm$coefficients[1]+modelCbo$coefficients[1]+modelLcom$coefficients[1]+modelRfc$coefficients[1]+modelDit$coefficients[1]+modelNoc$coefficients[1]
return(eps)
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

