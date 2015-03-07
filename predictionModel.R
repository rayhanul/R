source('C:/Users/x-man/Copy/R/Defect_Prediction/multiplyCoefficient.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DbScanCluster.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DataInfo.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/ResidualValueAnalysis.R')
# predict defects using ... DBScan...and 
predictDefect<-function(){
	#loading a Csv File...
	csvData<-loadCsvFile(sourcePath[['mainFile']])
	data<-splitData(csvData)
	# 
	dataTrain<-getDataApplicableForDbScan(data$trainset)
		
	dbData<-getDbScanData(dataTrain)
  
	#eps<- getEpsValueFromAverageDistanceOfEachPoint(dbData)
	#eps<-getEpsValue(csvData)
	eps<-getEpsValueFromInterceptFromSelectingEach(csvData)
	eps<-3.856
	cluster<-getClustersUsingDbScan(dbData,eps)
	
	# storing each cluster...to file....
	jointClusterIds<-grouplingCsvFileUsingClusterInfo(data$trainset,cluster)
	#..............
	testData<-data$testset
	#testData<-loadCsvFile('C:/Users/xman/Copy/R/Data/testData.csv')
	testdbData<-getDbScanData(testData)
    #.............
	predictedCluster<-getPredictedCluster(dbData,eps,testdbData)
	numberOfCluster<-tail(sort(predictedCluster),1)
	residualValues<-c()
	for(i in 0:numberOfCluster){
	testDataIds<-testData[predictedCluster==i,]
		# joint cluster and simple clusters....
		if(is.element(i,jointClusterIds)==FALSE){
			trainDataFileName<-paste(i,"csv",sep='.')
			trainDataFileName<-paste(sourcePath[['dataPath']],trainDataFileName,sep='/')
		}else{
			trainDataFileName<-paste('11111',"csv",sep='.')
			trainDataFileName<-paste(sourcePath[['dataPath']],trainDataFileName,sep='/')
		}
		
	trainData<-read.csv(trainDataFileName)
	if(nrow(trainData)>1){
	predictionModel <- lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
	
	prdtedDefect<-predict(predictionModel,testDataIds,interval="predict")
	#adding cluster info...
	prdtedDefect<-addingClusterInfoToData(prdtedDefect,jointClusterIds,i)
	
	residualValues<-rbind(residualValues,prdtedDefect)
	}
	}
	combinedData<-	getCombinedTestDataWithResidualValues(testData,residualValues,"testDataWithResidualValuesForDbScan.csv")
	writeDataFrameTo(combinedData,sourcePath[['dataPath']],"testDataWithResidualValuesForDbScan.csv")
	computeResidualValue("testDataWithResidualValuesForDbScan.csv","ResidualValForDbScan.csv")
  return(residualValues)
}



