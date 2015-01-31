
source('C:/Users/x-man/Copy/R/Defect_Prediction/multiplyCoefficient.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/DbScanCluster.R')
source('C:/Users/x-man/Copy/R/Defect_Prediction/datasetManager.R')

sourcePath <- c(filePath="C:/Users/x-man/Copy/R/Defect_Prediction/Data/jedit-3.2.csv", dataPath="C:/Users/x-man/Copy/R/Defect_Prediction/Data")
dbScanParam<-c(eps=4.3434)
# predict defects using ... DBScan...and 
predictDefect<-function(){
	#loading a Csv File...
	csvData<-loadCsvFile(sourcePath[['filePath']])
	data<-splitData(csvData)
	# 
	dataTrain<-getDataApplicableForDbScan(data$trainset)
		
	dbData<-getDbScanData(dataTrain)
  
	eps<- getEpsValueFromAverageDistanceOfEachPoint(dbData)
	
  cluster<-getClustersUsingDbScan(dbData,dbScanParam[['eps']])
	
	# storing each cluster...to file....
	jointClusterIds<-grouplingCsvFileUsingClusterInfo(data$trainset,cluster)
	#..............
	testData<-data$testset
#	testData<-loadCsvFile('C:/Users/x-man/Copy/R/Defect_Prediction/Data/testData.csv')
	testdbData<-getDbScanData(testData)
    #.............
	predictedCluster<-getPredictedCluster(dbData,dbScanParam[['eps']],testdbData)
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
	
	residualValues<-rbind(residualValues,prdtedDefect)
	}
	}
	getCombinedTestDataWithResidualValues(testData,residualValues,"testDataWithResidualValues.csv")
	
  return(residualValues)
}