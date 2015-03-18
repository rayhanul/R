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
	testData<-data$testset
	
	dataTrain<-getDataApplicableForDbScan(data$trainset)
		
	dbData<-getDbScanData(dataTrain)
  
	#eps<- getEpsValueFromAverageDistanceOfEachPoint(dbData)
	#eps<-getEpsValue(csvData)
	eps<-getEpsValueFromInterceptFromSelectingEach(csvData)
	eps<-3.856
	cluster<-getClustersUsingDbScan(dbData,eps)
	numberOfMainClusters<-tail(sort(cluster$cluster),1)
	if(numberOfMainClusters!=0){
		# storing each cluster...to file....
		jointClusterIds<-grouplingCsvFileUsingClusterInfo(data$trainset,cluster)
		#..............
		
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
			#if(file.exists(trainDataFileName)){
				trainData<-read.csv(trainDataFileName)
			if(nrow(trainData)>10){
				predictionModel <- lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,trainData)
				
				# writing model summary....................
				fileName<-paste("ModelSummaryForDBScan",i,sep="_")
				fileName<-paste(fileName,"csv",sep=".")
				writeModelSummary(predictionModel,sourcePath[['dataPath']],fileName)
				
				prdtedDefect<-predict(predictionModel,testDataIds,interval="predict")
				#adding cluster info...
				prdtedDefect<-addingClusterInfoToData(prdtedDefect,jointClusterIds,i)
				
				residualValues<-rbind(residualValues,prdtedDefect)
			}
		#	}
		combinedData<-	getCombinedTestDataWithResidualValues(testData,residualValues,"testDataWithResidualValuesForDbScan.csv")
		writeDataFrameTo(combinedData,sourcePath[['dataPath']],"testDataWithResidualValuesForDbScan.csv")
		resFile<-paste("ResidualValForDbScan",i,sep="_")
		resFile<-paste(resFile,"csv",sep=".")
		computeResidualValue("testDataWithResidualValuesForDbScan.csv",resFile)
		}
	}
#   else{
# 		predictionModel <- lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,data$trainset)
# 		
# 		# writing model summary....................
# 		fileName<-paste("ModelSummaryForDBScan","csv",sep=".")
# 		writeModelSummary(predictionModel,sourcePath[['dataPath']],fileName)
#     
#     
# 		prdtedDefect<-predict(predictionModel,data$testset,interval="predict")
# 		#adding cluster info...
# 		residualValues<-addingClusterInfoToData(prdtedDefect,jointClusterIds,0)
# 	}
	
  return(combinedData)
}



