
writeDataFrameTo<-function(data,directory,fileName){
filePath<-paste(directory,fileName,sep="/")
write.csv(data, file =filePath)
}

getCombinedTestDataWithResidualValues<-function(testData,residualValues,fileName){
combinedData<-c()
	for(n in 1:nrow(residualValues)){
		rowResidualItem<-residualValues[n,]
		fit<-rowResidualItem[1]
		clster<-rowResidualItem[4]
		rowTestItem<-testData[n,]
		rowItem<-cbind(rowTestItem,fit,clster)
		combinedData<-rbind(combinedData,rowItem)
	}
writeDataFrameTo(combinedData,sourcePath[['dataPath']],fileName)
}


