
writeDataFrameTo<-function(data,directory,fileName){
   names(data)<-c("resVal","id")
   filePath<-paste(directory,fileName,sep="/")
   write.csv(data, file =filePath)
}



writeDataFrameWithOutRowIdColumn<-function(data,directory,fileName){
  filePath<-paste(directory,fileName,sep="/")
  write.csv(data, file =filePath,row.names=FALSE)
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
 return(combinedData)
}
writeModelSummary<-function(predictionModel,directory,fileName){
	x <- cbind(t(as.numeric(coefficients(predictionModel))), t(as.numeric(summary(predictionModel)$coefficients[, 4])), summary(predictionModel)$r.squared)
	d<-data.frame(x)
    names(d) <- c(paste("coeff", names(coefficients(predictionModel))), paste("P-value", names(summary(predictionModel)$coefficients[, 4])), "R-squared")
  
	filePath<-paste(directory,fileName,sep="/")
	write.csv(d, file =filePath)
}