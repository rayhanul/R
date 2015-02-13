sourcePath<-c(mainFile="",predictedFile="")



computeResidualValue<-function(){

	mainData<-read.csv(sourcePath[["mainFile"]])
	predictedData<-read.csv(sourcePath[["predictedFile"]])
	allResidualValues<-c()
	for(i in 1:nrow(predictedData)){
	rowResidualValue<-c()
		for(j in 1:nrow(mainData)){
			if(predictedData[["id"]]==mainData[["id"]]){
				rowResidualValue<-abs(predictedData[["fit"]]-mainData[["bug"]])
				rowResidualValue<-cbind(rowResidualValue,predictedData[["id"]])
			}
		}
		allResidualValues<-rbind(allResidualValues,rowResidualValue)
	}
}
