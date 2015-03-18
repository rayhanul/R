
path="C:/Users/x-man/Copy/R/Data/Ant-1.3"

getComputedFinalResults<-function(){

	filesList<-list.files(path,pattern = "^ResidualValFor")
	allData<-c()
	for( eachFile in 1: length(filesList)){
    
		fileName<-paste(path,filesList[eachFile],sep="/")
		csvData<-read.csv(fileName)
		
		sumVal<-sum(csvData[["resVal"]])
		meanVal<-mean(csvData[["resVal"]])
		medianVal<-median(csvData[["resVal"]])
		stdVal<-sd(csvData[["resVal"]])
		
		rowData<-c(filesList[eachFile],sumVal,meanVal,medianVal,stdVal)
		allData<-rbind(allData,rowData)
	}
  allData<-data.frame(allData)
  names(allData)<-c("Clustering approach","sum","mean","median","stdVal")
return(allData)
}