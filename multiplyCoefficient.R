
CodeMetricsStdVal<- c(cbo=9,wmc=20 , rfc=50, loc=60, lcom=2,npm=7,dit=5,noc=6 )
# get coefficient value from the existing data....
getCoefficientFromAvailableData<-function(data){

predictionModel <- lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,data)
return(predictionModel$coefficients)
}

# multiply coefficients value...
getDataBymultiplingCoefficientValue<-function(data,coff){
	newMatrix<-c()
	for (n in 1:nrow(data)){
		c<-data[n,]
		c[["wmc"]]<-c[["wmc"]]*coff[["wmc"]]
		c[["dit"]]<-c[["dit"]]*coff[["dit"]]
		c[["noc"]]<-c[["noc"]]*coff[["noc"]]
		c[["cbo"]]<-c[["cbo"]]*coff[["cbo"]]
		c[["rfc"]]<-c[["rfc"]]*coff[["rfc"]]
		c[["lcom"]]<-c[["lcom"]]*coff[["lcom"]]
		c[["npm"]]<-c[["npm"]]*coff[["npm"]]
		c[["loc"]]<-c[["loc"]]*coff[["loc"]]
		newMatrix<-rbind(newMatrix,c)
	}
	#newMatrix[-1,]
	return(newMatrix)
}

#compare data with standard value...
compareDataWithStandardValue<-function(data,stdValue){

	newMatrix<-c()
	
	for (n in 1:nrow(data)){
		c<-data[n,]
		c[["wmc"]]<- 100- (c[["wmc"]]/stdValue[["wmc"]])*100
		c[["dit"]]<- 100- (c[["dit"]]/stdValue[["dit"]])*100
		c[["noc"]]<- 100- (c[["noc"]]/stdValue[["noc"]])*100
		c[["cbo"]]<- 100- (c[["cbo"]]/stdValue[["cbo"]])*100
		c[["rfc"]]<- 100- (c[["rfc"]]/stdValue[["rfc"]])*100
		c[["lcom"]]<- 100-(c[["lcom"]]/stdValue[["lcom"]])*100
		c[["npm"]]<- 100- (c[["npm"]]/stdValue[["npm"]])*100
		c[["loc"]]<- 100- (c[["loc"]]/stdValue[["loc"]])*100
		
		newMatrix<-rbind(newMatrix,c)
	}
	
	#newMatrix[-1,]
	
	return(newMatrix)
} 
# partioning data based positive impact and negative impact...
partitionDataBasedOnSign<-function(data){
	newMatrix<-c()
	for(n in 1:nrow(data)){
		rowItem<-data[n,]
		nPos<-0
		nNeg<-0
		for(i in 3:(ncol(rowItem)-1)){
			if(is.finite(rowItem[[i]])){
				if(rowItem[[i]]>=0){
					nPos<-nPos+rowItem[[i]]
				}else{
					nNeg<-nNeg-rowItem[[i]]
				}		
			}
		}
		newMatrix<-rbind(newMatrix,c(rowItem[["id"]],nPos,nNeg,rowItem[["bug"]]))		
	}
	return(newMatrix)
}
# loading all csv files and make a combined data frame...
loadAllCsvFile<-function(path){

allFiles<-list.files(path)
combindedData<-c()
	for(i in 1: length(allFiles)){
	combindedPath<-paste(path,allFiles[[i]],sep="/")
		data<-read.csv(combindedPath)
		multipliedData<-compareDataWithStandardValue(data,CodeMetricsStdVal)
		partitionedData<-partitionDataBasedOnSign(multipliedData)
		combindedData<-rbind(combindedData,partitionedData)
	}
return(combindedData)
}


getDataApplicableForDbScan<-function(data){
	combindedData<-c()
	# using comparison with standard value ...
	#unsignedData<-compareDataWithStandardValue(data,CodeMetricsStdVal)
	
	# using coefficient values...
	coeff<-getCoefficientFromAvailableData(data)
	unsignedData<-getDataBymultiplingCoefficientValue(data,coeff)
	
	
	partitionedData<-partitionDataBasedOnSign(unsignedData)
	combindedData<-rbind(combindedData,partitionedData)
	
return(combindedData)
}


loadCsvFile<-function(filepath){
	data<-read.csv(filepath)
	return(data)
}
getDbScanData<-function(data){

	db<-cbind(data[,2],data[,3],data[,1])
	return(db)
}

