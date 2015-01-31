splitData<-function(myData){

## 75% of the sample size
smp_size <- floor(0.75 * nrow(myData))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(myData)), size = smp_size)

train <- myData[train_ind, ]
test <- myData[-train_ind, ]
list(trainset=train, testset=test)
}

prediction<-function(fileName){

data<-read.csv(fileName)

splits<-splitData(data)

model<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,splits$trainset)

pr<-predict(model, splits$testset, interval="none")
myDa<-data.frame(splits$testset$Id,pr)
#write.xlsx(myDa, "mydata.xlsx")
#write.table(myDa, "c:/mydata.txt", sep="\t")

write.csv2(myDa, file =fileName+"_prediction".csv",row.names=FALSE)


}
prediction("org.gjt.sp.jedit.gui.csv");