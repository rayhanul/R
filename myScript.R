data<-read.csv("jedit-3.2.csv")
x<-data
set.seed(10)
#split(x, sample(rep(1:2, 13)))

splitdf <- function(dataframe, seed=NULL) {
    if (!is.null(seed)) set.seed(seed)
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/2))
	
	trainset <- dataframe[trainindex, ]
	testset <- dataframe[-trainindex, ]
	list(trainset=trainset, testset=testset)
}

#split data set into 75 for train and 25 for test 

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


splits <- splitdf(data, seed=808)
splits<-splitData(data)

model<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,splits$trainset)
model<-lm(bug~wmc+loc+npm+cbo+lcom+rfc+noc+dit,data)
predict(model, splits$testset, interval="predict")

predict(model, splits$testset, interval="confidence")
predict(model, splits$testset, interval="none")

