
splitData<-function(myData){

## 75% of the sample size
smp_size <- floor(0.85 * nrow(myData))

## set the seed to make your partition reproductible
set.seed(1232343324)
train_ind <- sample(seq_len(nrow(myData)), size = smp_size)

train <- myData[train_ind, ]
test <- myData[-train_ind, ]
list(trainset=train, testset=test)
}