set.seed(777)

#Required libraries
library(caret)
library(randomForest)

##Input datasets

mydata <- read.table("c:/Data/pml-training.csv", header=TRUE, sep=",",row.names = "X")
validation <- read.table("c:/Data/pml-testing.csv", header=TRUE, sep=",",row.names = "X")


#Removing non predictive variables var1..var6 for all datasets
mydata <- mydata[,-c(1:6)]
validation<-validation[,-c(1:6)]


#Removing variables with 90% or more missing values
x <- sapply(mydata, function(x) mean(is.na(x))) > 0.9
  mydata <- mydata[, x==FALSE]
  validation <- validation[, x==FALSE]

#transform to numeric
 
  mydata[,-ncol(mydata)] <- apply(mydata[,-ncol(mydata)], 2, function(x){as.numeric(as.character(x))})

  
#Removing again variables with 90% or more missing values 

  x <- sapply(mydata, function(x) mean(is.na(x))) > 0.9
    mydata <- mydata[, x==FALSE]
    validation <- validation[, x==FALSE]
  

# Train and test samples

inTrain  <- createDataPartition(mydata$classe, p=0.7, list=FALSE)
train <- mydata[inTrain, ]
test  <- mydata[-inTrain, ]

# Prediction 
# Accuracy = 0.9943925 

fit1<-randomForest(train$classe ~ .,   data=train, do.trace=F, ntree=50)
pred1<-predict(fit1, newdata = test)
confusionMatrix(pred1,test$classe)$overall[1]


# Results

Results <- predict(fit1, newdata=validation)
Results




