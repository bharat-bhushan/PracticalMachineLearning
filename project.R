training.file <- 'pml-training.csv'
test.file     <- 'pml-test.csv'
training.url  <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
test.url      <- 'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
download.file(training.url, training.file, method='wget')
download.file(test.url,test.file,method='wget')
read.pml<- function(x) {read.csv(x, na.strings = c("", "NA", "#DIV/0!"))}
training<- read.pml(training.file)
test<- read.pml(test.file)
names(training)
names(test)
training<- training[,-c(1,3,4,5,6,7)]
test<- test[,-c(1,3,4,5,6,7,160)]
#install.packages("caret")
#install.packages("randomForest")
#install.packages('e1071', dependencies=TRUE)
library(caret)
library(randomForest)
library(knitr)
inTrain<-createDataPartition(training$classe, p=.50, list=FALSE)
training.train<-training[ inTrain,]
training.test<-training[-inTrain,]
#Next we create a function to remove entire NA columns, and apply it to both data frames. Lastly we create a function that removes any variables with missing NAs and apply this to the training set.
rm.na.cols<- function(x) { x[ , colSums( is.na(x) ) < nrow(x) ]}
training.train <- rm.na.cols(training.train)
training.test  <- rm.na.cols(training.test)
complete<- function(x) {x[,sapply(x, function(y) !any(is.na(y)))] }
incompl<- function(x) {names( x[,sapply(x, function(y) any(is.na(y)))] ) }
trtr.na.var<- incompl(training.train)
trts.na.var<- incompl(training.test)
training.train<- complete(training.train)
training.test<- complete(training.test)
modFit<-train(classe~.,training.train,method='rf')
summary(modFit)
confusionMatrix(predict(modFit,newdata=training.test[,-54]),training.test$classe)


#Prediciting with Testing Dataset
predictions <- as.character(predict(modFit,newdata=test))
print(predictions)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions)
