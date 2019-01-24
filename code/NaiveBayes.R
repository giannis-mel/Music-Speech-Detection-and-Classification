rm(list=ls())

#import libraries. Install them first if not installed (install.packages("..."))
library("e1071")
library("MLmetrics")
library("ROCR")
library("class")
library("dmm")
library("scatterplot3d")

#standarization not min-max
myNormalization <- function(train, test){
  # Get the min values for each column
  min = sapply(train,function(x)(mean(x)))
  # Get the max - min values for each column
  denominator = sapply(train, function(x)(sd(x)))
  
  # Creating the new data frames
  train_normalized = data.frame(matrix(nrow = nrow(train), ncol =  ncol(train)))
  test_normalized = data.frame(matrix(nrow = nrow(test), ncol =  ncol(test)))
  
  # Normalize the training set
  for(i in 1:ncol(train)){
    vec = (train[,i] - min[i])/denominator[i]
    train_normalized[i] = vec
  }
  
  
  # Normalize the test set 
  for(i in 1:ncol(test)){
    vec = (test[,i] - min[i])/denominator[i]
    test_normalized[i] = vec
  }
  
  # Add names to columns
  colnames(train_normalized) = colnames(train)
  colnames(test_normalized) = colnames(test)
  
  print("Normalization completed !")
  
  # Return the normalized data frames
  return(list(train = train_normalized, test = test_normalized))
}

####### Naive Bayes without normalization###############

#read txt file as table
data=read.table('myDataFinal.txt', header = TRUE ,sep = "\t",dec=".")
data$Class=factor(data$Class)
#check the data for NAN values
#options(max.print = 1000000)
#is.na.data.frame(data) 
#or is.na(data$column) for every column if you cant see the whole dataset and then
# for (i in 1:7680){
#   if (x[i]==TRUE){
#     print(i)
#   }
# }


#NAN values replaced with median of that column
m=median(data$Centroid, na.rm=TRUE)
data$Centroid[is.na(data$Centroid)]=m

#Split the data to sets
trainingset=data[1:6480,1:24]
Class=trainingset[,24]
xtest=data[6481:7680,1:23]
ytest=data[6481:7680,24]

####Naive Bayes######

#apply Naive Bayes model
model = naiveBayes(Class ~. ,data=trainingset,laplace = 1)
#predictions and plot Roccurve
pred1=predict(model,xtest)
predprob=predict(model,xtest,type="raw")
pred_obj = prediction(predprob[,1], ytest, label.ordering = c(0,1))

#metrics for not-normalized model
ROCcurve <- performance(pred_obj, "tpr", "fpr")
perf1=performance(pred_obj, "auc")
ac1=Accuracy(pred1,ytest)
rec=Recall(ytest,pred1,positive = 1)
prec=Precision(ytest,pred1,positive = 1)
f1=F1_Score(ytest,pred1)

#training error
pred = predict(model, trainingset[, c(1:23)])
training_error1 = 1 - Accuracy(trainingset[,24], pred)
#testing error
pred = predict(model, xtest)
testing_error1 = 1 - Accuracy(ytest, pred)

sprintf("From Naive Bayes without normalization we got accuracy=%f , F1 score=%f , training eror=%f and testing error=%f",ac1,f1,training_error1,testing_error1)

###################Naive Bayes with normalization#################################
###parakatw apodiknioumai mesw peiramatos oti 8a exoume ta idia apotelesmata me eite xwris normalize
data=read.table('myDataFinal.txt', header = TRUE ,sep = "\t",dec=".")
# #NAN values replaced with median of that column
m=median(data$Centroid, na.rm=TRUE)
data$Centroid[is.na(data$Centroid)]=m


#Get metrics of normalized model
trainingset=data[1:6480,1:24]
Class=trainingset[,24]
xtest=data[6481:7680,1:23]
ytest=data[6481:7680,24]

#normalize data
norm_data=myNormalization(trainingset,xtest)
#and adjust the factor
trainingset=norm_data$train
trainingset$Class=factor(trainingset$Class)
xtest=norm_data$test

model2 = naiveBayes(Class ~. ,data=trainingset,laplace =1)
pred2=predict(model2,xtest)
predprob2=predict(model2,xtest,type="raw")
pred_obj2 = prediction(predprob2[,1], ytest, label.ordering = c(0,1))
ROCcurve2 <- performance(pred_obj2, "tpr", "fpr")
per2=performance(pred_obj2, "auc")

ac2=Accuracy(pred2,ytest)
rec2=Recall(ytest,pred2,positive = 1)
prec2=Precision(ytest,pred2,positive = 1)
f1_2=F1_Score(ytest,pred2)

#training error
pred = predict(model2, trainingset[, c(1:23)])
training_error2 = 1 - Accuracy(trainingset[,24], pred)
#testing error
pred = predict(model2, xtest)
testing_error2 = 1 - Accuracy(ytest, pred)

plot(ROCcurve, col = "blue")
plot(ROCcurve2, col = "red", add = TRUE)
abline(0,1, col = "grey")
title("ROC Curve for Bayes with or without normalization")
sprintf("From Naive Bayes with normalization we got accuracy=%f , F1 score=%f , training eror=%f and testing error=%f",ac2,f1_2,training_error2,testing_error2)

sprintf("As we can see the metrics are all the same with or without normalization")
###################Conclusion about normalization#################
#blepoume idia ola ta metrics eite me normalization(idia itan kai me tin xrisi scale) 
#eite xwris ston Naive Bayes poy ofeilete stin fisi tou algori8mou(logw ipologismou pi8anotitwn kai oxi euclidian distance)

##########Test for boundaries in a music-speach file###########

#read txt file as table
data=read.table('myDataFinal.txt', header = TRUE ,sep = "\t",dec=".")
data$Class=factor(data$Class)


#NAN values replaced with median of that column
m=median(data$Centroid, na.rm=TRUE)
data$Centroid[is.na(data$Centroid)]=m

#Split the data to sets
trainingset=data[1:6480,1:24]
Class=trainingset[,24]
sth2=colnames(data[1:23])

test3=read.table('test3.txt', header = TRUE ,sep = "\t",dec=".")
test3=as.data.frame(test3)
colnames(test3)=sth2
#NAN values replaced with median of that column
m=median(test3$Centroid, na.rm=TRUE)
test3$Centroid[is.na(test3$Centroid)]=m
#test3=na.omit(test3)


model3 = naiveBayes(Class ~. ,data=trainingset,laplace =1)
pred3=predict(model3,test3)

  
#Accuracy only for speach 
pred_speach=pred3[1:198]
ytest_speach=rep(0,198)
sprintf(" Accuracy only for speach = %f",Accuracy(pred_speach,ytest_speach))

#Accuracy only for music
pred_mus=pred3[199:length(pred3)]
ytest_mus=rep(1,162)
sprintf(" Accuracy only for music = %f",Accuracy(pred_mus,ytest_mus))

#Accuracy for both speach and music
ytest_new=c(ytest_speach ,ytest_mus)
sprintf(" Accuracy for both speach and music = %f",Accuracy(pred3,ytest_new))
sprintf(" F1_score for both speach and music = %f",F1_Score(pred3,ytest_new))

