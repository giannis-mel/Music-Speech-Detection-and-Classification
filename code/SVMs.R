rm(list=ls())

#import libraries. Install them first if not installed (install.packages("..."))
library("e1071")
library("MLmetrics")
library("ROCR")
library("class")



#################SVMS##########################

data=read.table('myDataFinal.txt', header = TRUE ,sep = "\t",dec=".")

m=median(data$Centroid, na.rm=TRUE)
data$Centroid[is.na(data$Centroid)]=m

trainingset=data[1:6480,1:24]
Class=trainingset[,24]
xtest=data[6481:7680,1:23]
ytest=data[6481:7680,24]


### which kernel to use?############
#############linear ################
svm_model = svm(Class ~ ., kernel="linear", type="C-classification", data = trainingset,scale = TRUE)
pred = (predict(svm_model,xtest))
testing_error_linear = 1 - F1_Score(ytest,pred)
#########radial ######################
# Set of gamma values
gammavalues = c(0.001,0.01, 0.1,1)#10,100,1000,10000 den tis bazoyme gia na glitosoume xrono ektelesis efoson gnorizoume to apotelesma
# Calculate the training error
training_error = c()
for (gamma in gammavalues) {
  svm_model = svm(Class ~ ., kernel="radial", type="C-classification", data = trainingset, gamma = gamma,scale = TRUE)
  pred = predict(svm_model, trainingset[, c(1:23)])
  training_error = c(training_error, 1 - F1_Score(trainingset[,24], pred))
}

# Calculate the testing error
testing_error = c()
for (gamma in gammavalues) {
  svm_model = svm(Class ~ ., kernel="radial", type="C-classification", data = trainingset, gamma = gamma,scale = TRUE)
  pred = predict(svm_model,xtest)
  testing_error = c(testing_error, 1 - F1_Score(ytest, pred))
}

#plot errors on the same diagram
plot(training_error, type = "l", col="blue", ylim = c(0, 0.5), xlab = "Gamma", ylab = "Error", xaxt = "n")
axis(1, at = 1:length(gammavalues), labels = gammavalues)
lines(testing_error, col="red")
legend("top", c("Training Error", "Testing Error"), pch = c("-","-"),  col = c("blue", "red"))


########## sigmoid ###########
gammavalues = c(0.001,0.01, 0.1,1,10,100)
training_error = c()
for (gamma in gammavalues) {
  svm_model = svm(Class ~ ., kernel="sigmoid", type="C-classification", data = trainingset, gamma = gamma,scale = TRUE)
  pred = predict(svm_model, trainingset[, c(1:23)])
  training_error = c(training_error, 1 - F1_Score(trainingset[,24], pred))
}

# Calculate the testing error
testing_error = c()
for (gamma in gammavalues) {
  svm_model = svm(Class ~ ., kernel="sigmoid", type="C-classification", data = trainingset, gamma = gamma,scale = TRUE)
  pred = predict(svm_model,xtest)
  testing_error = c(testing_error, 1 - F1_Score(ytest, pred))
}

#plot errors on the same diagram
plot(training_error, type = "l", col="blue", ylim = c(0, 0.5), xlab = "Gamma", ylab = "Error", xaxt = "n")
axis(1, at = 1:length(gammavalues), labels = gammavalues)
lines(testing_error, col="red")
legend("right", c("Training Error", "Testing Error"), pch = c("-","-"),  col = c("blue", "red"))
title("Sigmoid kernel")

######################9-fold validation##############

#we get the not-normalized data again because we will normalize inside kfold
trainingset=data[1:6480,1:24]
xtest=data[6481:7680,1:23]
gammavalues = c(0.001,0.01, 0.1,1)
k = 9;
# Split in 9 folds so we got all the 60 packets of 1 song
dsize = nrow(trainingset)
#we dont use sample(1:dsize) because we want to get all 60 packets of 1 song and not packets of differrent songs
set.seed(0); folds = split(1:dsize, ceiling(seq(dsize) * k / dsize))
accuracies <- c()
#recalls <- c()
f1_scores<- c()
vars=c()
for (gamma in gammavalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select 8 out of 9 folds for training and 1 for validation
    trainingset2 <- trainingset[unlist(folds[-i]),]
    validationset <- trainingset[unlist(folds[i]),]
    # #unfactor so we can call myNormalization
    # #trainingset2$Class=unfactor(trainingset2$Class)
    # #validationset$Class= unfactor(validationset$Class)
    # sth=myNormalization(trainingset2,validationset)
    # trainingset2=sth$train
    # #calibrate the factor
    # trainingset2$Class=factor(trainingset2$Class)
    # validationset=sth$test
    # validationset$Class=factor(validationset$Class)
    # Train and apply the model
    svm_model = svm(Class ~ ., kernel="radial", type="C-classification", data = trainingset2, gamma = gamma,scale = TRUE)
    pred = predict(svm_model, validationset[, c(1:23)])
    # Save predictions and testsets
    predictions <- rbind(predictions, as.data.frame(pred))
    testsets <- rbind(testsets, as.data.frame(validationset[,24]))
  }
  # Calculate the new accuracy and add it to the previous ones
  accuracies = c(accuracies, Accuracy(predictions, testsets))
  f1_scores=c(f1_scores, F1_Score(unlist(testsets),unlist(predictions)))
  vars= c(vars,var(f1_scores))
}
# Find the best gamma value
print(accuracies)
print(vars)

plot(gammavalues,f1_scores,type="o",col="blue")
title("f1 scores per gamma")

bestgamma = gammavalues[which.max(f1_scores)]
sprintf(" So best gamma value=%f",bestgamma)

######Conclusion about gamma value and predictions #######

#blepoume ara kai apo ta training-testin error alla kai apo to 9-fold cross validation oti
# i kaliteri timi gia to gamma einai to gamma=0.1

#prediction on test set with bestgamma

svm_model = svm(Class ~., kernel="radial", type="C-classification", data = trainingset, gamma = bestgamma,scale = TRUE)
pred = predict(svm_model,xtest)
sprintf("Without PCA we got accuracy=%f and f1_score=%f ",Accuracy(pred,ytest),F1_Score(ytest,pred,positive = 1))



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

model3 = svm(Class ~., kernel="radial", type="C-classification", data = trainingset, gamma = bestgamma,scale = TRUE)
pred3=predict(model3,test3)


#Accuracy only for speach 
pred_speach=pred3[1:198]
ytest_speach=c(rep(0,198))
sprintf(" Accuracy only for speach = %f",Accuracy(pred_speach,ytest_speach))

#Accuracy only for music
pred_mus=pred3[199:length(pred3)]
ytest_mus=rep(1,162)
sprintf(" Accuracy only for music = %f",Accuracy(pred_mus,ytest_mus))

#Accuracy for both speach and music
ytest_new=c(ytest_speach ,ytest_mus)
sprintf(" Accuracy for both speach and music = %f",Accuracy(pred3,ytest_new))
sprintf(" F1_score for both speach and music = %f",F1_Score(pred3,ytest_new))




















# here is  a false attemp to figure out if we should use PCA or no based on the f1 scores and accuracies of predictions on test set
# # See how many PCs we need 
# svm_accuracies = c()
# svm_f1_scores = c()
# 
# for (i in 1:22){
#   #reconstruct the data
#   trainingset_pc <- as.data.frame(predict(pca_model,trainingset[,1:23])[, 1:i])
#   trainingset_pc[, (i+1):23] <- 0
#   trainingset_rec = data.frame(t(t(as.matrix(trainingset_pc) %*% t(pca_model$rotation)) * pca_model$scale + pca_model$center))
#   trainingset_rec[,24]=Class
#   xtest_pc = as.data.frame(predict(pca_model,xtest[,1:23])[, 1:i])
#   xtest_pc[, (i+1):23] <- 0
#   xtest_rec = data.frame(t(t(as.matrix(xtest_pc) %*% t(pca_model$rotation)) * pca_model$scale + pca_model$center))
#   #apply model
#   svm_model = svm(V24 ~., kernel="radial", type="C-classification", data = trainingset_rec, gamma = bestgamma,scale= TRUE)
#   pred = predict(svm_model,xtest_rec)
#   #save accuracies and f1_scores
#   svm_accuracies = c(svm_accuracies, Accuracy(pred,ytest))
#   svm_f1_scores=c(svm_f1_scores,F1_Score(ytest,pred,positive = 1))
# }
# 
# print(svm_accuracies)
# print(svm_f1_scores)
# 
