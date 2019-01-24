##### (0)Preprocessing #####

# For writing to txt
# write.table(normalized, file = "norm.txt", append = FALSE, quote = FALSE, sep = "\t",eol = "\n", dec = ".", row.names = FALSE,col.names = TRUE)

# Normalize the data (min-max Normalization)
# Used : https://en.wikipedia.org/wiki/Feature_scaling
# Useful : https://machinelearningmastery.com/pre-process-your-dataset-in-r/
myNormalization <- function(train, test){
  # Get the min values for each column
  min = sapply(train,function(x)(min(x)))
  # Get the max - min values for each column
  denominator = sapply(train, function(x)(max(x)-min(x)))

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



# Reading data
data = read.table('myDataFinal.txt',header = TRUE, sep = '\t', dec = '.')

# Nan values appear only in Centroid
# Replaze NaN values with median value of column Centroid
m = median(data$Centroid, na.rm = TRUE)
data$Centroid[is.na(data$Centroid)] = m

# Separate training and test data(80-20 split)
train = data[1:6120,]
test = data[6121:7680,]

# Normalize train and test data
myList = myNormalization(train, test)
train = myList$train
test = myList$test
#####(1)kNN ######
# Used : https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
#		 https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/kNN

library(class) 
library(MLmetrics)

# Now seperate the train and test data, 80-20 split
train_attributes = train[,1:23]
train_class = train[,24]
test_attributes = test[,1:23]
test_class = test[,24]

# Predict and plot accuracies for different k
x = c()
for(i in 1:100){
  knn_predictions = knn(train_attributes,test_attributes,train_class,k=i)
  acc = Accuracy(knn_predictions,test_class)
  x = c(x,acc)
  #cat(sprintf("The accuracy for k = %d is %2f .\n",i,acc))
}

plot(x, type = "l", col="blue",  xlab = "K", ylab = "Accuracy", xaxt = "n", main = "Accuracies")
axis(1)
# useful function : CrossTable(x = knn_predictions,y = test_class, prop.chisq = FALSE)




# K-fold validation
# Separate training and test data(85-15 split)
train = data[1:6480,]
test = data[6481:7680,]

rm(x)
x = c()
y = c()
k = 9  
size = nrow(train)
set.seed(0); folds = split((1:size), ceiling(seq(size) * k / size))
accuracies = c()
for(j in 1:20){
  rm(myList)
  accuracies = c()
  predictions = data.frame()
  testsets = data.frame()
  for(i in 1:k){
    # Select 8 out of 9 folds for training and 1 for validation
    trainingset <- train[unlist(folds[-i]),]
    validationset <- train[unlist(folds[i]),]
	# Normalize the data
	myList = myNormalization(trainingset, validationset)
	trainingset = myList$train
	validationset = myList$test
    training_attributes = trainingset[,1:23]
    training_class = trainingset[,24]
    validation_attributes = validationset[,1:23]
    validation_class = validationset[,24]
    # Train and apply the model
    knn_predictions = knn(training_attributes,validation_attributes,training_class,k=j)
    # Save predictions and testsets
    predictions <- rbind(predictions, knn_predictions)
    testsets <- rbind(testsets, validation_class)
    accuracies = rbind(accuracies,Accuracy(knn_predictions, validation_class))
  }
  x = c(x, mean(accuracies))
  y = c(y, var(accuracies))
  print(j)
  print(accuracies)
  #cat(sprintf("The mean accuracy for 8-fold cv, for k = %d is %2f .\n",i,x))
}

plot(x, type = "l", col="blue",  xlab = "K", ylab = "Accuracy", xaxt = "n", main = "9-fold Accuracies")
axis(1)
plot(y, type = "l", col="blue",  xlab = "K", ylab = "Variance", xaxt = "n", main = "Variance of 9-fold Accuracies")
axis(1)

# The best accuracy is max(x) that matches with K = 5
# So we pick K = 5 for the training of the algorithm
# Plot training and test MSE 
# Separate training and test data(80-20 split)
train = data[1:6120,]
test = data[6121:7680,]
myList = myNormalization(train, test)
train = myList$train
test = myList$test
training_error = c()
test_error = c()

# Training error
for (j in 1:20){
  knn_predictions = knn(train = training_attributes,test = training_attributes,cl = training_class,k = j)
  knn_predictions2 = as.numeric(knn_predictions)
  for(i in 1:length(knn_predictions2)){
    if(knn_predictions2[i]==2){
      knn_predictions2[i] = 1
    }
    else{
      knn_predictions2[i] = 0
    }
  }
  training_error = rbind(training_error, mean((knn_predictions2-training_class)^2) )
}

# Test error
for (j in 1:20){
  knn_predictions = knn(train = training_attributes,test = test_attributes,cl = training_class,k = j)
  knn_predictions2 = as.numeric(knn_predictions)
  for(i in 1:length(knn_predictions)){
    if(knn_predictions2[i]==2){
      knn_predictions2[i] = 1
    }
    else{
      knn_predictions2[i] = 0
    }
  }
  test_error = rbind(test_error, mean((knn_predictions2-test_class)^2))
}

plot(training_error, type = "l", col="blue", xlab = "K", ylab = "Error", xaxt = "n",asp = 15)
axis(1, at = 1:20, labels = 1:20)
lines(test_error, col="red")
legend("bottomright", c("Training Error", "Testing Error"), pch = c("-","-"),  col = c("blue", "red"))
# In this plot we see the U-shape in K = 5 

train = data[1:6480,]
test = data[6481:7680,]
myList = myNormalization(train, test)
train = myList$train
test = myList$test

train_attributes = train[,1:23]
train_class = train[,24]
test_attributes = test[,1:23]
test_class = test[,24]

# Running the model with K = 5 returns 0.84 accuracy
knn_predictions = knn(train_attributes,test_attributes,train_class,k=5)
acc = Accuracy(knn_predictions,test_class)


# Test on mySong.wav
sth2=colnames(data[1:23])
train = data[1:6480,]
test3=read.table('test3.txt', header = TRUE ,sep = "\t",dec=".")
test3=as.data.frame(test3)
colnames(test3)=sth2
#NAN values replaced with median of that column
m=median(test3$Centroid, na.rm=TRUE)
test3$Centroid[is.na(test3$Centroid)]=m
#test3=na.omit(test3)

myList = myNormalization(train, test3)
train = myList$train
test3 = myList$test

# train the model
knn_predictions3 = knn(train[,1:23],test3,train[,24],k=5)


#Accuracy only for speach 
pred_speach=knn_predictions3[1:198]
ytest_speach=rep(0,198) # 198 zeros
sprintf(" Accuracy only for speach = %f",Accuracy(pred_speach,ytest_speach))

#Accuracy only for music
pred_mus=knn_predictions3[199:length(knn_predictions3)]
ytest_mus=rep(1,162)
sprintf(" Accuracy only for music = %f",Accuracy(pred_mus,ytest_mus))

#Accuracy for both speach and music
ytest_new=c(ytest_speach ,ytest_mus)
sprintf(" Accuracy for both speach and music = %f",Accuracy(knn_predictions3,ytest_new))
sprintf(" F1_score for both speach and music = %f",F1_Score(knn_predictions3,ytest_new))









