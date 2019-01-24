# Algorithm that classifies files of GTZAN dataset using KNN and SVMs results.
# 1 is music and 0 is speech
# the correct type of the test set is  music, speech, music, speech, ...


frames1 = c()
k = 0
knn_predictions2 = as.numeric(knn_predictions)
sum0 = 0
sum1 = 0


for (i in 1:length(knn_predictions2)){

  if(knn_predictions2[i] == 1)
    sum0 = sum0 + 1
  else
    sum1 = sum1 + 1
  
  if(i%%60 == 0){

    k = k + 1
    if(sum0 - sum1 > 15) # if 75% of frames is 0
      frames1[k] = 0
    else if(sum1 - sum0 > 15)
      frames1[k] = 1
    else
      frames1[k] = 0.5 # not sure
    
    sum0 = 0
    sum1 = 0
  }
}

frames2 = c()
k = 0
svm_predictions = as.numeric(pred)
sum0 = 0
sum1 = 0


for (i in 1:length(svm_predictions)){
  
  if(svm_predictions[i] == 1)
    sum0 = sum0 + 1
  else
    sum1 = sum1 + 1
  
  if(i%%60 == 0){
    
    k = k + 1
    if(sum0 - sum1 > 15) # if 75% of frames is 0
      frames2[k] = 0
    else if(sum1 - sum0 > 15)
      frames2[k] = 1
    else
      frames2[k] = 0.5 # not sure
    
    sum0 = 0
    sum1 = 0
  }
}

# frames1 are KNN and frames2 are SVM results !

# we change on purpose the result of frames1[1] to 0 so that we can see
# that the ensemble method classifies it as 0.5(not sure) due to different results from
# the two algorithms

frames1[1] = 0

# We trust SVM more

results = c()

for(i in 1:length(frames1)){
  
  if(frames1[i] == frames2[i])
    results[i] = frames1[i] # if they both agree then they are probably right!
  else if(frames1[i] == 0.5)
    results[i] = frames2[i] # if KNN is not sure then SVM picks the result
  else if(frames1[i] != frames2[i])
    results[i] = 0.5 # if they found different things then we dont know
  
}
  

plot(frames1,type = "s",col = "red", xlab = "File",ylab="Classification")
title("KNN results")
plot(frames2,type = "s",col = "blue", xlab = "File",ylab="Classification")
title("SVM results")
plot(results,type = "s",col = "green", xlab = "File",ylab="Classification")
title("Final results")
