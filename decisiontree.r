#Purity
IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}

#Jumlah Purity Pada Data (Entropy)
Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
    -sum(res)
}

#Information Gain
InformationGain <- function( tble ) {
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}

#Train Data ID3

TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all toxic), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #calculate the information gain
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    #chose the feature with the highest information gain (e.g. 'color')
    #if more than one feature have the same information gain, then take
    #the first one
    feature <- names(which.max(ig))
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    
    childObs <- split(data[ ,names(data) != feature, drop = FALSE], 
                      data[ ,feature], 
                      drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
    
  }
}

# Split Data Train Dengan Test
set.seed(3333)
ind <- sample(2, nrow(star_data), replace=TRUE, prob = c(0.7, 0.3))
trainData <- star_data[ind==1,]
testData <- star_data[ind==2,]

#Model DT
rumus <- star_data$class ~ star_data$ra+star_data$dec+star_data$u+star_data$g+star_data$r+star_data$i+star_data$z
modeltree <- ctree(rumus, data=trainData)

#Akurasi Prediksi
pred <- predict(modeltree, newdata=testData)
library(caret)
confusionMatrix(pred, testData$class)

#Rpart
classes  <- rpart(class~., data = star_data)
rpart.plot(classes, type=1, clip.right.lab=FALSE, branch=.5, under=TRUE)


#Plot
plot(modeltree)