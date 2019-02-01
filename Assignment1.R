rm(list=ls())

###########################
#######QUESTION 1##########
###########################
Homeprices <- read.table("HomePrices.txt", sep ='\t', header=TRUE)
head(Homeprices)
n <- nrow(Homeprices)
p <- ncol(Homeprices)-1
print(paste('MSE of predicting the mean:', 
            mean((Homeprices$medv-mean(Homeprices$medv))^2)))
print(paste('Variance of the population:', 
            (var(Homeprices$medv)*(n-1)/n)))
Homeprices[1:p] <- scale(Homeprices[1:p])
head(Homeprices)
set.seed(5072)
trainprop <- 0.75  
validateprop <- 0.15


train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)
trainset <- Homeprices[train,]
validateset <- Homeprices[validate,]
testset <- Homeprices[test,]
head(trainset)
head(validateset)
head(testset)

train.x <- trainset[-13]
train.y <- trainset$medv
validate.x <- validateset[-13]
validate.y <- validateset$medv
test.x <- testset[-13]
test.y <- testset$medv

#install.packages('FNN') 
#install.packages('ISLR')
library(FNN)

biggestk <- 19

testMSEs <- rep(0, biggestk %/% 2 + 1)
trainMSEs <- rep(0, biggestk %/% 2 + 1)
validateMSEs <- rep(0, biggestk %/% 2 + 1)
kset <- seq(1, biggestk, by=2)

for(k in kset) {
  knn.regression.model <- knn.reg(train.x, test.x, train.y, k=k)
  testMSEs[(k+1)/2] <- mean((test.y-knn.regression.model$pred)^2)
  knn.regression.model <- knn.reg(train.x, validate.x, train.y, k=k)
  validateMSEs[(k+1)/2] <- mean((validate.y-knn.regression.model$pred)^2)
  #print(knn.regression.model)
  # print(testMSEs)
  knn.regression.model <- knn.reg(train.x, train.x, train.y, k=k)
  trainMSEs[(k+1)/2] <- mean((train.y-knn.regression.model$pred)^2)
}

trainMSEs
validateMSEs
print(cbind(kset, testMSEs, trainMSEs, validateMSEs))
print(paste("Best test k is", kset[which.min(validateMSEs)], "with a test MSE of", validateMSEs[which.min(validateMSEs)]))
print(paste("Best training k is", kset[which.min(trainMSEs)], "with training MSE of", trainMSEs[which.min(trainMSEs)]))

plot(kset, validateMSEs, 
     xlim=c(biggestk, 1), 
     ylim=c(0,max(c(validateMSEs, trainMSEs))),      
     type='n',  
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Mean Squared Errors', 
     main='Training and validate MSEs as a Function of \n Flexibility for KNN Prediction')
lines(seq(biggestk, 1, by=-2), 
      validateMSEs[order(length(validateMSEs):1)], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(biggestk, 1, by=-2), 
      trainMSEs[order(length(trainMSEs):1)], 
      type='b', 
      col=1, 
      pch=16)
legend("topleft", legend = c("Validate MSEs", "Training MSEs"), 
       col=c(2, 1), 
       cex=.75, 
       pch=16)
set.seed(seed=NULL)
##################
####QUESTION 2###
##################



#install.packages('class')
library(class)

Loan <- read.csv("LoanData (1).csv", header=TRUE)
head(Loan)
Loan$yes = "Yes"
mytable <- table(Loan$loan_repaid, Loan$yes)
print(paste("The error rate that would result from all yes", mytable[1]/sum(mytable[1],mytable[2])))
Loan <- Loan[-9]
n <- nrow(Loan)
p <- ncol(Loan)-1
Loan <- Loan[-9]
Loan[1:p] <- scale(Loan[1:p])
set.seed(5072)

trainprop <- 0.75  
validateprop <- 0.15


train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(setdiff(1:n, train), validate)
trainset <- Loan[train,]
validateset <- Loan[validate,]
testset <- Loan[test,]
head(trainset)
head(testset)
head(validateset)

train.x <- trainset[-8]
train.y <- trainset$loan_repaid
validate.x <- validateset[-8]
validate.y <- validateset$loan_repaid
test.x <- testset[-8]
test.y <- testset$loan_repaid

library(class)


biggestk <- 19

test.error <- rep(0, biggestk %/% 2 + 1)
train.error <- rep(0, biggestk %/% 2 + 1)
validate.error <- rep(0, biggestk %/% 2 + 1)
kset <- seq(1, biggestk, by=2)

for(k in kset) {
  knn.predvalidate<- knn(train.x, validate.x, train.y, k=k)
  validate.error[k %/% 2 + 1] <- mean(validate.y != knn.predvalidate )
  knn.predtrain<- knn(train.x, train.x, train.y, k=k)
  train.error[k %/% 2 + 1] <- mean(train.y != knn.predtrain)
}

print(paste("Minimum train set error rate occurred at k =", which.min(train.error )))
print(paste("Minimum train error rate was ", train.error[which.min(train.error)]))

print(paste("Minimum validate set error rate occurred at k =", which.min(validate.error )))
print(paste("Minimum validate error rate was ", validate.error[which.min(validate.error)]))

plot(NULL, NULL, type='n', 
     xlim=c(10, 1), 
     ylim=c(0,max(c(validate.error, train.error))), 
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Error Rates', 
     main='Error Rates as a Function of \n Flexibility for KNN Classification')
lines(seq(10, 1), 
      validate.error[length(validate.error):1], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(10, 1), 
      train.error[length(train.error):1], 
      type='b', 
      col=1, 
      pch=16)
legend("topleft", legend = c("Validation Error Rate", "Training Error Rate"), 
       col=c(2, 1), 
       cex=.75, 
       pch=16)
##################
###QUESTION 3#####
##################


Homeprices <- read.table("HomePrices.txt", sep ='\t', header=TRUE)
head(Homeprices)
n <- nrow(Homeprices)
p <- ncol(Homeprices)-1
print(paste('MSE of predicting the mean:', 
            mean((Homeprices$medv-mean(Homeprices$medv))^2)))
print(paste('Variance of the population:', 
            (var(Homeprices$medv)*(n-1)/n)))
Homeprices[1:p] <- scale(Homeprices[1:p]) 
head(Homeprices) 
set.seed(5072)
trainprop <- 0.75  
validateprop <- 0.15

vmse <- vector()
validate_k <- vector()
tmse <- vector()


for (i in 1:50) {
  train  <-  sample(n, trainprop * n)
  validate  <-  sample(setdiff(1:n, train), validateprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  trainset <- Homeprices[train,]
  validateset <- Homeprices[validate,]
  testset <- Homeprices[test,]
  head(trainset)
  head(validateset)
  head(testset)
  
  train.x <- trainset[-13]
  train.y <- trainset$medv
  validate.x <- validateset[-13]
  validate.y <- validateset$medv
  test.x <- testset[-13]
  test.y <- testset$medv
  
  #install.packages('FNN') 
  #install.packages('ISLR')
  library(FNN)
  
  biggestk <- 19
  
  testMSEs <- rep(0, biggestk %/% 2 + 1)
  trainMSEs <- rep(0, biggestk %/% 2 + 1)
  validateMSEs <- rep(0, biggestk %/% 2 + 1)
  kset <- seq(1, biggestk, by=2)
  vmse[i] <- Inf
  tmse[i] <- Inf
  for(k in kset) {
    knn.regression.model <- knn.reg(train.x, test.x, train.y, k=k)
    testMSEs[(k+1)/2] <- mean((test.y-knn.regression.model$pred)^2)
    knn.regression.model <- knn.reg(train.x, validate.x, train.y, k=k)
    validateMSEs[(k+1)/2] <- mean((validate.y-knn.regression.model$pred)^2)
    #print(knn.regression.model)
    # print(testMSEs)
    knn.regression.model <- knn.reg(train.x, train.x, train.y, k=k)
    trainMSEs[(k+1)/2] <- mean((train.y-knn.regression.model$pred)^2)
  }
  
  if (validateMSEs[(k+1)/2]<vmse[i]){
    vmse[i] <- validateMSEs[(k+1)/2]
    validate_k[i] <- k
    vmse[i]
  }
  
  if (testMSEs[(k+1)/2]<tmse[i]){
    tmse[i] <- testMSEs[(k+1)/2]
    validate_k[i] <- k
    tmse[i]
  }
  
  print(cbind(kset, testMSEs, trainMSEs, validateMSEs))
  print(paste("Best test k is", kset[which.min(validateMSEs)], "with a Validate MSE of", validateMSEs[which.min(validateMSEs)]))
  print(paste("Best training k is", kset[which.min(trainMSEs)], "with training MSE of", trainMSEs[which.min(trainMSEs)]))
  
}
vmse
tmse
mtmse <- mean(tmse)
sd(tmse)
mvmse<- mean(vmse)
sd(tmse)
therange <- seq(1,50)
print(therange)
?seq
plot(therange, tmse, 
     xlim=c(1, length(therange)), 
     ylim=c(0,max(c(vmse, tmse))),      
     type='n',  
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Mean Squared Errors', 
     main='Training and validate MSEs as a Function of \n Flexibility for KNN Prediction')
lines(seq(1,length(therange)), 
      vmse[order(1:length(vmse))], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(1,length(therange)), 
      tmse[order(1:length(tmse))], 
      type='b', 
      col=1, 
      pch=16)
abline(h=mean(vmse), col = 'black', lty = 'dotdash')
abline(h=mean(tmse), col = 'red', lty = 'dotdash')
legend("topleft", legend = c("Validate MSEs", "Training MSEs", "Mean ValidateMSE", "Mean Train MSEs"), 
       col=c(2, 1), 
       cex=.75, 
       pch=16)
set.seed(seed=NULL)
##################
###QUESTION 4#####
##################
college <- read.table('apps.csv', header=T, sep=',', quote='')

train.x <- college[-1,-9, -13]
train.y <- college$Applications
biggestk <- 19


trainMSEs <- rep(0, biggestk)

kset <- seq(1, biggestk, by=2)
for(k in kset) {
  knn.regression.model <- knn.reg(train.x, train.x, train.y, k=k)
  trainMSEs[k] <- mean((train.y-knn.regression.model$pred)^2)
}

print(paste("Best test k is", kset[which.min(trainMSEs)], "with a train MSE of", validateMSEs[which.min(trainMSEs)]))

print('omitted varibles: terminal degrees, PHd students')
