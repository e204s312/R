##########################
#### QUESTION 1 Part a ####
##########################
rm(list=ls())
set.seed(5072)
library(ISLR) 
library(MASS) 
library(FNN)
attach(Weekly)
dim(Weekly)

##########################
#### QUESTION 1 Part b ####
##########################
glm.logfit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family="binomial")
summary(glm.logfit)
#Lag 2 is the only statiscally significant varible
##########################
#### QUESTION 1 Part c ####
##########################
glm.probs <- predict(glm.logfit, type="response")
glm.preds <- ifelse(glm.probs>.5, "Up", "Down")
table <- (table(Weekly$Direction, glm.preds))
table


print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))


glm.train<-(Year<2009)
glm_fit<- glm(Direction~Lag2, data=Weekly, family=binomial, subset=glm.train)


Weekly_2009 <- Weekly[!glm.train,]
dim(Weekly_2009)
Direct_09 <- Direction[!glm.train] 
glm.prob<- predict(glm_fit, Weekly_2009, type="response")
glm_predicton<-rep("Down",104)
glm_predicton[glm.prob>0.5]="Up"
table<-table(Direct_09,glm_predicton)
print(table)

##########################
#### QUESTION 1 Part f ####
##########################

Weekly_2009 <- Weekly[!glm.train,]
dim(Weekly_2009)
Direct_09 <- Direction[!glm.train] 
glm.prob<- predict(glm_fit, Weekly_2009, type="response")
glm_predicton<-rep("Down", 104)
glm_predicton[glm.prob>0.5]="Up"
table<-table(Direct_09,glm_predicton)
print(table)

print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))

##########################
#### QUESTION 1 Part g ####
##########################

LDA.fit<- lda(Direction~Lag2, data=Weekly, subset=glm.train)
LDA.prediction<- predict(LDA.fit,Weekly_2009)
names(LDA.prediction)
LDA.cls<- LDA.prediction$class
table <- table(Direct_09,LDA.cls)
print(table)
CR<-mean(LDA.cls == Direct_09)
ER<- 1-CR

print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))

##########################
#### QUESTION 1 Part h ####
##########################

qda.fit<-qda(Direction~Lag2, data=Weekly, subset=glm.train)
qda.cls<-predict(qda.fit,Weekly_2009)$class
table <- table(Direct_09,qda.cls)
print(table)

print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))


##########################
#### QUESTION 1 Part i####
##########################


train_direction <- Weekly$Direction[glm.train]
train_x <- (Lag2)[glm.train] 
head(train_x)
test_x<-(Lag2)[!glm.train]
head(test_x)

j <- 1
kset <- seq(1,99,2)
error <- c()
for (k in kset){
  knn_prediciton <- knn(data.frame(train_x), data.frame(test_x), train_direction, k=k)
  table <- table(Direct_09, knn_prediciton)
  error[j] <- 1 - ((table[1] + table[4])/sum(table))
  j <- j + 1
}
(optk <- kset[which.min(error)])



knn_prediciton <- knn(data.frame(train_x), data.frame(test_x), train_direction, k=optk)
table <- table(Direct_09, knn_prediciton)
print(table)

print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))


##########################
#### QUESTION 1 Part j ####
##########################

knn_prediciton <- knn(data.frame(train_x), data.frame(test_x), train_direction, k=optk)
table <- table(Direct_09, knn_prediciton)
print(table)

print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))
##########################
#### QUESTION 1 Part k ####
##########################
#The lowest overall error is the KNN model.

##########################
#### QUESTION 2 Part a ####
##########################

rm(list=ls())
set.seed(5072)

##########################
#### QUESTION 2 Part b ###################
##########################
attach(Auto)
Auto$mpg01<-with(ifelse(mpg>median(mpg),1,0), data=Auto)
for (i in 1:(ncol(Auto)-2)){
  Auto[i] <- scale(Auto[i])}

##########################
#### QUESTION 2 Part c ###
##########################

trainprop<- 0.8
row<- sample(x=nrow(Auto), size=trainprop*nrow(Auto)) 

train.set<- Auto[row,]
test.set<- Auto[-row,]

##########################
#### QUESTION 2 Part d ###
##########################
glm_fit<- glm(as.factor(mpg01)  ~ cylinders+ displacement+ weight, data=train.set, family="binomial")
glm_prob<- predict(glm_fit, test.set, type="response")
glm_pred<- ifelse(glm_prob>0.5, "1", "0")


##########################
#### QUESTION 2 Part e ###
##########################  
predict<- as.matrix(glm_pred[1:79])
actual<- as.matrix(test.set$mpg01)
table<-table(actual,predict)
rownames(table)<- c("Below","Above")
colnames(table)<- c("Below","Above")
print(table)


print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))


##########################
#### QUESTION 2 Part f ###
########################## 

lda.fit <- lda(mpg01 ~ cylinders+ displacement+ weight, data=train.set)
lda.pred <- predict(lda.fit, test.set)
table<-table(test.set$mpg01, lda.pred$class)
rownames(table)<- c("Below","Above")
colnames(table)<- c("Below","Above")
print(table)


print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))

##########################
#### QUESTION 2 Part g ###
########################## 
qda.fit <- qda(mpg01 ~ cylinders+ displacement+ weight, data=train.set)
qda.pred <- predict(qda.fit, test.set)
table<-table(test.set$mpg01, qda.pred$class)
rownames(table)<- c("Below","Above")
colnames(table)<- c("Below","Above")
print(table)


print(paste("The overall fraction of correct preditctions:",(table[1] + table[4])/sum(table)))
print(paste("The overall error rate:", 1 - ((table[1] + table[4])/sum(table))))
print(paste("Type 1 error:", table[3]/(table[1] + table[3])))
print(paste("type 2 error:", table[2]/ (table[2] + table[4])))
print(paste("The power of the model:",table[4]/(table[2] + table[4])))
print(paste("The model precision:",table[4]/(table[3] + table[4])))


##########################
#### QUESTION 2 Part h ###
########################## 
library(FNN)

Train_x <- cbind(train.set$displacement,train.set$weight,train.set$cylinders)
Test_x<-cbind(test.set$displacement,test.set$weight,test.set$cylinders)
train.mpg <-  train.set$mpg01

error<- c()  
test.set$mpg01<- as.factor(test.set$mpg01)
kvec <- c()
j <- 1
kset <- seq(1,11,2)
error <- c()
for (k in kset){
  knn_prediciton <- knn(data.frame(Train_x ), data.frame(Test_x), train.mpg, k=k)
  table <- table(test.set$mpg01, knn_prediciton)
  error[j] <- 1 - ((table[1] + table[4])/sum(table))
  j <- j + 1
}
bestk <- kset[which.min(error)]
bestk

##########################
#### QUESTION 2 Part i ###
##########################

knn_prediciton <- knn(data.frame(Train_x ), data.frame(Test_x), train.set$mpg01, k=optk)
table <- table(test.set$mpg01,knn_prediciton)
rownames(table)<- c("Below", "Above")
colnames(table)<-c("Below","Above")
print(table)

avg<-mean(knn_prediciton == test.set$mpg01)
avg
##########################
#### QUESTION 2 Part j ###
##########################

#The KNN model is the best model because it has the lowest error rate.

##########################
#### QUESTION 3 Part a ###
##########################
rm(list=ls())
set.seed(5072)

##########################
#### QUESTION 3 Part b ###
##########################

Boston$crim01<-with(ifelse(crim>median(crim),1,0), data=Boston)
attach(Boston)
for (i in 1:(ncol(Boston)-1)){
  Boston[i] <- scale(Boston[i])}
trainprop<- 0.8
row<- sample(x=nrow(Boston), size=trainprop*nrow(Boston)) 
train.set<- Boston[row,]
test.set<- Boston[-row,]

##########################
#### QUESTION 3 Part c ###
##########################

glm_fit<- glm(crim01  ~ nox+ rad+ dis, data=train.set, family="binomial")
glm_prob<- predict(glm_fit, test.set, type="response")


##########################
#### QUESTION 3 Part d ###
##########################

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
 
installIfAbsentAndLoad('pROC')
installIfAbsentAndLoad('verification')

roc.plot(test.set$crim01,glm_prob,main="Curve for Logistical Regression")
print(roc(test.set$crim01, glm_prob)$auc)


glm_pred<- ifelse(glm_prob>0.5, "1", "0") 
table<-table(test.set$crim01,glm_pred)
print(table)

glm_pred<- ifelse(glm_prob>0.4, "1", "0")
table<-table(test.set$crim01,glm_pred)
print(table)

print(mean(glm_pred != test.set$crim01))


##########################
#### QUESTION 3 Part e ###
##########################

lda.fit <- lda(crim01  ~ nox+rad+dis, data=train.set)
lda.pred <- predict(lda.fit, test.set)
table<-table(test.set$crim01, lda.pred$class)
print(table)

roc.plot(test.set$crim01,lda.pred$posterior[,2],main="ROC Curve for LDA")
print(roc(test.set$crim01, lda.pred$posterior[,2])$auc)
print(mean(lda.pred$class != test.set$crim01))


lda.fit<- ifelse(lda.pred$posterior[,2]>0.4, "1", "0")
table<-table(test.set$crim01,l da.fit)
print(table)

print(mean(lda.pred$class != test.set$crim01))
##########################
#### QUESTION 3 Part f ###
##########################

qda_fit <- qda(crim01  ~ nox+rad+dis, data=train.set)
qda_pred <- predict(qda_fit, test.set)
table<-table(test.set$crim01, qda_pred$class)
print(table)

roc.plot(test.set$crim01,qda_pred$posterior[,2],main="ROC Curve for QDA")
print(roc(test.set$crim01, qda_pred$posterior[,2])$auc)
print(mean(quanda_pred$class != test.set$crim01))

qda_fit<- ifelse(qda_pred$posterior[,2]>0.4, "1", "0")
table<-table(test.set$crim01,qda_fit)
print(table)

print(mean(qda_pred$class != test.set$crim01))
##########################
#### QUESTION 3 Part g ###
##########################

library(FNN)

Train_x <- cbind(train.set$nox,train.set$rad,train.set$dis)
Test_x<-cbind(test.set$nox,test.set$rad,test.set$dis)
train.mpg <-  train.set$crim01


test.set$crim01<- as.factor(test.set$crim01)
klist <- c()
j <- 1
kset <- seq(1,11,2)
error <- c()
for (k in kset){
  knn_prediciton <- knn(data.frame(Train_x ), data.frame(Test_x), train.mpg, k=k)
  table <- table(test.set$crim01, knn_prediciton)
  error[j] <- 1 - ((table[1] + table[4])/sum(table))
  j <- j + 1
}
optk <- kset[which.min(error)]
optk


##########################
#### QUESTION 3 Part h###
##########################

knn_prediciton <- knn(data.frame(Train_x ), data.frame(Test_x), train.set$crim01, k=optk)
table <- table(test.set$crim01,knn_prediciton)
rownames(table)<- c("0", "1")
colnames(table)<-c("0","1")
print(table)
print(mean(knn_prediciton != test.set$crim01))
##########################
#### QUESTION 3 Part i ###
##########################

#qda model has the lowest error rate. 
##########################
#### QUESTION 3 Part j ###
##########################
print(mean(knn_prediciton != test.set$crim01))

#with cutoff = .4 then the glm has the lowest error rate

