rm(list=ls())
###########################
#######QUESTION 1##########
###########################
proddata <- read.table("ProductionData.txt", sep ='\t', header=TRUE)
head(proddata)
###########################
#######QUESTION 2########
###########################
prod.mach <- as.character(proddata$prod.mach)
splitprod.mach <- strsplit(prod.mach, '/')
product <-sapply(splitprod.mach,"[",1)
machine <-sapply(splitprod.mach,"[",2)
proddata <- cbind(product, machine, proddata[-1])
product <- as.numeric(proddata$product)
proddata$product <- factor(product, ordered= TRUE)
proddata$machine <- factor(machine, ordered= TRUE)
head(proddata)
str(proddata)
###########################
#######QUESTION 3##########
###########################
proddata$starttime <- (strptime(proddata$starttime, "%m/%d/%y %H:%M " ))
proddata$endtime <- (strptime(proddata$endtime, "%m/%d/%y %H:%M " ))
proddata$elapsedminutes <- difftime(time1 = proddata$endtime, time2 = proddata$starttime, units = "min")
str(proddata)
###########################
#######QUESTION 4##########
###########################

proddata$product <- as.numeric(proddata$product)
proddata$machine <- as.numeric(proddata$machine)
proddata <- proddata[,-c(3:5)]
proddata.agg <- aggregate(proddata, by = list(product,machine), sum)
proddata.agg <- proddata.agg[,-c(3:4)]
product <- proddata.agg[]
names(proddata.agg)[1] <- "product"
names(proddata.agg)[2] <- "machine"
proddata.agg
str(proddata.agg)
proddata.agg$product<-as.numeric(proddata.agg$product)
proddata.agg$product <- factor(proddata.agg$product, ordered=T)
proddata.agg$machine<-as.numeric(proddata.agg$machine)
proddata.agg$machine <- factor(proddata.agg$machine, ordered=T)
str(proddata.agg)
proddata.agg
###########################
#######QUESTION 5##########
###########################
proddata.agg$avgscraprate <- (proddata.agg$scrap/proddata.agg$batchsize)
proddata.agg$avgruntime <- (proddata.agg$elapsedminutes/proddata.agg$batchsize)
head(proddata.agg)
###########################
#######QUESTION 6##########
###########################
proddata.agg <- proddata.agg[order(proddata.agg$product, proddata.agg$machine),]
proddata.agg

###########################
#######QUESTION 7##########
###########################
set.seed(5072)

#recreating proddata to include deleted rows 
proddata <- read.table("ProductionData.txt", sep ='\t', header=TRUE)
prod.mach <- as.character(proddata$prod.mach)
splitprod.mach <- strsplit(prod.mach, '/')
product <-sapply(splitprod.mach,"[",1)
machine <-sapply(splitprod.mach,"[",2)
proddata <- cbind(product, machine, proddata[-1])
product <- as.numeric(proddata$product)
proddata$product <- factor(product, ordered= TRUE)
proddata$machine <- factor(machine, ordered= TRUE)
proddata$starttime <- (strptime(proddata$starttime, "%m/%d/%y %H:%M " ))
proddata$endtime <- (strptime(proddata$endtime, "%m/%d/%y %H:%M " ))
proddata$elapsedminutes <- proddata$endtime-proddata$starttime
proddata$product <- as.numeric(proddata$product)
proddata$machine <- as.numeric(proddata$machine)
#proddata
nobs <- nrow(proddata) 
trainprop <- 0.80 
validateprop <- 0.1
trainset <- sample(nobs, trainprop * nobs)
validateset <- sample(setdiff(1:nobs, trainset), validateprop * nobs) 
testset <- setdiff(setdiff(1:nobs, trainset), validateset)
length(intersect(trainset,validateset))
length(intersect(trainset,testset))
length(intersect(testset,validateset))
length(union(union(trainset, testset), validateset)) == nobs
trainset <- proddata[trainset, ]
validateset <- proddata[validateset, ]
testset <- proddata[testset,]
nobs
nrow(trainset)
nrow(testset)
nrow(validateset)
head(trainset, 5)
head(validateset, 5)
head(testset, 5)

set.seed=NULL       
###########################
#######QUESTION 8##########
###########################
rm(list=ls())
set.seed(5072)
vec <- vector("numeric",1)
for(i in 1) {
  if((x <- rnorm(1,mean=100,sd=5)) < 80) {
    break
  } else {
    vec <- c(x)
    print(length(vec[1]))
  }
}
  min(vec)
  set.seed=NULL