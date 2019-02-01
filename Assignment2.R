rm(list=ls())
##########################
#### QUESTION 1 Part a ###
##########################
set.seed(5072)
##########################
#### QUESTION 1 Part b ###
##########################
x <- rnorm(100,0,1)
##########################
#### QUESTION 1 Part c###
##########################
eps <- rnorm(100,0, .25)
##########################
#### QUESTION 1 Part d ###
##########################
y <- as.vector(x/2 -1+eps)
##########################
#### QUESTION 1 Part e ###
##########################
length(y)
##########################
#### QUESTION 1 Part f ###
##########################
#for this model -1 represents our slope estimator for beta 0, and 0.5 represents our Beta 1 
##########################
#### QUESTION 1 Part g ###
##########################
plot(y ~x)
##########################
#### QUESTION 1 Part h ###
##########################
# I see an apparently linear positive relationship that has an apparent trend but displays high varibility 
##########################
#### QUESTION 1 Part i ###
##########################
lm.fit <- lm(y ~ x)
summary(lm.fit)
##########################
#### QUESTION 1 Part j###
##########################
#the estimators in the fitted model show beta 0 being -1 and beta 1 being .4676 in the fitted model, 
#the estimates are quite close to the unfitted model and the the R squared is about 70% showing that this model is actually a pretty good fit for the data
##########################
#### QUESTION 1 Part k ###
##########################
plot(y~x); abline(lm.fit, col ="red")
abline(-1, 0.5, col='black') 
##########################
#### QUESTION 1 Part m###
##########################
legend("bottomright", c("lm.fit", "Population"),col=c("red", 'black'), lty=c(1,1))
##########################
#### QUESTION 1 Part n ###
##########################
polynomial <- lm(y~x+I(x^2))
summary(polynomial)
##########################
#### QUESTION 1 Part o###
##########################
anova(lm.fit, polynomial)
#Although the R squared is .0009 higher in the polynominal term the p values in the anova summaries suggest that the polynomial term doesn't make the model better and we fail to reject the null hypothesis
##########################
#### QUESTION 1 Part p ###
##########################
eps2 <- rnorm(100,0, sqrt(.1))
y <- as.vector(x/2 -1+eps2)
length(y)
#for this model -1 represents our slope estimator for beta 0, and 0.5 represents our Beta 1 
plot(y ~x)
# I see an apparently linear positive relationship that has an apparent trend but displays less varibility than the first model 
lm.fit2 <- lm(y ~ x)
summary(lm.fit2)
#the estimators in the fitted model show beta 0 being -1 and beta 1 being .506 in the fitted model,lo

#the estimates are closer to the unfitted model that the first time, reducing the variation makes the fit for the model even better 
plot(y~x); abline(lm.fit2, col ="red")
abline(-1, 0.5, col='black') 
legend("bottomright", c("lm.fit", "Population"),col=c("red", 'black'), lty=c(1,1))
##########################
#### QUESTION 1 Part q ###
##########################
eps3 <- rnorm(100,0, sqrt(.5))
y <- as.vector(x/2 -1+eps3)
length(y)
#for this model -1 represents our slope estimator for beta 0, and 0.5 represents our Beta 1 
plot(y ~x)
# I see a less apparent linear positive relationship that that displays high varibility 
lm.fit3 <- lm(y ~ x)
summary(lm.fit3)
#the estimators in the fitted model show beta 0 being -1 and beta 1 being .506 in the fitted model,lo
#the estimates are further away than the two prior models suggesting that when you introduce higher variance it becomes more difficult to predict the slope estimators 
plot(y~x); abline(lm.fit3, col ="red")
abline(-1, 0.5, col='black') 
legend("bottomright", c("lm.fit", "Population"),col=c("red", 'black'), lty=c(1,1))
##########################
#### QUESTION 1 Part r ###
##########################

# As the variance incresses we see the model fitting the data less and less with the best fit being with variance of .1 and worse being with variance of .5
##########################
#### QUESTION 1 Part s ###
##########################
confint(lm.fit)
confint(lm.fit2)
confint(lm.fit3)
##########################
#### QUESTION 1 Part t ###
##########################
#As you increase the noise in the systems, the intervals widen, the data is more predictable with less noise.
##########################
#### QUESTION 2 Part a ###
##########################
set.seed (5072)
x1 = runif (100)
x2 = 0.5 * x1 + rnorm (100) /10
y = 2 + 2* x1 + 0.3* x2 + rnorm (100)
##########################
#### QUESTION 2 Part a ###
##########################
#Population Parameters are b0=2, b1=2, b2=.03
##########################
#### QUESTION 2 Part b###
##########################
cor(x1,y)
cor(x2,y)
cor(x1,x2)
##########################
#### QUESTION 2 Part c ###
##########################
pairs(y~x1+x2)
##########################
#### QUESTION 2 Part d ###
##########################
print('With x1 and x1 having a correlation of .84 we can say they are highly correlated with one another')
##########################
#### QUESTION 2 Part e ###
##########################
lm.fit.both <- lm(y ~ x1+x2)
##########################
#### QUESTION 2 Part f ###
##########################
summary(lm.fit.both)
##########################
#### QUESTION 2 Part g ###
##########################
print("At the 95% confidence level only B0 and B1 are statisically significant")
##########################
#### QUESTION 2 Part h ###
##########################
print("We reject the null the B1=0, with a t value of 3.63, it surpasses the critical value and we would fail to reject B2=0 as it fails to pass the critical value needed")
##########################
#### QUESTION 2 Part i ###
##########################
lm.fit.justx1 <- lm(y~x1)
##########################
#### QUESTION 2 Part j ###
########################## 
summary(lm.fit.justx1)
##########################
#### QUESTION 2 Part k ###
##########################
print('We reject the null that B1=0 as it has a p-value less than .05 ')
##########################
#### QUESTION 2 Part l ###
##########################
lm.fit.justx2 <- lm(y~x2)
summary(lm.fit.justx2)
##########################
#### QUESTION 2 Part m ###
##########################
print('We reject the null hypothesis as the P-Value is less than .05')
##########################
#### QUESTION 2 Part n ###
##########################
print('NO the results dont contradict one another, in the model with both B2 is insignificant, but with just B2 it is significant mean we are dealing with high correlation and colinearity which can mask our B2')
##########################
#### QUESTION 2 Part o ###
##########################
x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)
lm.fit.both2 <- lm(y ~ x1+x2)
summary(lm.fit.both2)
lm.fit.justx12 <- lm(y~x1)
summary(lm.fit.justx12)
lm.fit.justx22 <- lm(y~x2)
summary(lm.fit.justx22)
##########################
#### QUESTION 2 Part p###
##########################
print('With this new data we run into the problem of joint insignificants, so will B1 and B2 are significant by themselves, together they are insignificant')
##########################
#### QUESTION 2 Part q ###
##########################
par(mfrow=c(2, 2)) 
plot(lm.fit.both2)
plot(lm.fit.justx12)
plot(lm.fit.justx22)
print('')
par(mfrow=c(1,1))
##########################
#### QUESTION 2 Part r ###
##########################
print('Point 101 is a high leverage pont in the model with both x1 and x2 and also whe its just x2, and an outlier in the model with just x1')

##########################
#### QUESTION 3 Part a ###
##########################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
needed  <-  c("ISLR", "MASS", "car")      
installIfAbsentAndLoad(needed)
set.seed(5072)
library("MASS")
str(Boston)

zn <- lm(Boston$crim~Boston$zn)
zn
indus<- lm(Boston$crim~Boston$indus)
indus
chas <- lm(Boston$crim~Boston$chas)
nox <- lm(Boston$crim~Boston$nox)
rm<- lm(Boston$crim~Boston$rm)
age<- lm(Boston$crim~Boston$age)
dis<- lm(Boston$crim~Boston$dis)
rad <- lm(Boston$crim~Boston$rad)
tax <- lm(Boston$crim~Boston$tax)
ptratio<- lm(Boston$crim~Boston$ptratio)
black <- lm(Boston$crim~Boston$black)
lstat <- lm(Boston$crim~Boston$lstat)
medv <- lm(Boston$crim~Boston$medv)


zn_fstat <-summary(zn)$fstatistic[1]
zn_fstat
zn_pvalue <- anova(zn)$'Pr(>F)'[1]
zn_pvalue
coef(zn)
zn_beta <- coef(zn)[2]
zn_beta



summary(indus)$fstatistic[1]
indus_fstat <- 99.81704
indus_pvalue <- anova(indus)$'Pr(>F)'[1]
indus_pvalue
coef(indus)
indus_beta <- 0.5097763


summary(chas)$fstatistic[1]
chas_fstat <-1.57936
chas_pvalue <- anova(chas)$'Pr(>F)'[1]
chas_pvalue
coef(chas)
chas_beta <- -1.89277



nox_fstat <- summary(nox)$fstatistic[1]
nox_pvalue <- anova(nox)$'Pr(>F)'[1]
nox_pvalue
noxbeta <- coef(nox)[2]
noxbeta



rm_fstat <- summary(rm)$fstatistic[1]
rm_fstat
rm_pvalue <- anova(rm)$'Pr(>F)'[1]
rm_pvalue
rmbeta <- coef(rm)[2]
rmbeta


age_fstat <-summary(age)$fstatistic[1]
age_fstat
age_pvalue <- anova(age)$'Pr(>F)'[1]
age_pvalue
coef(age)
age_beta <- coef(age)[2]
age_beta

dis_fstat <-summary(dis)$fstatistic[1]
dis_fstat
dis_pvalue <- anova(dis)$'Pr(>F)'[1]
dis_pvalue
coef(dis)
dis_beta <- coef(dis)
dis_beta

rad_fstat <-summary(rad)$fstatistic[1]
rad_fstat
rad_pvalue <- anova(rad)$'Pr(>F)'[1]
rad_pvalue
coef(rad)
rad_beta <- coef(rad)[2]
rad_beta

tax_fstat <-summary(tax)$fstatistic[1]
tax_fstat
tax_pvalue <- anova(tax)$'Pr(>F)'[1]
tax_pvalue
coef(tax)
tax_beta <- coef(tax)[2]
tax_beta

ptratio_fstat <-summary(ptratio)$fstatistic[1]
ptratio_fstat
ptratio_pvalue <- anova(ptratio)$'Pr(>F)'[1]
ptratio_pvalue
coef(ptratio)
ptratio_beta <- coef(ptratio)[2]
ptratio_beta

black_fstat <-summary(black)$fstatistic[1]
black_fstat
black_pvalue <- anova(black)$'Pr(>F)'[1]
black_pvalue
coef(black)
black_beta <- coef(black)[2]
black_beta

lstat_fstat <-summary(lstat)$fstatistic[1]
lstat_fstat
lstat_pvalue <- anova(lstat)$'Pr(>F)'[1]
lstat_pvalue
coef(lstat)
lstat_beta <- coef(lstat)[2]
lstat_beta

medv_fstat <-summary(medv)$fstatistic[1]
medv_fstat
medv_pvalue <- anova(medv)$'medv>F)'[1]
medv_pvalue
coef(medv)
medv_beta <- coef(medv)[2]
medv_beta

matrix <- matrix(c('zn', zn_fstat, zn_pvalue, zn_beta,
                   'indus',indus_fstat,indus_pvalue,indus_beta,
                   'chas', chas_fstat, chas_pvalue, chas_beta, 
                   'nox', nox_fstat, nox_pvalue, noxbeta,
                   'rm', rm_fstat, rm_pvalue, rmbeta,
                   'age', age_fstat, age_pvalue, age_beta,
                   'dis', dis_fstat, dis_pvalue, dis_beta,
                   'rad', rad_fstat, rad_pvalue, rad_beta,
                   'tax', tax_fstat, tax_pvalue, tax_beta,
                   'ptratio', ptratio_fstat, ptratio_pvalue, ptratio_beta,
                   'black', black_fstat, black_pvalue, black_beta,
                   'lstat', lstat_fstat, lstat_pvalue, lstat_beta,
                   'medv', medv_fstat, medv_pvalue, medv_beta), nrow =13, byrow=T)
              

matrix

##########################
#### QUESTION 3 Part b ###
##########################
#Every predictor except for chas is significant 

##########################
#### QUESTION 3 Part c ###
##########################
par(mfrow=c(4,3))
plot(x=Boston$zn, Boston$crim)
abline(zn, col='red')

plot(Boston$indus, Boston$crim)
abline(indus, col='red')

plot(Boston$nox, Boston$crim)
abline(nox, col="red")

plot(Boston$rm, Boston$crim)
abline(rm, col='red')

plot(Boston$age, Boston$crim)
abline(age, col='red')

plot(Boston$dis, Boston$crim)
abline(dis, col='red')

plot(Boston$rad, Boston$crim)
abline(rad, col='red')

plot(Boston$tax, Boston$crim)
abline(tax, col='red')

plot(Boston$ptratio, Boston$crim)
abline(ptratio, col='red')

plot(Boston$black, Boston$crim)
abline(black, col='red')

plot(Boston$lstat, Boston$crim)
abline(lstat, col='red')

plot(Boston$medv, Boston$crim)
abline(medv, col='red')
par(mfrow=c(1,1))

##########################
#### QUESTION 3 Part d ###
##########################

fit.all <- lm(crim ~ ., data = Boston)
summary(fit.all)

##########################
#### QUESTION 3 Part e ###
##########################
fit.all2 <- lm(crim ~zn+dis+rad+black+medv, data = Boston)
summary(fit.all2)

##########################
#### QUESTION 3 Part f ###
##########################

simple.reg <- vector("numeric",0)
simple.reg <- c(simple.reg, zn_beta)
simple.reg <- c(simple.reg, indus_beta)
simple.reg <- c(simple.reg, chas_beta)
simple.reg <- c(simple.reg, noxbeta)
simple.reg <- c(simple.reg, rmbeta)
simple.reg <- c(simple.reg, age_beta)
simple.reg <- c(simple.reg, dis_beta)
simple.reg <- c(simple.reg, rad_beta)
simple.reg <- c(simple.reg, tax_beta)
simple.reg <- c(simple.reg, ptratio_beta)
simple.reg <- c(simple.reg, black_beta)
simple.reg <- c(simple.reg, lstat_beta)
simple.reg <- c(simple.reg, medv_beta)
mult.reg <- vector("numeric", 0)
fit.all <- coef(fit.all)
mult.reg <- matrix(c(mult.reg, fit.all))
plot(simple.reg, mult.reg, col = "black")
#Due to the intercept term in the multiple regression case represing the increase in the slope estimators holding other estimators constant 
# The simple linear regression make othe most sense because it displays a strong relationshim between some of the predictors, while the multiple regression doesnt


##########################
#### QUESTION 3 Part g ###
##########################


fit.zn2 <- lm(Boston$crim ~ poly(Boston$zn, 3))
ZN <- as.numeric(unlist(c(anova(fit.zn2,zn)[5:6])))

fit.indus2 <- lm(Boston$crim ~ poly(Boston$indus, 3))
INDUS <- as.numeric(unlist(c(anova(fit.indus2,indus)[5:6])))

fit.nox2 <- lm(Boston$crim ~ poly(Boston$nox, 3))
NOX <- as.numeric(unlist(c(anova(fit.nox2,nox)[5:6])))

fit.rm2 <- lm(Boston$crim ~ poly(Boston$rm, 3))
RM <- as.numeric(unlist(c(anova(fit.rm2,rm)[5:6])))

fit.age2 <- lm(Boston$crim ~ poly(Boston$age, 3))
AGE <- as.numeric(unlist(c(anova(fit.age2,age)[5:6])))

fit.dis2 <- lm(Boston$crim ~ poly(Boston$dis, 3))
DIS <- as.numeric(unlist(c(anova(fit.dis2,dis)[5:6])))

fit.rad2 <- lm(Boston$crim ~ poly(Boston$rad, 3))
RAD <- as.numeric(unlist(c(anova(fit.rad2,rad)[5:6])))


fit.tax2 <- lm(Boston$crim ~ poly(Boston$tax, 3))
TAX <- as.numeric(unlist(c(anova(fit.tax2,tax)[5:6])))


fit.ptratio2 <- lm(Boston$crim ~ poly(Boston$ptratio, 3))
PTRATIO <- as.numeric(unlist(c(anova(fit.ptratio2,ptratio)[5:6])))
PTRATIO

fit.black2 <- lm(Boston$crim ~ poly(Boston$black, 3))
BLACK <- as.numeric(unlist(c(anova(fit.black2,black)[5:6])))

fit.lstat2 <- lm(Boston$crim ~ poly(Boston$lstat, 3))
LSTAT <- as.numeric(unlist(c(anova(fit.lstat2,lstat)[5:6])))


fit.medv2 <- lm(Boston$crim ~ poly(Boston$medv, 3))
MEDV <- as.numeric(unlist(c(anova(fit.medv2,medv)[5:6])))

AN<- rbind(MEDV,DIS, NOX, INDUS, AGE, TAX,PTRATIO,RM,ZN,RAD,LSTAT, RAD)
table <- AN[,-1]
final <- table[,-2]
table <- as.matrix(final)
table



