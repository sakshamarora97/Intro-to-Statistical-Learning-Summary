library(MASS)
library(ISLR)
?Boston
head(Boston)
names(Boston)

plot(Boston$medv,Boston$crim)
cor(Boston)
attach(Boston)
plot( dis,indus)
# indus dis
# indus tax
# nox age dis
# tax rad
# High correlations seen
# Insights
# Expected
# There is higher nox where industry proportion is higher
# As mean distance from employment center (dis increases), porportion of industries decreases
# we can say that employment is mainly in industries only
#Tax is higher where indus is higher
#nox is higher where dis is lower

plot(medv~lstat,Boston)
fit1 = lm(medv~lstat,data=Boston)
fit1
summary(fit1)
cor(medv,lstat)
sqrt(0.5441)

abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval ="confidence")


## Multiple linear regression
fit2 = lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3 = lm(medv~.,data=Boston)
summary(fit3)
##age was significant,but now it's not significant
##there are variables which are correlated with age thus
##age is not required
par(mfrow=c(2,2))
plot(fit3)
##Residuals vs fitted to see nonlinearity
fit4 = update(fit3,~.-age-indus)
summary(fit4)
## Non linear terms and interactions
fit5 = lm(medv~lstat*age,Boston) ##interaction with main effects included
summary(fit5)

fit6 =lm(medv~lstat+I(lstat^2),Boston); summary(fit6);
##including the non linearity of lstat
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
#can't use abline since fit is nonlinear
points(lstat,fitted(fit6),col="red",pch=20)
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)



###qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit8 = lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit8)

contrasts(Carseats$ShelveLoc)
###Writing R functions
regplot = function(x,y){
  fit = lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)

regplot = function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}

regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)
