# Elements Statistical Learning

# Chapter 3: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression
names(Boston)
?Boston
plot(medv~lstat,Boston)
fit1 = lm(medv~lstat,data=Boston)
fit1
summary(fit1)
abline(fit1,col="red")
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")

# Multiple Linear Regression
fit2 = lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3 = lm(medv~.,Boston)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
fit4 = update(fit3,~.-age-indus)
summary(fit4)

# Nonlinear terms and interactions
fit5 = lm(medv~lstat*age,Boston)
summary(fit5)
fit6 = lm(medv~lstat + I(lstat^2),Boston); summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
fit7 = lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
plot(1:20,1:20,pch=1:20,cex=2)

# Qualitative predictors
fix(Carseats)
names(Carseats)
summary(Carseats)
fit1 = lm(Sales~. + Income:Advertising + Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)

# Writing functions
regplot = function(x,y,...){
  fit=(lm(y~x))
  plot(x,y,...)
  abline(fit,col="red")
}

attach(Carseats)
regplot(Price,Sales)
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)


# Chapter 4: Classification

require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)

# Logistic Regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)
# make training and test
train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
# fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred = ifelse(glm.probs>0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)

# Linear Discriminant Analysis
require(ISLR)
require(MASS)
lda.fit = lda(Direction~Lag1+Lag2,data=Smarket,subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket,Year==2005)
lda.pred = predict(lda.fit,Smarket.2005)
class(lda.pred)
data.frame(lda.pred)[1:5, ]
table(lda.pred$class,Smarket.2005
      $Direction)
mean(lda.pred$class==Smarket.2005$Direction)

# K-nearest neighbours
library(class)
?knn
attach(Smarket)
Xlag=cbind(Lag1,Lag2)
train=Year<2005
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

# Chapter 5: Resampling Methods

# Cross-Validation
require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)

# LOOCV
glm.fit = glm(mpg~horsepower,data=Auto)
summary(glm.fit)
cv.glm(Auto,glm.fit)$delta

# write a function to use a formula
loocv = function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

loocv(glm.fit)

cv.error=rep(0.5)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error[d]=loocv(glm.fit)
}

plot(degree,cv.error,type="b")

# 10-fold CV
cv.error10=rep(0.5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d),data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}

lines(degree,cv.error10,type="b",col="red")

# Bootstrap
# minimum risk investment x and y investments
alpha = function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}

alpha(Portfolio$X,Portfolio$Y)

# What is the standard error of alpha?
alpha.fn = function(data,index){
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)


# Chapter 7: Moving beyond linearity

require(ISLR)
attach(Wage)

# Polynomials
fit = lm(wage~poly(age,4),data=Wage)
summary(fit)

agelines=range(age)
age.grid=seq(form=agelines[1],to=agelines[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bonds=cbind(preds$fit+2*preds$se,preds$fit-2*preds$se)
plot(age,wage,col="darkgrey")
lines(age.grid,preds$fit,lwd=1,col="blue")
matlines(age.grid,se.bonds,col="blue",lty=2)

# a more direct way...
fita = lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
summary(fita)
plot(fitted(fit),fitted(fita))

summary(fit)

fita=lm(wage~education,data=Wage)
fitb=lm(wage~education+age,data=Wage)
fitc=lm(wage~education+poly(age,2),data=Wage)
fitd=lm(wage~education+poly(age,2),data=Wage)

anova(fita,fitb,fitc,fitd)

# polynomial logistic regression
fit=glm(I(wage>250)~poly(age,3),data=Wage,family=binomial)
summary(fit)
preds=predict(fit,list(age=age.grid),se=T)
se.bands=preds$fit+cbind(fit=0,lower=-2*preds$se,upper=2*preds$se)
se.bands[1:5,]

prob.bands=exp(se.bonds)/(1+exp(se.bonds))
matplot(age.grid,prob.bands,col="blue",lwd=c(2,1,1),lty=c(1,2,2),type="l",ylim=c(0,.1))
points(jitter(age),I(wage>250)/10,pch="|",cex=0.5)

# Splines
require(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
plot(age,wage,col="darkgrey")
lines(age.grid,predict(fit,list(age=age.grid)),col="darkgreen",lwd=2)
abline(v=c(25,40,60),lty=2,col="darkgreen")

fit=smooth.spline(age,wage,df=16)
lines(fit,col="red",lwd=2)

fit=smooth.spline(age,wage,cv=TRUE)
lines(fit,col="purple",lwd=2)
fit

# GAM
require(gam)
gam1 = gam(wage~s(age,df=4)+s(year,df=4)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam1,se=T)

gam2 = gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education,data=Wage,family=binomial)
plot(gam2)

gam2a = gam(I(wage>250)~s(age,df=4)+year+education,data=Wage,family=binomial)
anova(gam2a,gam2,test="Chisq")

# Chapter 8: Tree based Methods

require(ISLR)
require(tree)
attach(Carseats)

hist(Sales)
High = ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
Carseats$High = as.factor(Carseats$High)

tree.carseats=tree(High~.-Sales,data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

set.seed(1011)
train =sample(1:nrow(Carseats),250)
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.pred=predict(tree.carseats,Carseats[-train,],type="class")
with(Carseats[-train,],table(tree.pred,High))
(72+33)/150

# Using cross validation to prune the tree
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
cv.carseats
plot(cv.carseats)
prune.carseats=prune.misclass(tree.carseats,best=13)
plot(prune.carseats)
text(prune.carseats,pretty=0)

# evaluate this pruned tree
tree.pred=predict(prune.carseats,Carseats[-train,],type="class")
with(Carseats[-train,],table(tree.pred,High))
(72+32)/150


