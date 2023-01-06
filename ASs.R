library(MASS)
head(Boston)

str(Boston)

attach(Boston)



fit<-lm(data = Boston,medv~lstat+age)


X<-model.matrix(fit)
X


head(X,10)



XT<-t(X)
y<-Boston$medv




beta<-solve(XT%*%X)%*%XT%*%y
beta

summary(fit)


y_h<-X%*%beta
head(y_h,10)
head(y,10)

y
(SSE<-t(y-y_h)%*%(y-y_h))

length(Boston)

str(Boston)


sqrt(SSE/(506 - 3))
v<-SSE/(506 - 3)

var(y-y_h)
var(y)

solve(XT%*%X)

VarB<-var(y-y_h)[1]*solve(XT%*%X)
VarB

RSE<-sqrt(SSE/(506 - 3))
RSE

RSE<-sqrt(SSE/(506 - 3))
VarB<-RSE[1]^2*solve(XT%*%X)
VarB



var(y)

VarBB<-84.58672*solve(XT%*%X)
VarBB


var(y-y_h)
solve(XT%*%X)
37.95669*solve(XT%*%X)


var(y-y_h)[1]*solve(XT%*%X)




summary(fit)
for(i in 1:3){
  print(sqrt(VarBB[i,i]))
}

beta[1]

VarB

for(i in 1:3){
  x=sqrt(VarB[i,i])
  d = qt(0.975,506-3)
  print(c(beta[i]-d*x,beta[i]+x*d))
}


str(y)

rep(1,5)

ybar<-mean(y)*rep(1,506)
SST<-t(y-ybar)%*%(y-ybar);SST
SSR<-t(y_h-ybar)%*%(y_h-ybar);SSR

42716.3 - 23548.17


SST-SSR
SSE










aov()
summary(fit)

var(y)


varbv<-rep(1,3)
for(i in 1:3){
  varbv[i]=varbv[i]*(sqrt(VarB[i,i]))
}


print(varbv)





varbv<-rep(1,3)
for(i in 1:3){
  varbv[i]=varbv[i]*(sqrt(VarB[i,i]))
}

for(i in 1:3){
  t=(beta[i]-0)/varbv[i]
  if(t<0){
    prt = pt(beta[i]/varbv[i], 503,lower.tail = T)*2
    print(c(t,prt))
  }
  if(t>0){
    prt = pt(beta[i]/varbv[i], 503,lower.tail = F)*2
    print(c(t,prt))
  }

}
summary(fit)$coeff

4.554816e+01

pt()
pt(4.554816e+01,)


summary(fit)

print(c(1,2))

beta[3]/varbv[3]


beta[2]/varbv[2]

pt(beta[2]/varbv[2], 505,lower.tail = T)*2



SSR<-(y_h - )


(SSR/2)/(SSE/(503))
pf((SSR/2)/(SSE/(503)),2,503,lower.tail = F)



SSE
SST-SSR

summary(fit)

(R2<-SSR/SST)
(adjR2<- 1-(SSE/(506-3))/(SST/(506-1)))

n<-506
p<-3
(SSE/(n-p))/(SST/(n-1))
1-(SSE/(n-p))/(SST/(n-1))

summary(fit)


1-(1-R2^2)*(505)/(505-3)



model<-lm(data = Boston,medv~lstat+age)



model.matrix(model)[1:10,]
summary(model)$coef

summary(model)$sigma
sqrt(SSE/(506 - 3))

(R2<-SSR/SST)



length(y)

(SSR/(3-1))/(SSE/(506-3))
pf((SSR/(3-1))/(SSE/(506-3)), 3-1, 506-3,lower.tail = F)


summary(model)$fstatistic



confint(model)

sqrt(SSE/(506-3))


vcov(model)
VarB

model<-lm(data = Boston,medv~lstat+age)



m1<-lm(data = Boston,medv~1)
m2<-lm(data = Boston,medv~lstat)
m3<-lm(data = Boston,medv~lstat+age)

anova(m1,m2,m3)

AIC(m1,m2,m3)
BIC(m1,m2,m3)


c(summary(m1)$r.squared,summary(m2)$r.squared,summary(m3)$r.squared)
c(summary(m1)$adj.r.squared,summary(m2)$adj.r.squared,summary(m3)$adj.r.squared)

mean(Boston$lstat)

new<-data.frame(lstat = c(mean(Boston$lstat)), age = c(mean(Boston$age)))
predict(model, newdata = new, interval="confidence")
predict(model, newdata = new, interval="prediction")
