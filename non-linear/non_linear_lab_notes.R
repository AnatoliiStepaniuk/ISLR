library(ISLR)
library(splines)
library(gam)
library(akima) # for 2D local regression with interactions plots
attach(Wage)

fit=lm(wage~poly(age, 4), data=Wage)

agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2])
preds<-predict(fit, newdata=list(age=age.grid), se.fit=TRUE)
se.bands <- cbind(preds$fit - 2*preds$se.fit,preds$fit + 2*preds$se.fit)

par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Degree-4 Polynomial", outer = TRUE)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Testing hypothesis that a more complex model M2 is needed instead of M1
fit.1<-lm(wage~poly(age,1),data=Wage)
fit.2<-lm(wage~poly(age,2),data=Wage)
fit.3<-lm(wage~poly(age,3),data=Wage)
fit.4<-lm(wage~poly(age,4),data=Wage)
fit.5<-lm(wage~poly(age,5),data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

table(cut(age,4))

# Splines
fit.sp <- lm(wage ~ bs(age,knots=c(24,40,60)), data = Wage)
pred.sp <- predict(fit.sp, newdata=list(age=age.grid), se.fit=TRUE)
par(mfrow=c(1,1))
plot(age, wage, col="gray")
lines(age.grid, pred.sp$fit, lwd=2)
lines(age.grid, pred.sp$fit+2*pred.sp$se.fit, lwd=2, lty="dashed")
lines(age.grid, pred.sp$fit-2*pred.sp$se.fit, lwd=2, lty="dashed")

# natural spline
fit.nsp <- lm(wage~ns(age,df = 4), data = Wage)
pred.nsp <- predict(fit.nsp, newdata=list(age=age.grid), se=TRUE)

# smooth spline
fit.smsp <- smooth.spline(age,wage,df=16)
fit.smsp2 <- smooth.spline(age,wage,cv=TRUE)
fit.smsp2$df # effective degrees of freedom
plot(age,wage,xlim=agelims,cex=0.5,col="darkgrey")
title("Smoothing Spline")
lines(fit.smsp,col="red",lwd=2)
lines(fit.smsp2,col="blue",lwd=2)
legend("topright",legend = c("16 DF", "6.8 DF"), col=c("red","blue"),lty = 1,lwd = 2,cex = 0.8)

# GAM
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education,data = Wage)
# s stands for smoothing spline
gam.m3 <- gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE, col="blue")
plot.gam(gam1,se=TRUE,col="red")
# using anova to determine how should we fit year:
gam.m1<-gam(wage~s(age,5)+education,data = Wage)
gam.m2<-gam(wage~year+s(age,5)+education,data = Wage)
anova(gam.m1, gam.m2, gam.m3)

summary(gam.m3)

# GAM predictions
pred.gam <- predict(gam.m2, newdata = Wage)

# Local regression:
gam.lo <- gam(wage~s(year,df=4)+lo(age,span=0.7)+education, data=Wage)
par(mfrow=c(1,1))
plot.gam(gam.lo, se=TRUE, col="green")
# create interactions before GAM:
gam.lo.i <- gam(wage~lo(age,year,span=0.5)+education, data=Wage)
plot(gam.lo.i)

# can use family=binomial for logistic regression GAM
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")
table(education,I(wage>250))
#looks like there are not high earners with education < HS, so we can avoid those
gam.lr.s<-gam(I(wage>250)~year+s(age,df=5)+education,family="binomial",data = Wage,subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=TRUE,col="orange")





