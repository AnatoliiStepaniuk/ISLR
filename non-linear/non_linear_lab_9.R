library(ISLR)
library(MASS)
library(boot)
set.seed(1)

# (a2)
lm.fit <- lm(nox~poly(dis,3), data = Boston)
sorted <- sort(Boston$dis, index.return=TRUE)
sorted.dis <- sorted$x
sorted.dis.ind <- sorted$ix

lm.pred <- predict(lm.fit, newdata=list(dis=sorted.dis), se.fit=TRUE, type="response")
par(mfrow=c(1,1))
plot(x=Boston$dis, y=Boston$nox, ylab = "NO2 level", xlab = "Distance to employment centers", col="grey")
lines(x=sorted.dis, y=lm.pred$fit, type="l", col="red")
title("NO2 level by distance")

# (b)
plot(x=Boston$dis, y=Boston$nox, ylab = "NO2 level", xlab = "Distance to employment centers", col="grey")
all.rss = list()
glm.all.fits = list()
all.deltas = rep(NA,10)
for(i in 1:10){
  glm.all.fits[[i]] <- glm(nox~poly(dis,i), data = Boston)
  lm.pred <- predict(glm.all.fits[[i]], newdata=list(dis=sorted.dis), se.fit=TRUE, type="response")
  lines(x=sorted.dis, y=lm.pred$fit, type="l", col=i, lwd=2)
  all.rss[[i]] <- sum((Boston$dis[sorted.dis.ind] - lm.pred$fit)^2)  
  all.deltas[i] <- cv.glm(Boston, glmfit=glm.all.fits[[i]],K=10)$delta[2]
}
legend("topright",title = "Polynomial fit of degree:", col = 1:10, legend = 1:10, ncol = 2, cex = 0.7, lty=1, lwd=2)
title("NO2 level by distance")

# (c)
lm.all.fits <- list()
for(i in 1:10){
  lm.all.fits[[i]] <- lm(nox~poly(dis,i), data = Boston)
}
anova(lm.all.fits[[1]],lm.all.fits[[2]],lm.all.fits[[3]],lm.all.fits[[4]],lm.all.fits[[5]],lm.all.fits[[6]],lm.all.fits[[7]],lm.all.fits[[8]],lm.all.fits[[9]],lm.all.fits[[10]])

plot(1:10,all.deltas,xlab="Degree", ylab = "CV error", type="l", lwd=2)
title("Poly degree cross validation")

# (d)
# quadratic growth
knots <- c(1.653262,2.351478,3.74791,6.540773,12.1265)
fit.sp <- glm(nox ~ bs(dis,knots=knots),data = Boston)
pred.sp <- predict(fit.sp, newdata=list(dis=sorted.dis), se.fit=TRUE)
par(mfrow=c(1,1))
plot(x=Boston$dis, y=Boston$nox, col="gray")
lines(sorted.dis, pred.sp$fit, lwd=2)
lines(sorted.dis, pred.sp$fit+2*pred.sp$se.fit, lwd=2, lty="dashed")
lines(sorted.dis, pred.sp$fit-2*pred.sp$se.fit, lwd=2, lty="dashed")
title("Spline approximation")