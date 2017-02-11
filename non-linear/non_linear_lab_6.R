library(ISLR)
set.seed(1)
# (a)
all.deltas <- rep(NA, 10)
for(i in 1:10){
  glm.fit <- glm(wage~poly(age,i), data=Wage)
  all.deltas[i] <- cv.glm(glm.fit, data=Wage, K=10)$delta[2]
}
min.delta <- min(all.deltas)
d <- which.min(all.deltas)

par(mfrow=c(1,1))
plot(x=1:10,y=all.deltas,col="red",type="l",ylab = "CV error",xlab="Polynomial degree")
title("Choosing polynomial degree")

fit1 <- lm(wage~poly(age,1), data=Wage)
fit2 <- lm(wage~poly(age,2), data=Wage)
fit3 <- lm(wage~poly(age,3), data=Wage)
fit4 <- lm(wage~poly(age,4), data=Wage)
fit5 <- lm(wage~poly(age,5), data=Wage)
fit6 <- lm(wage~poly(age,6), data=Wage)
fit7 <- lm(wage~poly(age,7), data=Wage)
fit8 <- lm(wage~poly(age,8), data=Wage)
fit9 <- lm(wage~poly(age,9), data=Wage)
fit10 <- lm(wage~poly(age,10), data=Wage)
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit6,fit7,fit8,fit9,fit10)
# Anova proved that CV determined polynom degree = 4 is a reasonable value

# b
all.step.deltas <- rep(NA, 9)
for(i in 2:10){
  Wage$age.cut <- cut(age,i)
  fit.step <- glm(wage~age.cut,data = Wage)
  all.step.deltas[i-1] <- cv.glm(Wage, fit.step,K=10)$delta[2]
}
Wage$age.cut<-NULL
best.cut.n <- which.min(all.step.deltas)

fit.step <- glm(wage~cut(age,best.cut.n),data = Wage)
age.range <- range(Wage$age)
age.grid <- (age.range[1]:age.range[2])
pred.step <- predict(fit.step, data.frame(age=age.grid)) 
plot(Wage$age, Wage$wage,col="darkgrey")
lines(age.grid, pred.step, type = "l", col="red")




