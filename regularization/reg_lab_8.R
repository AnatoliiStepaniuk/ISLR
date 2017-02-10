library(ISLR)
library(leaps)
library(glmnet)

# (a)
set.seed(1)
X <- rnorm(100)
noise <- 10 * rnorm(100)
b0 = 17
b1 = 2
b2 = 4
b3 = 7

# (b)
Y <- b0 + b1*X + b2*X^2 + b3*X^3 + noise
data.full <- data.frame(y = Y, x = X)

# (c)
regfit.exhaustive <- regsubsets(Y ~ poly(X,10,raw=TRUE), data = data.full, nvmax = 10, method = "exhaustive")
regfit.exhaustive.summary <- summary(regfit.exhaustive)

subsets.exhaustive.min_cp <- which.min(regfit.exhaustive.summary$cp)
subsets.exhaustive.max_adjr2 <- which.max(regfit.exhaustive.summary$adjr2)
subsets.exhaustive.min_bic <- which.min(regfit.exhaustive.summary$bic)
subsets.exhaustive.min_rss <- which.min(regfit.exhaustive.summary$rss)

par(mfrow=c(2,2))
plot(regfit.exhaustive.summary$cp, xlab="Subset Size", ylab = "Cp", pch=20, type="l")
points(subsets.exhaustive.min_cp, regfit.exhaustive.summary$cp[subsets.exhaustive.min_cp], col="red", lwd = 7, pch=25)

plot(regfit.exhaustive.summary$adjr2, xlab="Subset Size", ylab = "Adjusted R2", pch=20, type="l")
points(subsets.exhaustive.max_adjr2, regfit.exhaustive.summary$adjr2[subsets.exhaustive.max_adjr2], col="red", lwd = 7, pch=25)

plot(regfit.exhaustive.summary$bic, xlab="Subset Size", ylab = "Bic", pch=20, type="l")
points(subsets.exhaustive.min_bic, regfit.exhaustive.summary$bic[subsets.exhaustive.min_bic], col="red", lwd = 7, pch=25)

plot(regfit.exhaustive.summary$rss, xlab="Subset Size", ylab = "RSS", pch=20, type="l")
points(subsets.exhaustive.min_rss, regfit.exhaustive.summary$rss[subsets.exhaustive.min_rss], col="red", lwd = 7, pch=25)
title("Exhaustive Subsets", outer=TRUE)

# based on plots analysis
best_size = 3

# (d)
regfit.forward <- regsubsets(Y ~ poly(X,10,raw=TRUE), data = data.full, nvmax = 10, method = "forward")
regfit.forward.summary <- summary(regfit.forward)

subsets.forward.min_cp <- which.min(regfit.forward.summary$cp)
subsets.forward.max_adjr2 <- which.max(regfit.forward.summary$adjr2)
subsets.forward.min_bic <- which.min(regfit.forward.summary$bic)
subsets.forward.min_rss <- which.min(regfit.forward.summary$rss)

par(mfrow=c(2,2))
plot(regfit.forward.summary$cp, xlab="Subset Size", ylab = "Cp", pch=20, type="l")
points(subsets.forward.min_cp, regfit.forward.summary$cp[subsets.forward.min_cp], col="red", lwd = 7, pch=25)

plot(regfit.forward.summary$adjr2, xlab="Subset Size", ylab = "Adjusted R2", pch=20, type="l")
points(subsets.forward.max_adjr2, regfit.forward.summary$adjr2[subsets.forward.max_adjr2], col="red", lwd = 7, pch=25)

plot(regfit.forward.summary$bic, xlab="Subset Size", ylab = "Bic", pch=20, type="l")
points(subsets.forward.min_bic, regfit.forward.summary$bic[subsets.forward.min_bic], col="red", lwd = 7, pch=25)

plot(regfit.forward.summary$rss, xlab="Subset Size", ylab = "RSS", pch=20, type="l")
points(subsets.forward.min_rss, regfit.forward.summary$rss[subsets.forward.min_rss], col="red", lwd = 7, pch=25)
title("Forward Subsets", outer=TRUE)

print("Exhaustive coefficients:")
coef(regfit.exhaustive, subsets.exhaustive.min_cp)
coef(regfit.exhaustive, subsets.exhaustive.min_bic)

print("Forward coefficients:")
coef(regfit.forward, subsets.forward.min_cp)
coef(regfit.forward, subsets.forward.min_bic)

# (e)
x <- model.matrix(Y ~ poly(X, 10, raw=TRUE), data.full)[,-1]
lasso.mod <- glmnet(y = Y, x = x, alpha = 1)
lasso.mod$lambda[1] # shows lambda for this array entry
coef(lasso.mod)[,1] # shows predictors coeffs for this array entry

cv.result <- cv.glmnet(x = x, y = Y, alpha = 1)
par(mfrow=c(1,1))
plot(cv.result)
bestlam =cv.result$lambda.min

# (f)
b7 = 0.8
Y <- b0 + b7 * X^7 + rnorm(100)

subsets7 <- regsubsets(Y~poly(X,10,raw = TRUE), nvmax = 10, data = data.full)
subsets7.summary <- summary(subsets7)

lasso.mod7 <- glmnet(y = Y, x = x, alpha=1)

