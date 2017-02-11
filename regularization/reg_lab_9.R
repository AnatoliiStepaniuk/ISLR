library(ISLR)
library(leaps)
set.seed(1)

# (a)
train_ind <- sample(nrow(College), size = 0.7*nrow(College))
test_ind <- (1:nrow(College))[-train_ind]

# (b)
glm.fit <- glm(Apps ~ ., data = College, subset = train_ind)
glm.predict <- predict(glm.fit, newdata = College[test_ind, ], type = "response")
glm.fit.summary <- summary(glm.fit)
glm.error <- mean((College[test_ind,]$Apps - glm.predict)^2)

# (c)
mat <- model.matrix(Apps ~ ., data = College)[,-1]
lambda.grid <- 10^seq(10,-2, length=100)

cv.ridge <- cv.glmnet(y = College$Apps, x = mat, alpha = 0, lambda = lambda.grid)
bestlam.ridge <- cv.ridge$lambda.min
ridge.fit <- glmnet(y = College$Apps[train_ind], x = mat[train_ind, ], alpha = 0, lambda = bestlam.ridge)
ridge.pred <- predict(ridge.fit, newx = mat[test_ind,], type = "response", s = bestlam.ridge)
ridge.error <- mean((College$Apps[test_ind] - ridge.pred)^2)

# (d)
cv.lasso <- cv.glmnet(y = College$Apps, x = mat, alpha = 1, lambda = lambda.grid)
bestlam.lasso <- cv.lasso$lambda.min
lasso.fit <- glmnet(y = College$Apps[train_ind], x = mat[train_ind,], alpha = 1, lambda = bestlam.lasso)
lasso.pred <- predict(lasso.fit, newx = mat[test_ind,], s=bestlam.lasso, type="response")
lasso.error <- mean((College$Apps[test_ind] - lasso.pred)^2)

# (e)
pcr.fit <- pcr(Apps ~ ., data = College, subset = train_ind, scale = TRUE, validation = "CV")
pcr.pred <- predict(pcr.fit, newdata = mat[test_ind,], ncomp = 17)
pcr.error <- mean((College$Apps[test_ind] - pcr.pred)^2)

# (f)
pls.fit <- plsr(Apps ~ ., data = College, subset = train_ind, scale = TRUE, validation = "CV")
pls.pred <- predict(pls.fit, newdata = mat[test_ind,], ncomp = 8)
pls.error <- mean((College$Apps[test_ind] - pls.pred)^2)



