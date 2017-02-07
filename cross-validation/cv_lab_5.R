library("ISLR")
library("boot")
library("MASS")
frame <- Default
frame$default <- ifelse(Default$default == "Yes", 1, 0)
attach(frame)

# (a)
glm.fit <- glm(default ~ balance + income, family="binomial")
glm.probs.all <- predict(glm.fit, frame, type="response")
glm.pred.all <- rep(0,nrow(frame))
glm.pred.all[glm.probs.all >= 0.5] <- 1
table(glm.pred.all, frame$default)
mean(glm.pred.all != frame$default)

# (b)
getTestError <- function(train_ind){
  glm.fit.train <- glm(default ~ balance + income, subset = train_ind, family="binomial")
  glm.probs.test <- predict(glm.fit, frame[-train_ind,], type="response")
  glm.pred.test <- rep(0, nrow(frame[-train_ind,]))
  glm.pred.test[glm.probs.test >= 0.5] <- 1
  return(mean(glm.pred.test != frame$default[-train_ind]))
}

set.seed(0)
train_ind <- sample(nrow(frame), floor(0.8*nrow(frame)))
testError <- getTestError(train_ind)

# (c)
set.seed(1)
train_ind1 <- sample(nrow(frame), floor(0.8*nrow(frame)))
set.seed(2)
train_ind2 <- sample(nrow(frame), floor(0.8*nrow(frame)))
set.seed(3)
train_ind3 <- sample(nrow(frame), floor(0.8*nrow(frame)))

testError1 <- getTestError(train_ind1)
testError2 <- getTestError(train_ind2)
testError3 <- getTestError(train_ind3)

