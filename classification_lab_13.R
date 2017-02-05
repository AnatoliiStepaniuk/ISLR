library("MASS")

getLdaError <- function(train, test){
  lda.fit <- lda(crim ~ ., data = train)
  lda.pred <- predict(lda.fit, test)
  lda.error <- mean(lda.pred$class != test$crim)
  return(lda.error)  
}

getQdaError <- function(train, test){
  qda.fit <- qda(crim ~ ., data = train)
  qda.pred <- predict(qda.fit, test)
  qda.error <- mean(qda.pred$class != test$crim)
  return(qda.error)
}

getGlmError <- function(train, test){
  glm.fit <- glm(crim ~ ., data = train, family = "binomial")
  glm.probs <- predict(glm.fit, test)
  glm.pred <- rep(times = nrow(test), 0)
  glm.pred[glm.probs > 0.5] <- 1
  glm.error <- mean(glm.pred != test$crim)
  return(glm.error)
}

getKnnError <- function(k, train, test){
  crim_ind <- which(colnames(train)=="crim")
  knn.pred <- knn(scale(train[, -crim_ind]), scale(test[, -crim_ind]), train[, crim_ind], k=k)
  knn.error <- mean(knn.pred != test$crim)
  return(knn.error)
}

getMinError <- function(names){
  set.seed(1)
  allNames <- c("crim", names)
  frame <- Boston[allNames]
  frame$crim <- ifelse(Boston$crim > median(Boston$crim), 1, 0) 
  
  train_size <- floor(0.75 * nrow(frame))
  train_ind <- sample.int(n = nrow(frame), size = train_size)
  train <- frame[train_ind, ]
  test <- frame[-train_ind, ]

  lda.error <- getLdaError(train, test)
  qda.error <- getQdaError(train, test)
  glm.error <- getGlmError(train, test)
  knn.errors <- unlist(lapply(1:5, getKnnError, train, test))

  min.error <- min(c(lda.error, qda.error, glm.error, knn.errors))
  return(min.error)
}

allColumns <- c("tax", "rad", "lstat", "nox", "indus")
bestCombination <- allColumns
bestError <- 1

for(subset_size in 1:length(allColumns)){
  sets <- combn(x = allColumns, subset_size, simplify = FALSE)
  errors <- unlist(lapply(sets, getMinError))
  minIndex <- which(errors == min(errors))
  if(min(errors) < bestError){
    bestError <- min(errors)
    bestCombination <- unlist(sets[minIndex])
  }
}

print(bestCombination)
print(bestError)

