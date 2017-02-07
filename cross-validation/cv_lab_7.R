library("ISLR")
library("boot")
library("MASS")

frame <- Weekly
frame$Direction <- ifelse(Weekly$Direction == "Up", 1, 0)
attach(frame)

# (a)
glm.fit.all <- glm(Direction ~ Lag1 + Lag2)
glm.prob.all <- predict(glm.fit.all, frame, type = "response")
glm.pred.all <- ifelse(glm.prob.all > 0.5, 1, 0)
mean(glm.pred.all != frame$Direction)

# (b)
glm.fit.1 <- glm(Direction ~ Lag1 + Lag2, subset = 2:nrow(frame))
# (c)
glm.prob.1 <- predict(glm.fit.1, frame[1,], type="response")
glm.pred.1 <- ifelse(glm.prob.1 > 0.5, 1, 0)

# (d,e)
predictionError <- rep(0,nrow(frame))
for(i in 1:nrow(frame)){
  glm.fit.i <- glm(Direction ~ Lag1 + Lag2, subset = (1:nrow(frame))[-i])
  glm.prob.i <- predict(glm.fit.i, frame[i,], type="response")
  glm.pred.i <- ifelse(glm.prob.i > 0.5, 1, 0)
  if(glm.pred.i != frame$Direction[i]){
    predictionError[i] = 1
  }
}

my_LOOCV_error <- mean(predictionError)
LOOCV_error2 <- cv.glm(frame, glm.fit.all)$delta[1]



