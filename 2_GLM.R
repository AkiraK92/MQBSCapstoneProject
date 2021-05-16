######################################################
### Data Modelling: Generalised Linear Model (GLM) ###
######################################################
set.seed(123)
trainsampleindex <- sample(c(1:nrow(data2)),0.7*nrow(data2),replace=FALSE) 
#random sample index of size 70% of total rowcounts without replacement
train <- data2[trainsampleindex,]
test <- data2[-trainsampleindex,]

###################################################
### GLM using all predictors
modelfit.glm1 <- glm(claimsNum~gender+zone+mcCl+bonCl+ownAge+vehAge,data=train, family=poisson(),offset=log(duration))

summary(modelfit.glm1) # 64548 data and 21 predictors (1 for beta_0)

train$fit.glm1 <- predict(modelfit.glm1, newdata=train, type="response")
test$fit.glm1 <- predict(modelfit.glm1, newdata=test, type="response")

in_sample_error.glm1 <- 
  2*(sum(log((train$claimsNum/train$fit.glm1)^train$claimsNum))-sum(train$claimsNum)+sum(train$fit.glm1))/nrow(train) 
out_sample_error.glm1 <- 
  2*(sum(log((test$claimsNum/test$fit.glm1)^test$claimsNum))-sum(test$claimsNum)+sum(test$fit.glm1))/nrow(test)
cat(" GLM1 in-sample error:", in_sample_error.glm1*100, "\n","GLM1 out-of-sample error:", out_sample_error.glm1*100)


###################################################
### Regularisation (LASSO)
library(glmnet)
Xdata.train <- model.matrix(~gender+zone+mcCl+bonCl+ownAge+vehAge, data=train)
Xdata.test <- model.matrix(~gender+zone+mcCl+bonCl+ownAge+vehAge, data=test)

modelfit.glm2 <- glmnet(x=Xdata.train, y=train$claimsNum, family = "poisson", alpha = 1, offset = log(train$duration))
cv.glm2 <- cv.glmnet(Xdata.train, train$claimsNum, type.measure ="deviance", 
                     family = "poisson", alpha = 1, offset = log(train$duration), nfolds = 10)

plot(cv.glm2)
abline(v=log(cv.glm2$lambda.min), lty = 5,lwd = 2, col = "darkorange")
abline(v=log(cv.glm2$lambda.1se), lty = 5,lwd = 2, col = "darkblue")
abline(h=cv.glm2$cvup[which(cv.glm2$lambda == cv.glm2$lambda.min)], lty = 2,lwd = 1, col = "red")
legend("topleft", legend = c("minimum", "min+1se"), col = c("darkorange", "darkblue"), lty = c(5,5), lwd = c(2,2))

plot(modelfit.glm2, xvar="lambda", label="TRUE")
abline(v=log(cv.glm2$lambda.min), lty = 5,lwd = 2, col = "darkorange")
abline(v=log(cv.glm2$lambda.1se), lty = 5,lwd = 2, col = "darkblue")
legend("topright", legend = c("minimum", "min+1se"), col = c("darkorange", "darkblue"), lty = c(5,5), lwd = c(2,2))

# here choose to just fit based on lambda that gives lowest CV error (i.e. the orange line = 24 parameters)
train$fit.glm2 <- 
  predict(modelfit.glm2, newx = Xdata.train, newoffset = log(train$duration), type="response", s = cv.glm2$lambda.min)
test$fit.glm2 <- 
  predict(modelfit.glm2, newx = Xdata.test, newoffset = log(test$duration), type="response", s = cv.glm2$lambda.min)

in_sample_error.glm2 <- 
  2*(sum(log((train$claimsNum/train$fit.glm2)^train$claimsNum))-sum(train$claimsNum)+sum(train$fit.glm2))/nrow(train)
out_sample_error.glm2 <- 
  2*(sum(log((test$claimsNum/test$fit.glm2)^test$claimsNum))-sum(test$claimsNum)+sum(test$fit.glm2))/nrow(test)
cat(" GLM2 in-sample error:", in_sample_error.glm2*100, "\n","GLM2 out-of-sample error:", out_sample_error.glm2*100)

###################################################
### forward and backward selection (not used in report)
#need to manually create dummy variables for step-wise regression
library(dummies)
gender.dum <- data.frame(dummy(train2$gender)[,-1])
names(gender.dum) <- c("genderK")
zone.dum <- data.frame(dummy(train2$zone)[,-1])
mcCl.dum <- data.frame(dummy(train2$mcCl)[,-1])
bonCl.dum <- data.frame(dummy(train2$bonCl)[,-1])
train2 <- train
train2 <- cbind(train2[,c(1,5,7,8)],gender.dum,zone.dum,mcCl.dum,bonCl.dum)
str(train2)
modelfit.glm.full <- glm(claimsNum~.-duration,data=train2, family=poisson(),offset=log(duration))
#check that the fit is the same as glm1
summary(modelfit.glm.full)
summary(modelfit.glm1) 

#step-wise regression
library(MASS)
step <- stepAIC(modelfit.glm.full, direction = "both", trace = TRUE)
summary(step)

test2 <- test
gender.dum <- data.frame(dummy(test2$gender)[,-1])
names(gender.dum) <- c("genderK")
zone.dum <- data.frame(dummy(test2$zone)[,-1])
mcCl.dum <- data.frame(dummy(test2$mcCl)[,-1])
bonCl.dum <- data.frame(dummy(test2$bonCl)[,-1])
test2 <- cbind(test2[,c(1,5,7,8)],gender.dum,zone.dum,mcCl.dum,bonCl.dum)
str(test2)

train$fit.glmstep <- predict(step, train2, type="response")
test$fit.glmstep <- predict(step, test2, type="response")

in_sample_error.glmstep <- 
  2*(sum(log((train$claimsNum/train$fit.glmstep)^train$claimsNum))
     -sum(train$claimsNum)+sum(train$fit.glmstep))/nrow(train)
out_sample_error.glmstep <- 
  2*(sum(log((test$claimsNum/test$fit.glmstep)^test$claimsNum))
     -sum(test$claimsNum)+sum(test$fit.glmstep))/nrow(test)
cat(" GLM Step in-sample error:", in_sample_error.glmstep*100, 
    "\n","GLM Step out-of-sample error:", out_sample_error.glmstep*100)

