######################################################
### Data Modelling: Decision Trees ###
######################################################

###################################################
### Regression Tree (using SBS) ###
# -> efficient tree with optimal cp (chosen as within 1 SE rule)

library(dplyr)
library(tidyverse)
library(readr)
library(tidyr)

require(rpart)
require(rpart.plot)
set.seed(123)
tree.fit <- rpart(cbind(duration, claimsNum) ~ gender+zone+mcCl+bonCl+ownAge+vehAge,
                            data = train,
                            method = "poisson",
                            parms = list(shrink=1),
                            control = rpart.control(xval = 10, minbucket = 1000, cp = 0.0000001))
# plot Relative error vs cp
plotcp(tree.fit, minline = T) #minline drows a horizontal line within 1SE of cp (min)
printcp(tree.fit)
summary(tree.fit)
rpart.plot(tree.fit)

min.cverr <- min(tree.fit$cp[,4])
min.cverr.sd <- tree.fit$cp[,5][which(tree.fit$cp[,4] == min.cverr)]

# create function called closest
# returns the closest point to x with the condition that x > y 
# (e.g. x = min+1se, y = all CVs then outcome is the data closest to and within min+1se)
closest <- function (x, y){ 
  dist <- ifelse(x<y,1e9,x-y)
  min.dist.row <- which(dist == min(dist))
  min.dist.row
}

min.cp <- tree.fit$cp[closest(min.cverr, tree.fit$cp[,4]),1]
min.size <- tree.fit$cp[closest(min.cverr, tree.fit$cp[,4]),2] + 1

one.se.cp <- tree.fit$cp[closest(min.cverr+min.cverr.sd, tree.fit$cp[,4]),1]
one.se.size <- tree.fit$cp[closest(min.cverr+min.cverr.sd, tree.fit$cp[,4]),2] + 1

plotcp(tree.fit, minline = T)
abline(h = min.cverr + min.cverr.sd, lty = 2,lwd = 1, col = "red")
abline(v= min.size, lty = 5,lwd = 2, col = "darkorange")
abline(v = one.se.size, lty = 5,lwd = 2, col = "darkblue")
legend("topright", legend = c("minimum", "min+1se"), col = c("darkorange", "darkblue"), lty = c(5,5), lwd = c(2,2))
cat("optimal cp baed on 1-se rule:", one.se.cp)

# plot CV error vs log(cp) (better)
plot(log(tree.fit$cp[,1]),tree.fit$cp[,4], 
     ylim=range(c(tree.fit$cp[,4]-tree.fit$cp[,5],tree.fit$cp[,4]+tree.fit$cp[,5])), #limit window to 1 se
     pch=19, xlab="log(cp)",ylab="CV error",
     main=" ", xlim = rev(range(log(tree.fit$cp[,1]))))
arrows(log(tree.fit$cp[,1]), tree.fit$cp[,4]-tree.fit$cp[,5], log(tree.fit$cp[,1]), 
       tree.fit$cp[,4]+tree.fit$cp[,5], length=0.05, angle =90, code = 3)

abline(h = min.cverr + min.cverr.sd, lty = 2,lwd = 1, col = "red")
abline(v= log(min.cp), lty = 5,lwd = 2, col = "darkorange")
abline(v = log(one.se.cp), lty = 5,lwd = 2, col = "darkblue")
legend("topright", legend = c("minimum", "min+1se"), col = c("darkorange", "darkblue"), lty = c(5,5), lwd = c(2,2))

cat("optimal cp baed on 1-se rule:", one.se.cp)


# Prune the tree at the chosen cp parameter
tree.fit.pruned <- prune(tree.fit, cp = one.se.cp)
summary(tree.fit.pruned)
rpart.plot(tree.fit.pruned)

#in-out sample error
train$fit.rt1 <- train$duration * predict(tree.fit.pruned,  newdata = train)
test$fit.rt1 <- test$duration * predict(tree.fit.pruned, newdata = test)

in_sample_error.rt1 <- 
  2*(sum(log((train$claimsNum/train$fit.rt1)^train$claimsNum))-sum(train$claimsNum)+sum(train$fit.rt1))/nrow(train)
out_sample_error.rt1 <- 
  2*(sum(log((test$claimsNum/test$fit.rt1)^test$claimsNum))-sum(test$claimsNum)+sum(test$fit.rt1))/nrow(test)
cat(" RT1 in-sample error:", in_sample_error.rt1*100, "\n","RT1 out-of-sample error:", out_sample.error.rt1*100)


###################################################
### Boosting Tree ###
# -> using 80 trees/weak learners with 1 layer-depth each (allowing for shrinking (with 0.5))
library(gbm)
oldw <- getOption("warn")
options(warn = -1) #to avoid printing warnings
set.seed(123)
K <- 80 #number of trees
idepth <- 1 #depth of each tree
fit.boost <- gbm(claimsNum~offset(log(duration))+gender+zone+mcCl+bonCl+ownAge+vehAge,
                           data=train,
                           distribution="poisson",
                           n.trees = K,
                           shrinkage = 0.5,
                           interaction.depth = idepth)
summary(fit.boost)                           
train$fit.rt2 <- train$duration
test$fit.rt2 <- test$duration
in_sample_error.rt2 <- vector()
out_sample_error.rt2 <- vector()

for (k in 1:K){
  train$fit.rt2 <- train$duration*suppressWarnings(predict(fit.boost, newdata=train, n.trees=k, type="response"))
  test$fit.rt2 <- test$duration*suppressWarnings(predict(fit.boost, newdata=test, n.trees=k, type="response"))
  in_sample_error.rt2[k] <- 
    2*(sum(log((train$claimsNum/train$fit.rt2)^train$claimsNum))-sum(train$claimsNum)+sum(train$fit.rt2))/nrow(train)
  out_sample_error.rt2[k] <- 
    2*(sum(log((test$claimsNum/test$fit.rt2)^test$claimsNum))-sum(test$claimsNum)+sum(test$fit.rt2))/nrow(test)
} #suppresswarnings since it produces a unneeded warning that predicted values don't have exposure taken into account

plot(seq(1:K),in_sample_error.rt2,xlab="iteration",ylab="gbm in-sample error")
plot(seq(1:K),out_sample_error.rt2,xlab="iteration",ylab="gbm out-of-sample error")

options(warn = oldw) #to revert back the setting on warnings

out_sample_error.rt2[K]*100
cat(" RT2 in-sample error:", in_sample_error.rt2[K]*100, "\n","RT2 out-of-sample error:", out_sample_error.rt2[K]*100)
# -> produces slightly better in and out-of-sample error out of all (especially against GLM)

######################################################
### GLM Boosting with regression tree improvements ###

# -> using 80 trees/weak learners with 1 layer-depth each (allowing for shrinking (with 0.5))
library(gbm)
oldw <- getOption("warn")
options(warn = -1) #to avoid printing warnings
set.seed(123)
K <- 80 #number of trees
idepth <- 1 #depth of each tree
fit.boost2 <- gbm(claimsNum~offset(log(train$fit.glm2))+gender+zone+mcCl+bonCl+ownAge+vehAge, 
                  #define offset as glm fitted value of number of claims
                 data=train,
                 distribution="poisson",
                 n.trees = K,
                 shrinkage = 0.5,
                 interaction.depth = idepth)
summary(fit.boost2)                           
train$fit.rt3 <- train$fit.glm2
test$fit.rt3 <- test$fit.glm2
in_sample_error.rt3 <- vector()
out_sample_error.rt3 <- vector()

for (k in 1:K){
  train$fit.rt3 <- train$fit.glm2*suppressWarnings(predict(fit.boost2, newdata=train, n.trees=k, type="response"))
  test$fit.rt3 <- test$fit.glm2*suppressWarnings(predict(fit.boost2, newdata=test, n.trees=k, type="response"))
  in_sample_error.rt3[k] <- 
    2*(sum(log((train$claimsNum/train$fit.rt3)^train$claimsNum))-sum(train$claimsNum)+sum(train$fit.rt3))/nrow(train)
  out_sample_error.rt3[k] <- 
    2*(sum(log((test$claimsNum/test$fit.rt3)^test$claimsNum))-sum(test$claimsNum)+sum(test$fit.rt3))/nrow(test)
} 

plot(seq(1:K),in_sample_error.rt3,xlab="iteration",ylab="gbm in-sample error")
plot(seq(1:K),out_sample_error.rt3,xlab="iteration",ylab="gbm out-of-sample error")

options(warn = oldw) #to revert back the setting on warnings

out_sample_error.rt3[K]*100
cat(" RT3 in-sample error:", in_sample_error.rt3[K]*100, "\n","RT3 out-of-sample error:", out_sample_error.rt3[K]*100)

# -> produces slightly better in and out-of-sample error out of all
# probably given the improvement from boosting the results base on GLM non-parametrically
# However,the OOSE will vary quite a lot (i.e. not stable), so for stable results RT1 may be better
