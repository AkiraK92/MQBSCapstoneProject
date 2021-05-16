###################################################
### Modelling using K-Means Clustering ###
###################################################
### K-means clustering
# set up
#data1 stores original data after cleansing (but before re-leveling)
data3 <- data1
data3$gender <- as.numeric(data3$gender)
data3 <- data.frame(data3)
str(data3)
x.cluster <- as.matrix(data3[,!(names(data3) %in% c("claimsNum","duration"))])

# compute WSS for K-means clustering
wss <- (nrow(x.cluster)-1)*sum(apply(x.cluster,2,var))

set.seed(123)
for (i in 2:20) wss[i] <- sum(kmeans(x.cluster,i, nstart=10)$withinss) #store the wss for k=2-15
plot(1:20, wss, type="b", xlab = "Number of Clusters", ylab = "Within Groups Sum of Squared (WSS)")
 

#check with Gap Statistics
### Gap Statistic Method
library(plyr)
library(ggplot2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# build gap statistic (adopted from https://github.com/echen/gap-statistic)
gap_statistic <-  function(data, min_num_clusters = 1, max_num_clusters = 20, num_reference_bootstraps = 10){
  num_clusters <-  min_num_clusters:max_num_clusters
  actual_dispersions <- maply(num_clusters, function(n) dispersion(data, n))
  ref_dispersions <- maply(num_clusters, function(n) reference_dispersion(data, n, num_reference_bootstraps))
  mean_ref_dispersions <- ref_dispersions[,1]
  stddev_ref_dispersions <- ref_dispersions[,2]
  gaps <- mean_ref_dispersions - actual_dispersions
  
  print(plot_gap_statistic(gaps, stddev_ref_dispersions, num_clusters))
  
  print(paste("The estimated numnber of clusters is ", num_clusters[which.max(gaps)],".", sep = ""))
  
  list(gaps = gaps, gap_stddevs = stddev_ref_dispersions)
}

# Plot the gaps along with error bars.
plot_gap_statistic = function(gaps, stddevs, num_clusters) {
  qplot(num_clusters, gaps, xlab = "# clusters", ylab = "gap", geom = "line", 
        main = "Estimating the number of clusters via the gap statistic") + 
    geom_errorbar(aes(num_clusters, ymin = gaps - stddevs, ymax = gaps + stddevs), 
                  size = 0.3, width = 0.2, colour = "darkblue")
}

# Calculate log(sum_i(within-cluster_i sum of squares around cluster_i mean)).
dispersion = function(data, num_clusters) {
  # R's k-means algorithm doesn't work when there is only one cluster.
  if (num_clusters == 1) {
    cluster_mean = aaply(data, 2, mean)
    distances_from_mean = aaply((data - cluster_mean)^2, 1, sum)
    log(sum(distances_from_mean))
  } else {  
    # Run the k-means algorithm `nstart` times. Each run uses at most `iter.max` iterations.
    k = kmeans(data, centers = num_clusters, nstart = 10, iter.max = 50)
    # Take the sum, over each cluster, of the within-cluster sum of squares around the cluster mean. 
    # Then take the log. This is `W_k` in TWH's notation.
    log(sum(k$withinss))
  }
}

# For an appropriate reference distribution (in this case, uniform points in the same range as `data`), 
# simulate the mean and standard deviation of the dispersion.
reference_dispersion = function(data, num_clusters, num_reference_bootstraps) {
  dispersions = maply(1:num_reference_bootstraps, function(i) dispersion(generate_uniform_points(data), num_clusters))
  mean_dispersion = mean(dispersions)
  stddev_dispersion = sd(dispersions) / sqrt(1 + 1 / num_reference_bootstraps) 
  # the extra factor accounts for simulation error
  c(mean_dispersion, stddev_dispersion)
}

# Generate uniform points within the range of `data`.
generate_uniform_points = function(data) {
  # Find the min/max values in each dimension, so that we can generate uniform numbers in these ranges.
  mins = aaply(data, 2, min)
  maxs = aaply(data, 2, max)
  
  num_datapoints = nrow(data)
  # For each dimension, generate `num_datapoints` points uniformly in the min/max range.
  uniform_pts = maply(1:length(mins), function(dim) runif(num_datapoints, min = mins[dim], max = maxs[dim]))
  uniform_pts = t(uniform_pts)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run gap statistic
gap_statistic(data=x.cluster, min_num_clusters = 1, max_num_clusters = 20, num_reference_bootstraps = 10)
# -> clarifies that the optimal number of clusters is two

###################################################
### Fit K-means with two clusters
fit.kmeans <- kmeans(x.cluster, 2, nstart=10, iter.max=50)
prop.table(table(fit.kmeans$cluster))
data3$cluster <- fit.kmeans$cluster
train$cluster <- data3$cluster[trainsampleindex]
test$cluster <- data3$cluster[-trainsampleindex]

###################################################
### GLM (improved with cluster)
modelfit.glm3 <- glm(claimsNum~gender+zone+mcCl+bonCl+ownAge+vehAge+as.factor(cluster),
                     data=train, family=poisson(),offset=log(duration))
summary(modelfit.glm3)
train$fit.glm3 <- predict(modelfit.glm3, newdata=train, type="response")
test$fit.glm3 <- predict(modelfit.glm3, newdata=test, type="response")

in_sample_error.glm3 <- 
  2*(sum(log((train$claimsNum/train$fit.glm3)^train$claimsNum))-sum(train$claimsNum)+sum(train$fit.glm3))/nrow(train)
out_sample_error.glm3 <- 
  2*(sum(log((test$claimsNum/test$fit.glm3)^test$claimsNum))-sum(test$claimsNum)+sum(test$fit.glm3))/nrow(test)
cat(" GLM3 in-sample error:", in_sample_error.glm3*100, "\n","GLM3 out-of-sample error:", out_sample_error.glm3*100)

###################################################
### GLM LASSO (improved with cluster)
library(glmnet)
Xdata.train <- model.matrix(~gender+zone+mcCl+bonCl+ownAge+vehAge+as.factor(cluster), data=train)
Xdata.test <- model.matrix(~gender+zone+mcCl+bonCl+ownAge+vehAge+as.factor(cluster), data=test)

modelfit.glm4 <- glmnet(x=Xdata.train, y=train$claimsNum, family = "poisson", alpha = 1, offset = log(train$duration))
cv.glm4 <- cv.glmnet(Xdata.train, train$claimsNum, type.measure ="deviance", 
                     family = "poisson", alpha = 1, offset = log(train$duration), nfolds = 10)
plot(cv.glm4)

train$fit.glm4 <- predict(modelfit.glm4, newx=Xdata.train, newoffset = log(train$duration), 
                          type="response", s = cv.glm4$lambda.min)
test$fit.glm4 <- predict(modelfit.glm4, newx=Xdata.test, newoffset = log(test$duration), 
                         type="response", s = cv.glm4$lambda.min)

in_sample_error.glm4 <- 
  2*(sum(log((train$claimsNum/train$fit.glm4)^train$claimsNum))-sum(train$claimsNum)+sum(train$fit.glm4))/nrow(train)
out_sample_error.glm4 <- 
  2*(sum(log((test$claimsNum/test$fit.glm4)^test$claimsNum))-sum(test$claimsNum)+sum(test$fit.glm4))/nrow(test)

cat(" GLM4 in-sample error:", in_sample_error.glm4*100, "\n","GLM4 out-of-sample error:", out_sample_error.glm4*100)

# -> no improvement against benchmark GLM fits
