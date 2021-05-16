######################################################
### Data Modelling: Neural Network (NN) ###
######################################################
### Use the same training and testing splits from the original
### normalise (categorical values already live in the [0,1] space so no need)
ownAge.min <- min(data2$ownAge)
ownAge.max <- max(data2$ownAge)
vehAge.min <- min(data2$vehAge)
vehAge.max <- max(data2$vehAge)


feature.train <- cbind(scale(train$ownAge,center=ownAge.min,scale=ownAge.max-ownAge.min),
                       scale(train$vehAge,center=vehAge.min,scale=vehAge.max-vehAge.min))
Xdata.train <- list(as.matrix(cbind(model.matrix(~gender+zone+mcCl+bonCl, data=train)[,-1],feature.train)), 
                    as.matrix(log(train$duration)))  

feature.test<-cbind(scale(test$ownAge,center=ownAge.min,scale=ownAge.max-ownAge.min),
                    scale(test$vehAge,center=vehAge.min,scale=vehAge.max-vehAge.min))
Xdata.test <- list(as.matrix(cbind(model.matrix(~gender+zone+mcCl+bonCl, data=test)[,-1],feature.test)), 
                   as.matrix(log(test$duration)))  

Ydata.train <- as.matrix(train$claimsNum)
Ydata.test <- as.matrix(test$claimsNum)

library(keras)
library(tensorflow)
library(tfdatasets)
library(tidyverse)

###################################################
### Shallow NN with 50 neurons (with 21 original covariates)
# build model
set_random_seed(123)
q0 <- 21 # no of covariates
q1 <- 50 # no of neurons
lambda0 <- sum(train$claimsNum)/sum(train$duration)

Design <- layer_input(shape=c(q0),dtype='float32', name='Design') #size of input
LogVol <- layer_input(shape=c(1),dtype='float32', name='LogVol') #for exposure/volume adj

Network = Design %>% 
  layer_dense(units=q1, activation='tanh',name='Layer1') %>% 
  layer_dense(units=1, activation='linear', name= 'Network', 
              weights=list(array(0, dim=c(q1,1)),array(log(lambda0), dim=c(1))))

Response = list(Network,LogVol) %>% 
  layer_add(name='Add') %>% 
  layer_dense(units=1,activation=k_exp,name='Response',trainable = FALSE, 
              weights = list(array(1,dim=c(1,1)),array(0,dim=c(1))))

# fit
modelfit.nn1 <- keras_model(inputs = c(Design, LogVol), outputs = c(Response))
modelfit.nn1 %>% compile(optimizer = optimizer_nadam(), loss = 'poisson')
summary(modelfit.nn1) #shows number of parameters to be estimated/trained

history.nn1 <- modelfit.nn1 %>% 
  fit(Xdata.train, Ydata.train, epochs=30,batch_size=nrow(Xdata.train),view_metrics = TRUE,
                         validation_data = list(Xdata.test, Ydata.test), verbose=0)
plot(history.nn1)

train$fit.nn1  <- modelfit.nn1 %>% predict(Xdata.train)
test$fit.nn1 <- modelfit.nn1 %>% predict(Xdata.test)

# in-out sample error
in_sample_error.nn1 <- 
  2*(sum(log((Ydata.train/train$fit.nn1)^Ydata.train))-sum(Ydata.train)+sum(train$fit.nn1))/nrow(Ydata.train)
out_sample_error.nn1 <- 
  2*(sum(log((Ydata.test/test$fit.nn1)^Ydata.test))-sum(Ydata.test)+sum(test$fit.nn1))/nrow(Ydata.test)
cat(" NN1 in-sample error =", in_sample_error.nn1*100, "\n", "NN1 out-sample error =", out_sample_error.nn1*100)


###################################################
### Deep NN (2 layers with 50 and 30 neurons each)
feature.train <- cbind(scale(train$ownAge,center=ownAge.min,scale=ownAge.max-ownAge.min),
                       scale(train$vehAge,center=vehAge.min,scale=vehAge.max-vehAge.min))
Xdata.train <- list(as.matrix(cbind(model.matrix(~gender+zone+mcCl+bonCl, data=train)[,-1],feature.train)), 
                    as.matrix(log(train$duration)))  

feature.test<-cbind(scale(test$ownAge,center=ownAge.min,scale=ownAge.max-ownAge.min),
                    scale(test$vehAge,center=vehAge.min,scale=vehAge.max-vehAge.min))
Xdata.test <- list(as.matrix(cbind(model.matrix(~gender+zone+mcCl+bonCl, data=test)[,-1],feature.test)), 
                   as.matrix(log(test$duration)))  

Ydata.train <- as.matrix(train$claimsNum)
Ydata.test <- as.matrix(test$claimsNum)

# build model
set_random_seed(123)
q0 <- 21 # no of covariates
q1 <- 50 # no of neurons (1st layer)
q2 <- 25 # no of neurons (2nd layer)
lambda0 <- sum(train$claimsNum)/sum(train$duration)

Design <- layer_input(shape=c(q0),dtype='float32', name='Design') #size of input
LogVol <- layer_input(shape=c(1),dtype='float32', name='LogVol') #for exposure/volume adj

Network = Design %>% 
  layer_dense(units=q1, activation='tanh',name='Layer1') %>% 
  layer_dense(units=q2, activation='tanh',name='Layer2') %>% 
  layer_dense(units=1, activation='linear', name= 'Network', 
              weights=list(array(0, dim=c(q2,1)),array(log(lambda0), dim=c(1))))

Response = list(Network,LogVol) %>% 
  layer_add(name='Add') %>% 
  layer_dense(units=1,activation=k_exp,name='Response',trainable = FALSE, 
              weights = list(array(1,dim=c(1,1)),array(0,dim=c(1))))

# fit
modelfit.nn2 <- keras_model(inputs = c(Design, LogVol), outputs = c(Response))
modelfit.nn2 %>% compile(optimizer = optimizer_nadam(), loss = 'poisson')
summary(model) #shows number of parameters to be estimated/trained

history.nn2 <- modelfit.nn2 %>% fit(Xdata.train, Ydata.train, epochs=30,batch_size=nrow(Xdata.train),view_metrics = TRUE,
                         validation_data = list(Xdata.test, Ydata.test), verbose=0)
plot(history.nn2)

train$fit.nn2  <- modelfit.nn2 %>% predict(Xdata.train)
test$fit.nn2 <- modelfit.nn2 %>% predict(Xdata.test)

# in-out sample error
in_sample_error.nn2 <- 
  2*(sum(log((Ydata.train/train$fit.nn2)^Ydata.train))-sum(Ydata.train)+sum(train$fit.nn2))/nrow(Ydata.train)
out_sample_error.nn2 <- 
  2*(sum(log((Ydata.test/test$fit.nn2)^Ydata.test))-sum(Ydata.test)+sum(test$fit.nn2))/nrow(Ydata.test)
cat(" NN2 in-sample error =", in_sample_error.nn2*100, "\n", "NN2 out-sample error =", out_sample_error.nn2*100)


###################################################
### Using shallow NN with q = 50, use neural net boosted GLM
feature.train <- cbind(scale(train$ownAge,center=ownAge.min,scale=ownAge.max-ownAge.min),
                       scale(train$vehAge,center=vehAge.min,scale=vehAge.max-vehAge.min))
Xdata.train <- list(as.matrix(cbind(model.matrix(~gender+zone+mcCl+bonCl, data=train)[,-1],feature.train)), 
                    as.matrix(log(train$fit.glm2)))  


feature.test <- cbind(scale(test$ownAge,center=ownAge.min,scale=ownAge.max-ownAge.min),
                      scale(test$vehAge,center=vehAge.min,scale=vehAge.max-vehAge.min))
Xdata.test <- list(as.matrix(cbind(model.matrix(~gender+zone+mcCl+bonCl, data=test)[,-1],feature.test)), 
                   as.matrix(log(test$fit.glm2)))  

Ydata.train <- as.matrix(train$claimsNum)
Ydata.test <- as.matrix(test$claimsNum)

# build model
set_random_seed(123)
q0 <- 21 # no of covariates
q1 <- 50 # no of neurons
lambda0 <- sum(train$claimsNum)/sum(train$fit.glm2) #initial NN lambda estimate to be based on GLM2 prediction

Design <- layer_input(shape=c(q0),dtype='float32', name='Design') #size of input
LogVol <- layer_input(shape=c(1),dtype='float32', name='LogVol') #for exposure/volume adj

Network = Design %>% 
  layer_dense(units=q1, activation='tanh',name='Layer1') %>% 
  layer_dense(units=1, activation='linear', name= 'Network', 
              weights=list(array(0, dim=c(q1,1)),array(log(lambda0), dim=c(1))))

Response = list(Network,LogVol) %>% 
  layer_add(name='Add') %>% 
  layer_dense(units=1,activation=k_exp,name='Response',trainable = FALSE, 
              weights = list(array(1,dim=c(1,1)),array(0,dim=c(1))))

# fit
modelfit.nn3 <- keras_model(inputs = c(Design, LogVol), outputs = c(Response))
modelfit.nn3 %>% compile(optimizer = optimizer_nadam(), loss = 'poisson')
summary(modelfit.nn3) #shows number of parameters to be estimated/trained
history.nn3 <- modelfit.nn3 %>% 
  fit(Xdata.train, Ydata.train, epochs=60,batch_size=nrow(Xdata.train),view_metrics = TRUE,
                         validation_data = list(Xdata.test, Ydata.test), verbose=0)
plot(history.nn3)

train$fit.nn3  <- modelfit.nn3 %>% predict(Xdata.train)
test$fit.nn3 <- modelfit.nn3 %>% predict(Xdata.test)

# in-out sample error
in_sample_error.nn3 <- 
  2*(sum(log((Ydata.train/train$fit.nn3)^Ydata.train))-sum(Ydata.train)+sum(train$fit.nn3))/nrow(Ydata.train)
out_sample_error.nn3 <- 
  2*(sum(log((Ydata.test/test$fit.nn3)^Ydata.test))-sum(Ydata.test)+sum(test$fit.nn3))/nrow(Ydata.test)
cat(" NN3 in-sample error =", in_sample_error.nn3*100, "\n", "NN3 out-sample error =", out_sample_error.nn3*100)

# -> not better than GLM (for out-sample error) but slightly better than the other NNs -- NN1 and NN2
# -> when GLM and NN is combined, it results in better fit and predictive power




