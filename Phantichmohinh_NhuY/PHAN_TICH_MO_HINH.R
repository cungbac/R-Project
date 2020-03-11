# Library
library(caret)
library(tidyverse)
library(mlbench) # To employ BostonHousing data
library(leaps) # To apply stepwise, backward and forward regression
# Data
data("BostonHousing")
head(BostonHousing)
str(BostonHousing)
summary(BostonHousing)
?BostonHousing
# Data split: train and test data
train_index<-createDataPartition(BostonHousing$medv,p=0.7,list = F)
train<-BostonHousing[train_index,]
test<- BostonHousing[-train_index,]
# Train control
ctr <- trainControl(method = "repeatedcv",number = 2,repeats = 5)
# Train models
##Model base
model_base = train(medv~crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
           data = train,
           method = 'lm',
           trControl = ctr)
##Model dummy
model_dummy = train(medv~chas+crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
                    data = train,
                    method = 'lm',
                    trControl = ctr) #dummy la 1 va 0
##Model Interaction
model_interaction = train(medv~chas + lstat + chas:lstat,
                          train,
                          method = 'lm',
                          trControl = ctr)
##Model Stepwise
model_stepwise = train(medv~., 
                       data = train,
                       method = "leapSeq",
                       tuneGrid = data.frame(nvmax = 1:5),
                       trControl = ctr)
##Model Backward
model_backward = train(medv~., 
                       data = train,
                       method = "leapBackward",
                       tuneGrid = data.frame(nvmax = 1:5),
                       trControl = ctr)
##Model Forward
model_forward = train(medv~., 
                       data = train,
                       method = "leapForward",
                       tuneGrid = data.frame(nvmax = 1:5),
                       trControl = ctr)
# Explore the results: 6 final models
model_base$finalModel
model_dummy
model_interaction
summary(model_stepwise$finalModel)
summary(model_backward$finalModel)
summary(model_forward$finalModel)
# Calculate fitted values of 6 final models and add them into dataframe
base.fit <- fitted(model_base)
dummy.fit <- fitted(model_dummy)
interaction.fit <- fitted(model_interaction)
step.fit <- fitted(model_stepwise)
back.fit <- fitted(model_backward)
for.fit <- fitted(model_forward)
data.fit <- data.frame(base.fit,dummy.fit,interaction.fit,step.fit,back.fit,for.fit)
