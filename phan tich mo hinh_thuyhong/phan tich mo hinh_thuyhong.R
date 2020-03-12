#insstall package
install.packages("mlbench")
install.packages("leaps")
#library
library(caret)
library(tidyverse)
library(mlbench) # To employ BostonHousing data
library(leaps) # To apply stepwise, backward and forward regression
# Data
data(BostonHousing)
head(BostonHousing)
str(BostonHousing)
# Data split: train and test data
set.seed(1)
nrow(BostonHousing) 
train_index=createDataPartition(BostonHousing$medv, p=0.7, list=F)
train=BostonHousing[train_index,]
str(train)
test=BostonHousing[-train_index,]
# Train control: kiem soat tao ra 1 yeu cau, cho dl chay tren do
ctr <- trainControl(method="repeatedcv",
                    number=2,
                    repeats = 5)

ctr$method
ctr$number
# Train models: 
  model_base <- train(medv~crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat, 
                      data = train, 
                      method = 'lm', 
                      trControl = ctr) 
  
  model_dummy <- train(medv~chas+crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
                       data = train, 
                       method = 'lm', 
                       trControl = ctr) 
  
  model_interaction <-train(medv~lstat+chas +lstat:chas, 
                           data=train, 
                            method = 'lm', 
                            trControl = ctr) 
                     
        
  model_stepwise <-
  model_backward <-
  model_forward <-
# Explore the results: 6 final models
  model_base$results
  model_base$resample
  model_base$finalModel 
  model_dummy$finalModel 
  model_interaction$finalModel 
  
# Calculate fitted values of 6 final models and add them into dataframe
  base_fit<- fitted(model_base) 
  dummy_fit<- fitted(model_dummy) 
  interaction_fit <- fitted(model_interaction) 
  data_fit<- data.frame(base_fit,dummy_fit,interaction_fit)
# Calculate residuals of 6 final models and add them into dataframe
  base_resid <- residuals(model_base) 
  dummy_resid <- residuals(model_dummy) 
  interaction_resid <- residuals(model_interaction) 
  data_resid <- data.frame(base_resid,dummy_resid,interaction_fit)
# Residuals analysis of 6 final models 
 plot(model_base$finalModel) 
 plot(model_dummy$finalModel) 
 plot(model_interaction$finalModel) 

 # Compare peformance of 6 final models 

# Predicted value of 6 final models 
 base_pred <- predict(model_base,newdata = test) 
 dummy_pred <- predict(model_dummy,newdata = test) 
 interaction_pred <- predict(model_interaction,newdata = test) 

# Evaluate 6 final models
















