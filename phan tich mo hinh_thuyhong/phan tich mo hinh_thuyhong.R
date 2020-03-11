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
str(BostonHousing$rad)
table(BostonHousing$rad)
# Data split: train and test data
set.seed(1)
nrow(BostonHousing) #bn dong
train_index=createDataPartition(BostonHousing$medv, p=0.7, list=F)
train=BostonHousing[train_index,]
str(train)
test=BostonHousing[-train_index,]
# Train control: kiem soat tao ra 1 yeu cau, cho dl chay tren do
ctr <- trainControl(method="repeatedcv",
                    number=10,
                    repeats = 5)

ctr$method
ctr$number
# Train models: 
  model_base <- train(medv~rad,
                      data=train,
                      method="lm",
                      trControl=ctr)
  model_dummy <-lm(medv~chas, BostonHousing) 
  model_interaction <-lm(medv~rad +chas+crim:chas, BostonHousing)
        river=BostonHousing%>%
              filter(chas==1)
        lm(medv~ rad, river)
         other=BostonHousing%>%
             filter(chas==0)
        lm(medv~ rad, other)
  model_stepwise <-
  model_backward <-
  model_forward <-
# Explore the results: 6 final models
  lm$resample #lay mau co thay the , 2 mau chay 5 lan thanh 10
  lm$finalModel
  summary(lm)
# Calculate fitted values of 6 final models and add them into dataframe

# Calculate residuals of 6 final models and add them into dataframe

# Residuals analysis of 6 final models
plot(model_)
# Compare peformance of 6 final models: 

# Predicted value of 6 final models:

# Evaluate 6 final models:



















