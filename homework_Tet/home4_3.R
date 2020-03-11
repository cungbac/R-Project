#Library
library(caret)
library(tidyverse)
library(mlbench) #to employ BostonHousing data
library(leaps) #to apply stepwise, backward and forward regression
library(summarytools)
#Data
data("BostonHousing")
head(BostonHousing)
str(BostonHousing)
?BostonHousing
summary(BostonHousing)

################
#Data split: train and test data
dfSummary(BostonHousing)%>% view()
train_index= createDataPartition(BostonHousing$medv, p= 0.7, list= F) #lay sample theo bien phu thuoc y
train_data= BostonHousing[train_index,]
str(train_data)
test_data= BostonHousing[-train_index,]
str(test_data)

#Train control: Important
ctr= trainControl(method = "repeatedcv",
                  number = 10,
                  repeats = 5)

ctr$method
ctr$number
ctr$repeats
ctr$sampling

#Train model
model_base= train(medv~crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat, data= train_data,
                  method= "lm",
                 trControl= ctr)

summary(model_base)

model_dummy=  train(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat, data= train_data,
                    method= "lm",
                    trControl= ctr)

summary(model_dummy)


model_interaction=  train(medv~crim+chas, data= train_data,
                          method= "lm",
                          trControl= ctr)
summary(model_interaction)


