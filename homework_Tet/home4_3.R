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


model_dummy=  train(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat, data= train_data,
                    method= "lm",
                    trControl= ctr)


model_interaction=  train(medv~rad+chas, data= train_data,
                          method= "lm",
                          trControl= ctr)

# Explore the results: 6 final models
summary(model_base)
summary(model_dummy)
summary(model_interaction)

# Calculate fitted values of 6 final models and add them into dataframe
base_fit= fitted(model_base)
dummy_fit= fitted(model_dummy)
inter_fit= fitted(model_interaction)

fitted_df= data.frame(base_fit, 
               dummy_fit,
               inter_fit)
# Calculate residuals of 6 final models and add them into dataframe
base_re= resid(model_base)
dummy_re=resid(model_dummy)
inter_re= resid(model_interaction)

re_df= data.frame(base_re,
                  dummy_re,
                  inter_re)
# Residuals analysis of 6 final models
plot(model_base$finalModel)
plot(model_dummy$finalModel)
plot(model_interaction$finalModel)

# Compare peformance of 6 final models: 
## Bieu do cua base va dummy gan nhu the hien tuong duong nhau. Con bieu do cua interaction cos su khac biet tuong doi lon.

# Predicted value of 6 final models:
predict_base= predict(model_base, newdata = test_data)
predict_dummy= predict(model_dummy, newdata = test_data)
predict_inter= predict(model_interaction, newdata = test_data)
# Evaluate 6 final models:
postResample(predict_base, test_data$medv)
postResample(predict_dummy, test_data$medv)
postResample(predict_inter, test_data$medv)
