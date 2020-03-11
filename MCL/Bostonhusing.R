library(caret)
library(tidyverse)
library(summarytools)
library(plotly)
library(broom)
library(mlbench) # data(BostonHousing)
library(leaps) 
library(plotly)
library(corrplot)
# data
data("BostonHousing")
head(BostonHousing)
str(BostonHousing)
dfSummary(BostonHousing) %>% view()
?BostonHousing

# Check for missing values 
sum(is.na(BostonHousing))

 ## Checking correlation between variables
Boston <- BostonHousing[,-4] # chas : factor
corrplot(cor(Boston), method = 'number',
         type = 'upper',
         diag = F)

# Data split: train and test data
set.seed(1)
train_row <- createDataPartition(BostonHousing$medv,
                                 p = 0.7, list = F)

train <- BostonHousing[ train_row, ]
test <- BostonHousing[- train_row,]

# Train control:
ctr <- trainControl(method = 'repeatedcv', 
                    number = 10,
                    repeats = 5)


 # Train models: 
  model_base <- train(medv ~ crim +zn + indus+ age + nox + rm +dis + ptratio + tax+ ptratio+b +lstat,
                   data = train,
                   method = 'lm',
                   trControl = ctr )
 
 
  model_dummy <- train(medv ~ crim + chas + nox + rm + dis + ptratio + rad + b + lstat,
                       data = train,
                       method = 'lm',
                       trControl = ctr )
  model_interaction <- train(medv ~ crim +zn + indus+ age + nox + rm +dis + ptratio + tax+ ptratio+b +lstat+ chas,
                             data = train,
                             method = 'lm',
                             trControl = ctr )

# Explore the results: 6 final models

model_base$finalModel 
broom::glance(model_base$finalModel)

model_dummy$finalModel
broom::glance(model_dummy$finalModel)

model_interaction$finalModel
broom::glance(model_interaction$finalModel)

# Calculate fitted values of 6 final models and add them into dataframe
fitted(model_base)
fitted(model_dummy)
fitted(model_interaction)

# Calculate residuals of 6 final models and add them into dataframe
r_base <- as.data.frame(residuals(model_base$finalModel))
r_dummy <- as.data.frame(residuals(model_dummy$finalModel))
r_in <- as.data.frame(residuals(model_interaction$finalModel))

# Residuals analysis of 6 final models
plot(model_base$finalModel)
plot(model_dummy$finalModel)
plot(model_interaction$finalModel)

# Compare peformance of 6 final models: 
........
# Predicted value of 6 final models:
base_pred <- predict(model_base, newdata = test)
dummy_pred <- predict(model_dummy, newdata = test)
int_pred <- predict(model_interaction, newdata = test)

# Evaluate 6 final models:
base_eva <- postResample(base_pred, test$medv)
dummy_eva <- postResample(dummy_pred, test$medv)
int_eva <- postResample(int_pred, test$medv)






