library(caret)
library(tidyverse)
library(mlbench) # To employ BostonHousing data
library(leaps) # To apply stepwise, backward and forward regression
library(summarytools)

#Data:
data("BostonHousing")
head(BostonHousing)
str(BostonHousing)

dfSummary(BostonHousing) %>% view()



# Split Data:
train_row <- createDataPartition(BostonHousing$medv, p = 0.8, list = F)
train <- BostonHousing[train_row, ]
test <- BostonHousing[-train_row, ]

# Train Control:
trc <- trainControl(method = 'repeatedcv', 
                    number = 2,
                    repeats = 10)
trc

## Train Model:
# Model Base: 
model_base <- train(medv ~ crim + zn + indus + nox + rm + age +dis +
                      rad + tax + ptratio + b + lstat,
                    data = train, 
                    method = 'lm',
                    trControl = trc)
# Model Dummy:
model_dummy <- train(medv~chas+crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
                     data = train,
                     method = 'lm',
                     trControl = trc) # Dummy is '1' or '0'
#Model Interaction
model_interaction = train(medv~chas + lstat + chas:lstat,
                          train,
                          method = 'lm',
                          trControl = trc)

## Explore Results:
model_base$finalModel 
broom::glance(model_base$finalModel)

model_dummy$finalModel
broom::glance(model_dummy$finalModel)

model_interaction$finalModel
broom::glance(model_interaction$finalModel)

# Calculate fitted values of 6 final models and add them into dataframe
base.fit <- fitted(model_base)
dummy.fit <- fitted(model_dummy)
interaction.fit <- fitted(model_interaction)

# Calculate residuals of 6 final models and add them into dataframe
base.resid <- residuals(model_base)
dummy.resid <- residuals(model_dummy)
interaction.resid <- residuals(model_interaction)

# Residuals analysis of 6 final models
plot(model_base$finalModel)
plot(model_dummy$finalModel)
plot(model_interaction$finalModel)

# Predicted value of 6 final models:
predicted_base <- predict(model_base, newdata = test)
predicted_dummy <- predict(model_dummy, newdata = test)
predicted_interaction <- predict(model_interaction, newdata = test)

# Evaluate 6 final models:
eva_base <- postResample(predicted_base, test$medv)
eva_dummy <- postResample(predicted_dummy, test$medv)
eva_interaction <- postResample(predicted_interaction, test$medv)


