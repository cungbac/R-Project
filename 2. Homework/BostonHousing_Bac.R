
# Library
library(caret)
library(tidyverse)
library(mlbench) # To employ BostonHousing data
library(leaps) # To apply stepwise, backward and forward regression
library(summarytools)
library(dplyr)
library(corrplot)

# Data
data(BostonHousing)
head(BostonHousing)
str(BostonHousing)
dfSummary(BostonHousing) %>%
  view()

#Exploratory data analysis
summary(BostonHousing)

BostonHousing %>%
  gather(key,val,-medv) %>%
  ggplot(aes(x = val,y = medv,col = key)) +
  geom_point() +
  facet_wrap(~ key)

BostonHousing %>%
  gather(key,val, -chas) %>%
  ggplot(aes(x = val,fill = chas)) +
  geom_bar(stat = 'count',position = 'stack') +
  facet_wrap(~ key,scales = 'free')

BostonHousing %>%
  group_by(chas) %>%
  summarise(crim = mean(crim),
            zn = mean(zn),
            indus = mean(indus),
            nox = mean(nox),
            rm = mean(rm),
            age = mean(age),
            dis = mean(dis),
            rad = mean(rad),
            tax = mean(tax),
            ptratio = mean(ptratio),
            b = mean(b),
            lstat = mean(lstat),
            medv = mean(medv))


#  FULFIL my requirements
  
## Checking correlation between variables

BostonHousing %>%
  select(-chas) %>%
  cor()

cor(BostonHousing[,-4])

sum(is.na(BostonHousing))
sum(duplicated(BostonHousing))
glimpse(BostonHousing)
  
# Data split: train and test data
set.seed(1)
split_data <- createDataPartition(BostonHousing$zn, p = 0.7, list = F)
train_data <- BostonHousing[split_data,]
test_data <- BostonHousing[-split_data,]

table(BostonHousing$rad)
table(BostonHousing$chas)

summary(lm(medv ~ crim + chas,BostonHousing))

summary(lm(medv ~ crim + chas + crim:chas , BostonHousing))

str(BostonHousing)

chas1 <- BostonHousing %>%
  filter(chas == 1)
summary(lm(medv ~ crim,chas1))

chas0 <- BostonHousing %>%
  filter(chas == 0)
summary(lm(medv ~ crim,chas0))

# Train control:
ctr <- trainControl(method = 'repeatedcv',
                    number = 3,
                    repeats = 5)
  # Train models: 
  model_base <- train(medv ~ crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
                   data = train_data,
                   method = 'lm',
                   trControl = ctr)
  model_dummy <- train(medv ~ crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat+chas,
                       data = train_data,
                       method = 'lm',
                       trControl = ctr)
  model_interaction <- train(medv ~ crim+chas,
                             data = train_data,
                             method = 'lm',
                             trControl = ctr)
  model_stepwise <- 
  model_backward <-
  model_forward <- 
  
# Explore the results: 6 final models
  model_base$finalModel
  model_dummy$finalModel
  model_interaction$finalModel

  model_base$resample
  
  BostonHousing %>%
    group_by(chas) %>%
    summarise(mean = mean(crim),
              mean_medv = mean(medv))
  
  BostonHousing %>%
    group_by(chas) %>%
    summarise(mean = mean(age),
              mean_medv = mean(medv))
  
  # Calculate fitted values of 6 final models and add them into dataframe
  fitted(model_base)
  fitted(model_dummy)
  fitted(model_interaction)
# Calculate residuals of 6 final models and add them into dataframe
  res_base <- as.data.frame(residuals(model_base$finalModel))
  class(res_base)
  
  res_dummy <- as.data.frame(residuals(model_dummy$finalModel))
  
  res_interation <- as.data.frame(residuals(model_interaction$finalModel))
  # Residuals analysis of 6 final models
  plot(model_base$finalModel)
  plot(model_dummy$finalModel)
  plot(model_interaction$finalModel)
  
  # Compare peformance of 6 final models: 
  ........
# Predicted value of 6 final models:
  base_prd <- as.data.frame(predict(model_base$finalModel,newdata = test_data))
  class(base_prd)
  evaluate_base <- cbind(base_prd,test_data$crim)
  str(evaluate_base)
  colnames(evaluate_base) <- c('predict','actual')
  evaluate_base <- evaluate_base %>%
    mutate(res = actual - predict)
  
  sd(evaluate_base$res)
# Evaluate 6 final models:
  
  
  
  