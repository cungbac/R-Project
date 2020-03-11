# Library
library(caret)
library(tidyverse)
library(mlbench) # To employ BostonHousing data
library(leaps) # To apply stepwise, backward and forward regression
# Data
data(BostonHousing)
head(BostonHousing)
str(BostonHousing)
?BostonHousing
-------------------------------------
# Data split: train and test data
set.seed(1)
nrow(BostonHousing) #truy cap s??? dòng
1:nrow(BostonHousing)
train_index <- sample(1:nrow(BostonHousing), size = 250)   #l???y m???u random t??? 1-98 ch???n ra 68 data
train <- BostonHousing[train_index, ]
test <- BostonHousing[-train_index, ]

# Train control:
control <- trainControl(method= 'repeatedcv', 
                        number = 10,
                        repeats = 5) 
control$method
control$number
control$repeats
  # Train models: 
## model_fw
null = lm(medv ~ 1, data=BostonHousing)
fw = step(null, medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat, 
          direction = "forward", test = "F")
, k=log(nrow(ind)))

model_fw= train(medv ~ lstat + rm + ptratio + dis + nox + chas + b + zn + crim + 
                  rad + tax, 
                    data = train,
                    method = 'lm',
                    trControl = control)
model_fw$results
model_fw$finalModel
model_fw$resample
summary(model_fw)
# Fitted model
Fitted_mod_fw = fitted(model_fw)

# residuals
res1 = resid(model_fw)

#plot
plot(model_fw$finalModel)
#predict
pred_modfw = predict(model_fw, newdata = test)
pred_modfw
# evaluate
postResample(pred_modfw, test$medv)

## model b???ckward
full = lm(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
            b + lstat, data = BostonHousing)
bw = step (full, direction = 'backward', test = 'F')

model_bw= train(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                  b + lstat, 
                data = train,
                method = 'lm',
                trControl = control)
model_bw$results
model_bw$finalModel
model_bw$resample
summary(model_bw)
# Fitted model
Fitted_mod_bw = fitted(model_bw)

# residuals
res2 = resid(model_bw)

#plot
plot(model_bw$finalModel)
#predict
pred_modbw = predict(model_bw, newdata = test)
pred_modbw
# evaluate
postResample(pred_modbw, test$medv)


## Stepwise
null1 = lm(medv ~ 1, data = BostonHousing)
st = step(null1, medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat,
             direction = "both", test = "F", trace = 0)
step$anova
summary(st)

model_st= train(medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + 
                  b + lstat, 
                data = train,
                method = 'lm',
                trControl = control)
model_st$results
model_st$finalModel
model_st$resample
summary(model_st)
# Fitted model
Fitted_mod_st = fitted(model_st)

# residuals
res3 = resid(model_st)

#plot
plot(model_st$finalModel)
#predict
pred_modst = predict(model_st, newdata = test)
pred_modst
# evaluate
postResample(pred_modst, test$medv)


#model dummy
model_dummy = train(medv ~ chas, 
                    data = train,
                    method = 'lm',
                    trControl = control)
model_dummy$results
model_dummy$finalModel
model_dummy$trainingData 
model_dummy$resample
summary(model_dummy)
# Fitted model
Fitted_mod_dum = fitted(model_dummy)

# residuals
res = resid(model_dummy)

#plot
plot(model_dummy$finalModel)
#predict
pred_moddm = predict(model_dummy, newdata = test)
pred_moddm
# evaluate
postResample(pred_moddm, test$medv)

## model_base 
model_base = train(medv~ zn, data = train,
                   method = 'lm',
                   trControl = control)
model_base$results
model_base$finalModel
model_base$trainingData 
model_base$resample
summary(model_base)
# Fitted model
Fitted_mod_base = fitted(model_base)
# residuals
res = resid(model_base)
#plot
plot(model_base$finalModel)
#predict
pred_modbs = predict(model_base, newdata = test)
pred_modbs
# evaluate
postResample(pred_modbs, test$medv)
  
#model_interaction mod
model_it = train(medv ~ zn + chas: chas,
                 data= train,
                 method= 'lm',
                 trControl= control)
model_it$results
model_it$finalModel
model_it$trainingData 
model_it$resample
summary(model_it)
# Fitted model
Fitted_mod_it = fitted(model_it)
# residuals
res = resid(model_it)
#plot
plot(model_it$finalModel)
#predict
pred_modit = predict(model_it, newdata = test)
pred_modit
# evaluate
postResample(pred_modit, test$medv)

  
  