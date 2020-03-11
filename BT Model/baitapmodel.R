library(caret)
library(tidyverse)
library(mlbench) # To employ BostonHousing data
library(leaps) # To apply stepwise, backward and forward regression
data(BostonHousing)
head(BostonHousing)
str(BostonHousing)
summary(BostonHousing)
###~~~~~~~~~Model base_1~~~~~~~~~###
set.seed(1)
train_row_1=createDataPartition(BostonHousing$medv,
                                p=0.7, 
                              list=F)
test_1=BostonHousing[-train_row_1 , ]
train_1=BostonHousing[train_row_1 , ]
## Train control_1
trcl_1=trainControl(method = "repeatedcv",
                  number = 2,
                  repeats = 5)
## Train the model_1 
str(train_1)
lm_1=train(medv~rm,data=train_1,
         method="lm",
         trControl=trcl_1)
## Eplore the result_1##
lm_1$resample
lm_1$finalModel
summary(lm_1)
lm_1$metric
plot(lm_1$finalModel)  
##fitted value
lm_fit_1=fitted(lm_1)
##Predicting value
lm_pred_1=predict(lm_1,newdata = test_1)
## Evaluating
postResample(lm_pred_1,test_1$medv) 
## R2 tren tap test
#RMSE tap test cao hon tap train la mo hinh do

##~~~~~~~~~Model Dummy_2~~~~~~~~~##
train_row_2=createDataPartition(BostonHousing$medv,
                                p=0.7, 
                                list=F)
test_2=BostonHousing[-train_row_2 , ]
train_2=BostonHousing[train_row_2 , ]
## Train control_2
trcl_2=trainControl(method = "repeatedcv",
                    number = 2,
                    repeats = 5)
## Train the model_2 
str(train_2)
lm_2=train(medv~ crim*chas+
             zn*chas+
             indus*chas+
             nox*chas+
             rm*chas+
             age*chas+
             dis*chas+
             rad*chas+
             tax*chas+
             ptratio*chas
           ,data=train_2,
           method="lm",
           trControl=trcl_1)
## Eplore the result_2##
lm_2$resample
lm_2$finalModel
summary(lm_2)
lm_2$metric
plot(lm_2$finalModel)  
##fitted value
lm_fit_2=fitted(lm_2)
##Predicting value
lm_pred_2=predict(lm_2,newdata = test_2)
## Evaluating
postResample(lm_pred_2,test_2$medv) 
###~~~~~Model Interactio_3~~~~~~`###
#crim
#zn
#rm, dis, rad, tax, 
train_row_3=createDataPartition(BostonHousing$medv,
                                p=0.7, 
                                list=F)
test_3=BostonHousing[-train_row_3 , ]
train_3=BostonHousing[train_row_3 , ]
## Train control_1
trcl_3=trainControl(method = "repeatedcv",
                    number = 2,
                    repeats = 5)
## Bien tuong tac: tax:rm
lm_3_tax_rm=train(medv~tax+rm+tax:rm,data=train_3,
           method="lm",
           trControl=trcl_3)
## Bien tuong tac: dis:rad
lm_3_dis_rad=lm_1=train(medv~dis+rad+dis:rad,data=train_3,
                        method="lm",
                        trControl=trcl_3)
## Bien tuong tac: crim:zn
lm_3_crim_zn=train(medv~crim+zn+crim:zn,data=train_3,
           method="lm",
           trControl=trcl_3)

