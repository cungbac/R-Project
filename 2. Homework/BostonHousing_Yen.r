library(caret)
library(tidyverse)
library(mlbench) # To employ BostonHousing data
library(leaps) # To apply stepwise, backward and forward regression
library(summarytools)
data("BostonHousing") #Dữ liệu nhà ở cho 506 vùng điều tra dân số của Boston từ cuộc điều tra dân số năm 1970
head(BostonHousing)
str(BostonHousing)
help("BostonHousing") 
dfSummary(BostonHousing) %>% view()
#crim: toi pham bình quân đầu người tỷ lệ theo thị trấn,zn: tỷ lệ đất thổ cư được khoanh vùng cho các lô trên 25.000
#indus :tỷ lệ mẫu đất kinh doanh không bán lẻ trên mỗi thị trấn, chas:Biến giả Charles River (= 1 nếu đường giới hạn sông; 0 nếu không)
#nox: nồng độ oxit nitric (phần trên 10 triệu), rm: số phòng trung bình cho mỗi căn hộ,age: tỷ lệ các đơn vị chủ sở hữu được xây dựng trước năm 1940
#dis: khoảng cách trọng số đến năm trung tâm việc làm Boston, rad: chỉ số khả năng tiếp cận đường cao tốc xuyên tâm,thuế: thuế suất tài sản đầy đủ giá trị trên 10.000 USD
#ptratio: tỷ lệ học sinh-giáo viên theo thị trấn,b: 1000 (B - 0,63) ^ 2 trong đó B là tỷ lệ người da đen theo thị trấn
#lstat :tỷ lệ phần trăm thấp của tình trạng thấp hơn của dân số,medv: giá trị trung bình của các ngôi nhà do chủ sở hữu chiếm 1000 USD

#Train & Test
set.seed(1)
Train_index = createDataPartition(BostonHousing$medv,p = 0.7, list = F) 
Train = BostonHousing[Train_index,]
Test = BostonHousing[-Train_index,]


#Control
Control = trainControl(method = "repeatedcv",
                       number = 2, 
                       repeats = 5)


#BASE
###Train
model_base = train(medv~ crim +zn +indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
                   data = Train,
                   method = "lm",
                   trControl = Control)
model_base$results
model_base$resample
model_base$finalModel

###fitted
lm_fitbase = fitted(model_base)
Train$y_fitbase = lm_fitbase

### residual
resi = resid(model_base)
Train$residbase = resi

###plot
plot(model_base$finalModel)

###predict
pred_modelbase = predict(model_base, newdata = Test)

###Evaluate on test set
postResample(pred_modelbase,Test$medv)



#DUMMY

##Train
model_dummy = train(medv~ crim +zn +indus+nox+rm+age+dis+rad+tax+ptratio+b+lstat+ chas,
                    data = Train,
                    method = "lm",
                    trControl = Control)

model_dummy$results
model_dummy$resample
model_dummy$finalModel
summary(model_dummy)

##fitted
lm_fitdm = fitted(model_dummy)
Train$y_fitdm = lm_fitdm

##residual
res = resid(model_dummy)
Train$residdm = res

##plot
plot(model_dummy$finalModel)

##predict
pred_modeldm = predict(model_dummy, newdata = Test)

##Evaluate on test set
postResample(pred_modeldm,Test$medv)


#INTERACTION

##Train
model_it = train(medv~ rad + chas + rad: chas,
                 data = Train,
                 method = "lm",
                 trControl = Control)
model_it$results
model_it$resample
model_it$finalModel
summary(model_it)

##fitted
lm_fitit = fitted(model_it)
Train$y_fitit = lm_fitit

##residual
res1 = resid(model_it)
Train$residit = res1

##plot
plot(model_it$finalModel)
##predict
pred_modelit = predict(model_it, newdata = Test)
##Evaluate
postResample(pred_modelit,Test$medv)
