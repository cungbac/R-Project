library(tidyverse)
library(mgcv)
library(ggplot2)
library(summarytools)
library(dplyr)

#Preparing Data
Female = rep(0,614)
P_F1 = rep(0,42)
P_F2 = rep(1,572)
P_F = c(P_F1,P_F2)


Male = rep(1,399)
P_M1 = rep(0,79)
P_M2 = rep(1,320)
P_M = c(P_M1,P_M2)

Pay = c(P_F,P_M)
Gen = c(Female,Male)

Bank0 = data.frame(Gender = Gen, Payment = Pay)

#Plot
Bank= Bank0 %>%
  mutate(Gender = factor(Gender,levels = c(1,0),labels = c("NAM","NỮ")),
         Payment = factor(Payment, levels = c(0,1),labels = c("Không đúng hạn","Đúng hạn"))) 
Bank_1 = Bank %>%
   group_by(Gender,Payment) %>%
  summarise(count = n())%>%
  ungroup() 

str(Bank_1)

ggplot(Bank_1,aes(x=Gender,y=count,fill=Payment))+
  geom_bar(stat ="identity",position = "dodge") +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  geom_text(aes(label = count),vjust = -0.5,position = position_dodge(0.9))+
  theme_bw()+ #hoa nen xam thanh trang,co vien
  theme(legend.position = "top",legend.text =element_text(size=11),panel.grid = element_blank()) +
  xlab("")+
  ylab("")

#Analysis data

ctable(Bank$Gender,Bank$Payment)%>%view()
logitt = glm(Payment ~ Gender,
            family = binomial, data = Bank) 
summary(logitt)
exp(1.2126) #xác suất trả nợ đúng hạn của nhóm nữ gấp 3,36 lần nam

fitted_logitt = fitted(logitt,type = "response")
summary(fitted_logitt)
fitted_logitt = ifelse(fitted_logitt > 0.88,1,0) %>%
  factor(levels = c(0,1),labels = c("Không đúng hạn","Đúng hạn"))
Bank2 = Bank %>%
  mutate(fitted = fitted_logitt)
head(Bank2)

ctable(Bank2$fitted,Bank2$Payment) %>% view()
confusionMatrix(Bank2$fitted, Bank2$Payment)

#Dựa vào biểu đồ ta thấy số lượng khách hàng là nữ nhiều hơn khách hàng nam. 
#Xác suất trả nợ đúng hạn của nữ gấp 3.36 lần nam.
#Xac xuất dự đoán đúng 64,26%
#Dự đoán nhom khong dung han : 65,29%, dự đoán nhóm đúng hạn :64,13%. dự đoán nhóm không đung hạn chinh xac hơn nhóm đúng hạn










