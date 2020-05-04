# Libaray
library(mgcv)
library(tidyverse)
library(ggplot2)
library(summarytools)

# 1.Prepare data
data1= data.frame(Gender= c("M"), Payment= c(0))
dat1= data1[rep(seq(nrow(data1)),79),]

data2= data.frame(Gender= c("M"), Payment= c(1))
dat2= data2[rep(seq(nrow(data2)),320),]

data3= data.frame(Gender= c("F"), Payment= c(0))
dat3= data3[rep(seq(nrow(data3)),42),]

data4= data.frame(Gender= c("F"), Payment= c(1))
dat4= data4[rep(seq(nrow(data4)),572),]

data12= rbind(dat1, dat2)
data34= rbind(dat3, dat4)

be_data= rbind(data12, data34)

data= be_data %>%
  mutate(Gender= factor(Gender, levels = c("M", "F"))) %>%
  mutate(Payment= factor(Payment, level= c(0,1), labels= c("N", "Y")))
str(data)

ctable(data)%>% view

#2 Plot
ggplot(data, aes(x= Gender, y= Payment))+
  geom_bar(stat = "identity", width = 0.5, aes(fill= factor(Payment)), position = "dodge")+
  facet_grid(~Gender)+
  theme(legend.position = "top")+
  scale_fill_discrete(name= "Payment", labels= c("Not in due", "In due"))

#3
## Linear model
lm_mod= lm(Payment~Gender,data=be_data)
summary(lm_mod)
plot(lm_mod, se= F)

## Logistic model
logit= glm(Payment~Gender, family= "binomial", data= be_data)
summary(logit)
fitted_value= fitted(logit, type= "response") 
summary(fitted_value)

fitted_value= ifelse(fitted_value >0.8806, 1,0)%>%
  factor(levels = c(0,1), labels = c("N", "Y"))
 
data_logit= data%>%
  mutate(fitted_value= fitted_value)

ctable(data_logit$fitted_value, data_logit$Payment) %>% view()

#Nhan xet: 
## Theo phuong pháp linear model, gender ch??? th??? hi???n chính xác 31.84% k???t qu??? c???a Payment. Có th??? lo???i phuong pháp này.
## Theo phuong pháp logistic, M???c d??? significant c???a data r???t nh???, không th??? gi???i thích rõ data.
## Nhìn chung, gender ch??? th??? hi???n du???c 1 ph???n nh??? c???a k???t qu??? c???a Payment. 
