# Libaray
library(mgcv)
library(tidyverse)
library(ggplot2)
library(summarytools)
library(dplyr)

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
str(be_data)

be_data= be_data %>%
  group_by(Gender, Payment)%>%
  mutate(count= n()) %>%
  ungroup()


data= be_data %>%
  mutate(Gender= factor(Gender, levels = c("M", "F"), labels = c("Male", "Female"))) %>%
  mutate(Payment= factor(Payment, level= c(0,1), labels= c("N", "Y")))
  
str(data)
ctable(data)%>% view


#2 Plot
ggplot(data, aes(x= Gender, y= count))+
  geom_bar(stat = "identity", width = 0.5, aes(fill= Payment), position = "dodge")+
  facet_grid(~Gender)+
  theme(legend.position = "top")+
  scale_fill_discrete(name= "Payment", labels= c("Not in due", "In due"))+
  geom_text(aes(label= count), position = position_dodge(0.8), vjust= -0.3, size= 3)

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

#Comments: 
## Linear Regression:
### Gender only show 31.84% of the result of Payment.
### This number is extremely small. Remove this method.

## Logistic Regression:
### Here values of Null Deviance can be read as 741.16 on 1012 degrees of freedom. And Residual deviance as 703.47 on 1011 degrees of freedom.
### Deviance is a measure of goodness of fit of a model. Higher numbers always indicates bad fit.
### The null deviance shows how well the response variable is predicted by a model that indicates only the intercept (grand mean)
### The addition of 1 (1012-1011=1) independent variables decreased the deviance to 703.47 from 741.16, a significant reduction in deviance.
### It's meaning that te residual deviance has reduced by 37.69 with a loss of 1 degrees of freedom.
### In conclusion, this null deviance is big , it means the null model explains the data worse.

