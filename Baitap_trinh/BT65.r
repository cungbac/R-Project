library(ggplot2)
library(summarytools)
library(mgcv)
library(dplyr)
library(tidyverse)
library(GGally)
library(foreign)
library(caret)

#1
M = rep(1, 399)
P1 = rep(0,79)
P2 = rep(1, 320)
P12 = c(P1, P2)

FM = rep(0,614)
p1 = rep(0,42)
p2 = rep(1,572)
p12 = c(p1, p2)

payment = c(p12, P12)
gender = c(FM, M)

df = data.frame(Gender = gender, Payment = payment)

df1 = df %>%
  mutate(Gender = factor(Gender, levels = c(0,1), labels = c('Female', 'Male')),
         Payment = factor(Payment, levels = c(0,1), labels = c('Khong dung han', 'Dung han')))
df2 <- df1 %>%
  group_by(Gender, Payment) %>%
  summarise(p = n()) %>%
  ungroup()
  
#2
ggplot(df2, aes(x = Gender, y = p, fill = Payment))+
  geom_bar(stat = 'identity', position = position_dodge())+
  geom_text(aes(label = p), position = position_dodge(1), vjust = -0.3)+
  theme(legend.position = "top",legend.text =element_text(size=11),panel.grid = element_blank())


#3
#Analysis data
a = ctable(df1$Gender,df1$Payment)%>%view()
#logistics model
logit <- glm(Payment ~ Gender, 
             family = binomial, data = df1)

summary(logit)
exp(-1.2126)
fitted_logit <- fitted(logit, type = 'response')
summary(fitted_logit)
fitted_logit <- ifelse(fitted_logit > 0.8806, 1, 0) %>%
  factor(levels = c(0,1), labels = c('Khong dung han', 'Dung han'))
summary(fitted_logit)

df3 <- df1 %>%
  mutate(fitted = fitted_logit)

b = ctable(df3$fitted, df3$Payment) %>% view() 
# Accuracy
mean(class$fitted == class$Payment)
# Confusion matrix
confusionMatrix(class$fitted, class$Payment)


#NX
#Xác su???t tr??? n??? dúng h???n N??? g???p 3 l???n Nam
#Accuracy 64,26%
#Khong dung han 65,29%
#dúng h???n 64,125%
# -> d??? doán nhóm dúng h???n chính xác hon






