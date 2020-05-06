
# Library
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(summarytools)
library(plotly)
library(car)
library(GGally)


#EX_1: Data

## data preparation

Female <- rep(0,614)
Fe1 <- rep(0,42)
Fe2 <- rep(1,572)
Fe <- c(Fe1, Fe2)

Male <- rep(1,399)
Ma1 <- rep(0,79)
Ma2 <- rep(1,320)
Ma <- c(Ma1, Ma2)

Payment <- c(Fe, Ma)
Gender <- c(Male, Female)

df <- data.frame(Gender, Payment)

df <- df %>%
  mutate(Payment =factor(Payment, levels = c(0,1),
                         labels = c('No','Yes')),
         Gender = factor(Gender, levels = c(0,1),
                         labels = c('F', 'M')))

## Exploring 
ggpairs(df, aes(col=factor(Payment)))


df<- df %>%
  group_by(Gender, Payment)%>%
  summarise(P = n())%>%
  ungroup()

## Ex_2: Plot

ggplot(df, aes(x=Gender, y=P, fill= Payment))+
  geom_bar(stat = 'identity',
           position = position_dodge())+
  geom_text(aes(label =P), position = position_dodge(0.9), vjust = 0)+ 
  labs(title = 'Payment of Bank',
       subtitle = 'Lê Trường Hận',
       x = 'Gender',
       y= 'Payment')+
  theme(panel.background = element_rect(fill='white',color = 'blue'),
                                        legend.position = 'bottom')
#EX_3: 

##Logistic model

logit_mod <- glm(Payment ~ Gender, data=df,
                family = 'binomial')
summary(logit_mod)

exp(0.2272) 

fitted_con <- fitted(logit_mod, type = 'reponse')
summary(fit_con)


fitted_class <- ifelse(fitted_con > 0.8806, 1, 0)%>%
  factor(levels = c(0,1), labels = c('No','Yes'))

df$fitted_class <- fitted_class
df
ctable(df$fitted_class, df$Payment)%>% view()

# Payment
## False Negative (FN): "important", dự đoán sai trường hợp không trả nợ là (10.5%)
## False Positive (FP):  dự đoán sai trường hợp có khả năng trả nợ lên đến 87.1% làm ảnh hưởng đến đến quyết định của ngân hàng.
## Để giảm tỷ lệ FN ta có thể tăng từ 0.8806 lên 0.9

fit<- ifelse(fitted_con > 0.9, 1, 0)%>%
  factor(levels = c(0,1), labels = c('No','Yes'))

df$fit <- fit
ctable(df$fit, df$Payment)%>% view()










