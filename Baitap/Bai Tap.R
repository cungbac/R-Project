library(ggplot2)
library(tidyverse)
library(lattice)
library(caret)
library(GGally)
library(summarytools)
Ma1 <- rep(0,79)
Ma2 <- rep(1,320)
Ma <- c(Ma1, Ma2)

Fe1 <- rep(0,42)
Fe2 <- rep(1, 572)
Fe <- c(Fe1, Fe2)

Payment <- c(Ma, Fe)

Male <-  rep(1,399)
Female <- rep(0, 614)
Gender <- c(Male, Female)

bank <- data.frame(Gender = Gender, Payment = Payment)

bank <- bank %>%
  mutate(Payment = factor(Payment, levels = c(0,1),
                          labels = c('Khong Dung Han', 'Dung Han')),
         Gender = factor(Gender, levels = c(0, 1),
                         labels = c('Nu', 'Nam'))) 
bank1 <- bank %>%
  group_by(Gender, Payment) %>%
  summarise(P = n()) %>%
  ungroup()
# 2:
ggplot(bank1, aes(x = Gender, y = P, fill = Payment)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(aes(label = P), vjust = -0.4, position = position_dodge(0.9)) +
  theme_bw() +
  ylab('Payment') 

# 3:
ctable(bank$Gender,bank$Payment)%>%view()
logit_mod <- glm(Payment ~ Gender, data = bank,
                 family = binomial)
summary(logit_mod)
exp(-1.2126) # Kha nang tra no dung han cua Nam chi bang 29% so voi Nu

fitted_logit <- fitted(logit_mod, type = 'response')
summary(fitted_logit)

fitted_class = ifelse(fitted_logit > 0.8806,1,0) %>%
  factor(levels = c(0,1),labels = c("Khong Dung Han","Dung Han"))
bank2 = bank %>%
  mutate(fitted = fitted_logit)
head(bank2)

ctable(bank2$fitted,bank2$Payment) %>% view()


