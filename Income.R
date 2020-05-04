# Library
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(summarytools)

# Load data
income <- read_excel('/Users/cungbac/Documents/Learning/4. R programming/15. Examination/Income/Income.xlsx')
head(income)
summary(income)
dfSummary(income) %>%
  view()
# Plots

## Create no column
income <- income %>%
  arrange(Income) %>%
  mutate(no = seq_along(income$Income))

## Generate some statistics
Mean = mean(income$Income)
Median = median(income$Income)
sd = sd(income$Income)

qplot(seq_along(income$Income),income$Income)

p1 <- income %>%
  ggplot(aes(x= no,y=Income)) +
  geom_point(color = 'lightblue') + 
  geom_hline(yintercept = mean(income$Income),color = 'blue',size = 2) +
  geom_text(aes(0,mean(Income),
                label = paste('Mean = ',mean(Income)),
                vjust = -1, hjust = - 1)) + 
  geom_hline(yintercept = Median) + 
  geom_text(aes(0,Median,
                label = paste('Median =',Median),
                vjust = -1,
                hjust = -1)) + 
  theme(axis.title.x = element_blank())
  
p2 <- income %>%
  ggplot(aes(x= no,y=Income)) +
  geom_point(color = 'lightblue') + 
  geom_hline(yintercept = mean(income$Income),
             color = 'blue',
             size = 2) +
  geom_text(aes(0,mean(Income),
                label = paste('Mean = ',mean(Income)),
                vjust = -1, 
                hjust = -1)) + 
  geom_hline(yintercept = Mean + sd,
             linetype = 'dashed',
             size = 2) + 
  geom_text(aes(0,Mean + sd,
                label = paste('Mean + 1sd'),
                vjust = -1,
                hjust = -1)) + 
  geom_hline(yintercept = Mean - sd,
             linetype = 'dashed',
             size = 2) + 
  geom_text(aes(0,Mean - sd,
                label = paste('Mean - 1sd'),
                vjust = -1,
                hjust = -1)) + 
  theme(axis.title.x = element_blank())

p3 <- income %>%
  ggplot(aes(y = Income)) + 
  geom_boxplot() + 
  #stat_summary(fun.y = 'mean', geom = 'point',color = 'red')
  geom_hline(yintercept = Mean,
             linetype = 'dashed',
             color = 'blue')
