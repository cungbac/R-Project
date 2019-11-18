# libray 
library(readxl)
library(dplyr)
library(ggplot2)
library(summarytools)
library(labelled)
library(pastecs)

# Part 1

# 1
H = read_xlsx('Data 6_Han.xlsx')

# 2
summary(H)
which(is.na(H$X3 & H$Y)) # NA: X3-208, Y-310, Dum-245
which(is.na(H$Dum))

H = H %>%
  filter(!is.na(H$X3 & H$Y) & !is.na(H$Dum))

# 3
dfSummary(H) %>%view()
table(H$Dum)
which(H$Dum == 212) # Missing: row 130

level = table(H$Dum)
mode_leves = names(level)[which(level == max(level))]
H = H %>%
  mutate(Dum = replace(Dum, Dum==212, mode_leves))

table(H$Dum)         

# 4

H = H %>% 
  set_variable_labels(Dum = 'Gioi tinh')
dfSummary(H$Dum)%>% view()

H$Dum = H$Dum %>% 
  factor(levels = c(1,2,3),
         labels = c('Nam', 'Nu', 'Khac'))
dfSummary(H$Dum)%>% view()


# Part 2

# 5
Dum = as.data.frame(table(H$Dum))
Dum_d = Dum %>%
  mutate(Freq = Freq/sum(Freq))

# 6
Q = H[,c(3,4,5,6,8)]
dfSummary(Q)%>% view()

# 7
Q1 = H[,c(3:8)]
Q2 = Q1 %>%
  group_by(Dum)%>%
  summarise(min1= min(X1), min2= min(X2),min3= min(X3),min4= min(X4),
            max1= max(X1),max2= max(X2),max3= max(X3), max4= max(X4),
            mean1= mean(X1), mean2= mean(X2), mean3= mean(X3), mean4= mean(X4),
            median1= median(X1),median2= median(X2),median3= median(X3),median4= median(X4),
            sd1= sd(X1),sd2= sd(X2),sd3= sd(X3),sd4= sd(X4) )
           
# 8

quantile(H$Y)

# 9

ggplot(H, aes(x = Y))+
  geom_histogram()+
  ggtitle('Histogram')

ggplot(H, aes(x = Y))+
  geom_density()+
  ggtitle('density')

ggplot(H, aes(y= Y))+
  geom_boxplot()+
  ggtitle('Boxplot')

# 10

ggplot(H, aes(x = Y, col = Dum))+
  geom_histogram()+
  ggtitle('Histogram')

ggplot(H, aes(x = Y, col = Dum))+
  geom_density()+
  ggtitle('density')

ggplot(H, aes(y= Y, col = Dum))+
  geom_boxplot()+
  ggtitle('Boxplot')

# 11

ggplot(H, aes(x = X1, y = Y))+
  geom_point()

# 12

ggplot(H, aes(x = X1, y = Y, col = Dum))+
  geom_point()

# 13

ggplot(H, aes(x = X1, y = Y))+
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm')

# 14

ggplot(H, aes(x = X1, y = Y, col = Dum))+
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm')


# Part 3

cor(H[,3:6], method = 'pearson')

# Part 4

lm = lm(Y~ X1 + X2 + X3 + X4, data= H)
lm 
summary(lm)
confint(lm)



