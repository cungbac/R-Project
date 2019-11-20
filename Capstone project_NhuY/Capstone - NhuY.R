library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
#Phan 1. Lam sach du lieu
data <- read_xlsx('Data 2_Y.xlsx')
str(data)
summary(data)
data$Dum <- as.integer(data$Dum)
data$Dum <- as.factor(data$Dum)
data$Team <- as.factor(data$Team)
data$idnum <- as.factor(data$idnum)

data %>%
  filter(Dum == 23) #dong 4500

data %>%
  filter(is.na(X1)|is.na(X3)) #X1 dong 4488, x3 dong 4497

# Return the column names containing missing observations
list_na <- colnames(data)[apply(data,2,anyNA)]
list_na
#compute mean of missing variable
mean_na <- apply(data[,colnames(data) %in% list_na],2,mean,na.rm = T)
mean_na
## du lieu chi lay ten cot ma trong do co NA 
## 2 la tinh toan tren cot
## dung ham mean tinh tren cot 
## na.rm xoa NA

# Dum:
getmode <- function(x){
  y<-unique(x)
  y[which.max(table(y))]
}
getmode(data$Dum)

# replace NA: 2 ways
## use replace function
data <- data %>%
  mutate(X1 = replace(X1,is.na(X1),mean(X1,na.rm = T)),
         X3 = replace(X3,is.na(X3),mean(X3,na.rm = T)),
         Dum = replace(Dum,Dum == 23,getmode(Dum)))

## use replace_na function
data <- data %>%
  replace_na(list(X1 = mean_na[1],
                     X3 = mean_na[2])) #thay voi 1 so da cho nhat dinh

summary(data)

# Label
## Bien so dinh luong 
library(summarytools)
data$Dum <- factor(data$Dum,levels = c(1,2,3),labels = c('Nam','Nu','Khac'))
label(data$Dum)<- 'Gioi Tinh'
dfSummary(data) %>% view()


#Phan 2.Thong ke mo ta
## bien dinh tinh
tab <- table(data$Dum)
data_qual <- prop.table(tab)

## bien dinh luong
data_quan <- data %>%
  select(X1,X2,X3,X4,Y)%>%
  summarise(mean1 = mean(X1), sd1 = sd(X1), med1 = median(X1),min1 = min(X1),max1 = max(X1),
            mean2 = mean(X2), sd2 = sd(X2), med2 = median(X2),min2 = min(X2),max2 = max(X2),
            mean3 = mean(X3), sd3 = sd(X3), med3 = median(X3),min3 = min(X3),max3 = max(X3),
            mean4 = mean(X4), sd4 = sd(X4), med4 = median(X4),min4 = min(X4),max4 = max(X4),
            meanY = mean(Y), sdY = sd(Y), medY = median(Y),minY = min(Y),maxY = max(Y))
data_quan

## thong ke cheo
data_cross <- data %>%
  select(X1,X2,X3,X4,Y,Dum)%>%
  group_by(Dum)%>%
  summarise(mean1 = mean(X1), sd1 = sd(X1), med1 = median(X1),min1 = min(X1),max1 = max(X1),
            mean2 = mean(X2), sd2 = sd(X2), med2 = median(X2),min2 = min(X2),max2 = max(X2),
            mean3 = mean(X3), sd3 = sd(X3), med3 = median(X3),min3 = min(X3),max3 = max(X3),
            mean4 = mean(X4), sd4 = sd(X4), med4 = median(X4),min4 = min(X4),max4 = max(X4),
            meanY = mean(Y), sdY = sd(Y), medY = median(Y),minY = min(Y),maxY = max(Y))
data_cross

# quantile bien phu thuoc
quan_Y <- quantile(data$Y)

#PLOT bien phu thuoc
ggplot(data,aes(x=Y,fill = Dum))+
  geom_histogram()

ggplot(data,aes(x=Y,col = Dum))+
  geom_density()

ggplot(data,aes(y=Y,col = Dum))+
  geom_boxplot()

#Scatter Y va X1
ggplot(data,aes(x=X1,y=Y,col=Dum))+
  geom_point()+
  geom_smooth(method = 'lm')

#Ma tran tuong quan
matrix_cor <- data %>%
  select(X1,X2,X3,X4) %>%
  cor(method = 'pearson')

#linear model
lm<-lm(formula = Y~X1+X2+X3+X4,data = data)
summary(lm)
