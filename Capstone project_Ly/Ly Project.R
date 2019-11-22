# Library:
library(dplyr)
library(readxl)
library(summarytools)
library(labelled)
library(ggplot2)
## phan 1: Lam sach du lieu
# 1. Import data
Data = read_xlsx('Data 7_Ly.xlsx')
str(Data)

# 2. Xoa du lieu:
table(Data$Dum)
which(Data$Dum == 22) #Thieu logic: dong 91
which(Data$Dum == 31) # Dong: 115
Data$Dum = as.factor(Data$Dum)
Data = Data %>%
  filter(Dum != 31 & Dum != 22)
table(Data$Dum)

# 3. Missing value:
summary(Data)
which(is.na(Data$X2)) # X2 Na row 201
which(is.na(Data$Y)) # Y Na row 151
which(is.na(Data$X1))
which(is.na(Data$X3))
which(is.na(Data$X2))
which(is.na(Data$X4))
which(is.na(Data$Team))
which(is.na(Data$Dum)) # Dum Na row 44

levels = table(Data$Dum)
mode_Dum = names(levels)[which(levels ==max(levels))]
Data = Data %>%
  mutate(Dum = replace(Dum, is.na(Dum), mode_Dum),
         X2 = replace(X2, is.na(X2), mean(X2, na.rm = TRUE)),
         Y = replace(Y, is.na(Y), mean(Y, na.rm = T)))

dfSummary(Data) %>%
  view()

## Gan thuoc tinh:

Data$Dum = Data$Dum %>% 
  factor(levels = c(1,2,3),
         labels = c('Nam', 'Nu', 'Khac'))
dfSummary(Data$Dum)%>% view()

Data = Data %>%
  set_variable_labels(Dum ='gioi tinh')

# Part 2: Thong ke mo ta:
## 5. Thong ke mo ta bien so dinh tinh:
dfSummary(Data$Dum) %>%
  view()
## 6.

dfSummary(Data[,c(3,4,5,6,8)]) %>%
  view()

## 7.

Data_cross = Data %>%
  group_by(Dum) %>%
  summarise(minX1 = min(X1), maxX1 = max(X1), medX1 = median(X1),
            minX2 = min(X2), maxX2 = max(X2), medX2 = median(X2),
            minX3 = min(X3), maxX3 = max(X3), medX3 = median(X3),
            minX4 = min(X4), maxX4 = max(X4), medX4 = median(X4),
            minY = min(Y), maxY = max(Y), medY = median(Y))

Data

## 8.
quantile(Data$Y)

## 9.
Data %>%
  ggplot(aes(x = Y)) +
  geom_histogram()

Data %>%
  ggplot(aes(x = Y)) +
  geom_density()

Data %>%
  ggplot(aes(y = Y)) +
  geom_boxplot()

## 10:
Data %>%
  ggplot(aes(x = Y, color = Dum)) +
  geom_histogram()

Data %>%
  ggplot(aes(x = Y, color = Dum)) +
  geom_density()

Data %>%
  ggplot(aes(y = Y, color = Dum)) +
  geom_boxplot()

## 11.
Data %>%
  ggplot(aes(X1, Y)) +
  geom_point()

## 12.
Data %>%
  ggplot(aes(X1, Y, col = Dum)) +
  geom_point()

## 13.
Data %>%
  ggplot(aes(X1, Y)) +
  geom_point() +
  geom_smooth(method ='lm')

## 14. 
Data %>%
  ggplot(aes(X1, Y, col = Dum)) +
  geom_point() +
  geom_smooth(method ='lm')

# Part 3:
cor(Data[, 3:6], method ='pearson') 
 ## pearson: danh gia muc do tuong quan tuyen tinh giua 2 bien dinh luong

# Part 4: phan tich hoi quy:
Data_lm = lm(data = Data, formula = Y ~ X1+X2+X3+X4)
summary(Data_lm)

confint(Data_lm)













