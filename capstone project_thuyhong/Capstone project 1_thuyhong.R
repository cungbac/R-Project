library(readxl)
library(dplyr)
library(ggplot2)
library(summarytools)
#phan 1
##1.Import data
data=read_xlsx("Data 3_Hong.xlsx")
str(data)
##2.Xoa dong du lieu thieu logic
summary(data)
table(data$Dum)
data$Dum=as.factor(data$Dum)
data1=data%>%
  filter(Dum ==22)
##3.Tim du lieu bi missing
summary(data)
data2=data%>%
  filter(is.na(Dum))
data3=data%>%
  filter(is.na(Team))
data4=data%>%
  filter(is.na(Y))

data=data%>%
  mutate(Dum=replace(Dum, is.na(Dum),getmode(Dum)),
         Dum = replace(Dum, Dum == 22 , getmode(Dum)),
         Team = replace(Team, is.na(Team),getmode(Team)),
         Y = replace(Y,is.na(Y),mean(Y,na.rm = T)))
data=data %>%
  filter(Dum!=22) %>%
  droplevels()

summary(data)
data$Team=as.factor(data$Team)
data$idnum=as.factor(data$idnum)
 ##4, Gan label & value label
##label
label(data$Dum) <- "Gioi tinh"
dfSummary(data)%>%view()
levels(data$Dum)
##value label
data$Dum=factor(data$Dum, levels =c (1,2,3), labels=c("Nam", "Nu", "Khac"))

#phan 2: thong ke mo ta
##5. Thong ke mo ta bien dinh tinh
summary(data$Dum)
dfSummary(data$Dum)%>%view()

##6. thong ke mo ta bien so dinh luong
data_num=data %>%
  select(X1, X2, X3, X4, Y)
summary(data_num)
dfSummary(data_num)%>% view()
descr(data_num)
##7thong ke bang cheo bien dinh tinh Dum mo ta phia tren theo bien dinh luong
data_Dum=data %>%
  select(Dum, X1, X2, X3, X4, Y)%>%
  group_by(Dum)%>%
  summarise(mean_1=mean(X1),min_1=min(X1),max_1=max(X1), sd_1=sd(X1),
            mean_2=mean(X2),min_2=min(X2),max_2=max(X2), sd_2=sd(X2),
            mean_3=mean(X3),min_3=min(X3),max_3=max(X3), sd_3=sd(X3),
            mean_4=mean(X4),min_4=min(X4),max_4=max(X4), sd_4=sd(X4))

##8. thong ke tu phan vi bien phu thuoc
mean(data$Y)
median(data$Y)
quantile(data$Y)
IQR(data$Y)
data_Y=data%>%
  select(Y)
descr(data_Y) %>% view()

##9.ve do thi histogram, density, box-plot bien phu thuoc
ggplot(data, aes(x=Y))+
  geom_histogram()

ggplot(data, aes(x=Y))+
  geom_density()

ggplot(data, aes(x=1, y=Y))+
  geom_boxplot()
#10.ve do thi histogram, density, box-plot bien phu thuoc. bien phu thuoc su dung colour=Dummy
ggplot(data, aes(x=Y))+
  geom_histogram(aes(col=Dum))

ggplot(data, aes(x=Y))+
  geom_density((aes(col=Dum)))

ggplot(data, aes(x=1, y=Y))+
  geom_boxplot((aes(col=Dum)))
a#11. ve do thi scatter giua bien phu thuoc y va bien doc lap X1
ggplot(data, aes(x=X1, y=Y))+
  geom_point()
##12.ve do thi scatter giua bien phu thuoc y va bien doc lap X1, su dung colour=Dummy
ggplot(data, aes(x=X1, y=Y))+
  geom_point(aes(col=Dum))
##13.ve do thi scatter giua bien phu thuoc y va bien doc lap X1 +duong hoi quy
ggplot(data, aes(x=X1, y=Y))+
  geom_point()+ geom_smooth(method ="lm")
##14.ve do thi scatter giua bien phu thuoc y va bien doc lap X1 +duong hoi quy,su dung colour=Dummy
ggplot(data, aes(x=X1, y=Y))+
  geom_point(aes(col=Dum))+ geom_smooth(method ="lm", col="black")

#phan 3. phaan tich ma tran tuong quan 
##1 ma tran tuong quan bien doc lap dinh luong
data_X=data %>%
  select( X1, X2, X3, X4)
cov(data_X, method="pearson")
cor(data_X)
#phan 4
##4.1Phan tich hoi quy 
