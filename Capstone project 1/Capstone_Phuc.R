#Library
library(dplyr)
library(ggplot2)
library(summarytools)
library(readxl)
library(labelled)
library(expss)
library(car)
#Phan 1:
#1 Import
data= read_excel(file.choose())
#2 Clean data
summary(data)
data$Dum= as.integer(data$Dum)
data$Dum= as.factor(data$Dum)

 data %>%
  filter(Dum ==43) 
data_1= data[-31,]

data_1$Dum= data_1$Dum %>%
  factor(levels = c(1,2,3), 
         labels = c("Nam", "Nu", "Khac"))
dfSummary(data_1)%>% view()
#3
level= table(data_1$Dum)
mode_level= names(which.max(level))

data_2= data_1 %>%
  mutate(X1= replace(X1, is.na(X1), mean(X1, na.rm = T)),
         Dum= replace(Dum,is.na(Dum), mode_level),
         Y= replace(Y, is.na(Y), mean(Y, na.rm = T)))
summary(data_2)

#4
summary(data_2)
data_3= apply_labels(data_2, Dum= "Gioi tinh")
dfSummary(data_3)%>% view()  

#Phan 2
#5
Dum1=table(data_3$Dum)
Dum1=as.data.frame(Dum1)
table1=Dum1 %>%
  mutate(Percent= Freq/sum(Freq))

#6
table2= data_3%>%
  select(-Dum)
dfSummary(table2)%>% view()
#7
  value=data_3 %>%
    summarise(meanX1=mean(X1),meanX2=mean(X2),meanX3=mean(X3),meanX4=mean(X4),
              minX1=min(X1),minX2=min(X2),minX3=min(X3),minX4=min(X4),
              maxX1= max(X1),maxX2= max(X2),maxX3= max(X3),maxX4= max(X4))

#8
x1= table2%>%
  select(X1)%>%
  arrange()
class(x1)
x1= as.matrix(x1)
quanx1=quantile(x1)

x2= table2%>%
  select(X2)%>%
  arrange()
x2= as.matrix(x2)
quanx2=quantile(x2)

x3= table2%>%
  select(X3)%>%
  arrange()
x3= as.matrix(x3)
quanx3=quantile(x3)

x4= table2%>%
  select(X4)%>%
  arrange()
x4= as.matrix(x4)
quanx4=quantile(x4)

y= table2%>%
  select(Y)%>%
  arrange()
y= as.matrix(y)
quany=quantile(y)

#ggplot
#9
ggplot(data_3, aes(x= Y, colour= Dum))+
  geom_histogram()

ggplot(data_3, aes(x= Y, color= Dum))+
  geom_density()


ggplot(data_3, aes(y= Y, color= Dum))+
  geom_boxplot()

#11
table_4= data_3 %>%
  select(Dum,X1,Y) %>%
  as.data.frame()
str(table_4)

ggplot(table_4, aes(x= X1, y= Y))+
  geom_point()
  
#12
#c1:
table_4
fit1= lm(X1~Y, data= table_4)
summary(fit1)
plot(X1~Y, data= table_4)
abline(fit1)

#c2
ggplot(data_3, aes(x= X1,y= Y))+
  geom_point()+
  stat_smooth(method = "lm", col="red")

#13
ggplot(data_3, aes(x= X1,y= Y, color= Dum))+
  geom_point()+
  stat_smooth(method = "lm", col="red")

#Phan3
ti= data_3%>%
  select(X1, X2, X3, X4) %>%
  as.matrix()
ti
class(ti)
cov(ti)
tuongquan=cor(ti)
corr1=as.table(tuongquan)

#Phan4
hoiquy1= lm(formula= Y~ X1+X2+X3+X4, data= data_3)
summary(hoiquy1)

hoiquy2=lm(formula= Y~ Dum+X1+X2+X3+X4, data= data_3)
summary(hoiquy2)

hoiquy3=lm(formula= Y~ Dum*X1+Dum+X1+X2+X3+X4, data= data_3)
summary(hoiquy3)

#kiem dinh da cong tuyen
vif(hoiquy3)


