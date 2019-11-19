##~~~~~~~~~~PHAN 1~~~~~~~~~~~
#~~~~~~`1~~~~~~~~.Import
library(readxl)
library(dplyr)
library(summarytools)
library(ggplot2)
data4=read_xlsx("Data 4_Lam.xlsx")
str(data4)
## Dung summary kiem tra bien NA va thieu logic
data4$Dum=as.factor(data4$Dum)
data4 %>%
  summary()
##ket qua
# X2: 1 NA
#Dum: thieu logic 1, NA:1
#Y: 1 NA
#~~~~~~~2~~~~~~.Xoa du lieu thieu logic
#tim du lieu thieu logic
data4 %>%
  filter(Dum=="23")
#=> Dong 47 cot Dum
#xoa
data4$Dum=as.character(data4$Dum)
data4[47,"Dum"]=NA #xoa
#~~~~~~3~~~.Tim missing value
##tim cot Y
data4 %>%
  filter(is.na(Y))
#=>Y na tai dong 297 cot y
## tim cot Dum
data4 %>%
  filter(is.na(Dum))
# Dum dong 47 95 129 cot dum
## tim cot X2
data4 %>%
  filter(is.na(X2))
# cot X2 dong 161

### thay the cot X2 va cot y va dum
level=table(data4$Dum)
model_level=names(level)[which(level==max(level))]

data4_new=data4 %>%
  mutate(X2=replace(X2,is.na(X2),
                    mean(X2,na.rm = T))) %>%
  mutate(Y=replace(Y,is.na(Y),
                   mean(Y,na.rm = T))) %>%
  mutate(Dum=replace(Dum,is.na(Dum),model_level))

## Kiem tra lai cac bien NA
data4_new$Dum=as.factor(data4_new$Dum)

data4_new%>%
  summary()
#~~~~~4~~~~~.
data4_new$Dum=data4_new$Dum %>%
  as.numeric()%>%
  factor(levels = c(1,2,3),
         labels = c("Nam","Nu","Khác"))

data4_new %>%
  summary()
#~~~~~~~~~~~~~~~~~~~~PHAN HAI~~~~~~~~~~
#~~~~~~5~~~~~~~.
##cach 1
freq(data4_new$Dum)
##cach 2
dfSummary(data4_new$Dum) %>%
  view()
##cach 3
prop.table(table(data4_new$Dum))
#~~~~~~~6~~~~~`.
##cach 1
data4_new %>%
  select("X1","X2","X3","X4","Y")%>%
  descr() %>%
  view()
##cach 2
X1=data4_new %>%
  summarise(mean(X1),
            median(X1),
            min(X1),
            max(X1),
            sd(X1),var(X1))
X2=data4_new %>%
  summarise(mean(X2),
            median(X2),
            min(X2),
            max(X2),
            sd(X2),var(X2))
X3=data4_new %>%
  summarise(mean(X3),
            median(X3),
            min(X3),
            max(X3),
            sd(X3),var(X3))
X4=data4_new %>%
  summarise(mean(X4),
            median(X4),
            min(X4),
            max(X4),
            sd(X4),var(X4))
data.frame(bind_cols(X1,X2,X3,X4)) 
#~~~~7~~~~~~
str(data4_new)
## mo ta bien dinh tinh Dum voi X1,X2,X3,X4
#cach 1
x1=data4_new %>%
  group_by(Dum)%>%
  summarise(mean(X1),
            median(X1),
            min(X1),
            max(X1),
            sd(X1),var(X1))
x2=data4_new %>% 
  group_by(Dum)%>%
  summarise(mean(X2),
            median(X2),
            min(X2),
            max(X2),
            sd(X2),var(X2))
x3=data4_new %>%
  group_by(Dum)%>%
  summarise(mean(X3),
            median(X3),
            min(X3),
            max(X3),
            sd(X3),var(X3))
x4=data4_new %>%
  group_by(Dum)%>%
  summarise(mean(X4),
            median(X4),
            min(X4),
            max(X4),
            sd(X4),var(X4))
bind_cols(x1,x2,x3,x4)
#cach 2
tk7=data4_new %>%
  group_by(Dum)%>%
  summarise(mean(X1),
            median(X1),
            min(X1),
            max(X1),
            sd(X1),var(X1),
            mean(X2),
            median(X2),
            min(X2),
            max(X2),
            sd(X2),var(X2),
            mean(X3),
            median(X3),
            min(X3),
            max(X3),
            sd(X3),var(X3),
            mean(X4),
            median(X4),
            min(X4),
            max(X4),
            sd(X4),var(X4))
#~~~~~~~8~~~.thong ke tu phan vi
#cach 1
quantile(data4_new$Y)
#cach 2
dfSummary(data4_new$Y) %>%
  view()
data4_new$Y %>%
  descr() %>%
  view()
   
#~~~~~~~9~~~.do thi histofram
##histogram
ggplot(data4_new, aes(x=Y)) + 
  geom_histogram() +
  ggtitle("Do thi histogram bien phu thuoc")
##density
ggplot(data4_new,aes(x=Y))+
  geom_density()+
  ggtitle("Do thi density bien phu thuoc")
##box-plot
ggplot(data4_new,aes(x=1,y=Y))+
  geom_boxplot()+
  ggtitle("Do thi boxplot bien phu thuoc")
#~~~~~~~10~~~~~~~
##histogram
ggplot(data4_new, aes(x=Y)) + 
  geom_histogram(aes(col=as.factor(Dum)))+
  ggtitle("Do thi histogram bien phu thuoc")
##density
ggplot(data4_new,aes(x=Y))+
  geom_density(aes(col=as.factor(Dum)))+
  ggtitle("Do thi density bien phu thuoc")

##box-plot
ggplot(data4_new,aes(x=1,y=Y))+
  geom_boxplot(aes(col=as.factor(Dum)))+
  ggtitle("Do thi boxplot bien phu thuoc")

#~~~~~~11~~~~~~~~
ggplot(data4_new,aes(x=X1,y=Y))+
  geom_line()+
  ggtitle("Scatter Y va X1")
#~~~~~~12~~~~~~
ggplot(data4_new,aes(x=X1,y=Y))+
  geom_line(aes(col=as.factor(Dum)))+
  ggtitle("Scatter Y va X1")
#~~~~~~13~~~~~~
ggplot(data4_new,aes(x=X1,y=Y))+
  geom_line()+
  geom_smooth(method ="lm")+
  ggtitle("Scatter Y va X1 + hoi quy")
#~~~~~~~~14~~~~~
ggplot(data4_new,aes(x=X1,y=Y))+
  geom_line(aes(col=as.factor(Dum)))+
  geom_smooth(method = "lm")+
  ggtitle("Scatter Y va X1 + hoi quy")

#~~~~~~~~~~~~~~~~~~~PHAN 3~~~~~~~~~~~
#1.
biendinhluong=data4_new %>%
  select(c("X1","X2","X3","X4"))

cor(biendinhluong)
##nhan xet 




