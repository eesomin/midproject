# 주제 : 연도별 OTT 시청인구 비율의 비교

## 1. 데이터 파일 불러오기

mydata <- read.csv('C:/Kimsunwoo/teamprojectdata/ksw/ott.csv', header=T)
View(mydata)

result <- table(mydata$구분별.2., mydata$X2019.1)
result

## 2. 데이터 전처리-컬럼명 변경- 및 범주화

library(ggplot2)
library(dplyr)
library(tidyr)

mydata[mydata$구분별.2.=='남',]
mydata[mydata$구분별.2.=='여',]

mydata1 <- mydata
names(mydata1)

mydata1<-rename(mydata1, cat1=구분별.1., cat2=구분별.2., pop.2019=X2019, True.2019=X2019.1, False.2019=X2019.2, pop.2020=X2020, True.2020=X2020.1, False.2020=X2020.2, pop.2021=X2021, True.2021=X2021.1, False.2021=X2021.2)
View(mydata1)

summary(mydata1)

table(mydata1$pop.2019)
str(mydata1)

sex <- subset(mydata1, cat1 == '성별')
sex
age <- subset(mydata1, cat1 == '연령')
income <- subset(mydata1, cat1 == '월평균소득')
school <- subset(mydata1, cat1 == '학력1')
region <- subset(mydata1, cat1 == '지역')
region.1 <- subset(mydata1, cat1 == '지역구분2')
job.wide <- subset(mydata1, cat1 == '종사상지위')
job.narrow <- subset(mydata1, cat1 == '상세직업')
house <- subset(mydata1, cat1 == '주택형태')
family.1 <- subset(mydata1, cat1 == '가족구성')
family.2 <- subset(mydata1, cat1 == '가구원수')
family.3 <- subset(mydata1, cat1 == '가구주와의관계')

## 3. 남자와 여자 년도별 OTT시청 비율 증가량 비교

man <- subset(mydata1, cat2 == '남')
woman <- subset(mydata1, cat2 == '여')

man1 <- man %>% select(True.2019, True.2020, True.2021)
man1
woman1 <- woman %>% select(True.2019, True.2020, True.2021)
woman1

long.man <- man1 %>% gather(key = years, value = pop)
long.man

long.woman <- woman1 %>% gather(key = years, value = pop)
long.woman

long.man$pop
long.woman$pop


par(mfrow=c(1,2))
plot(long.man$pop, type='o',xlab = 'man', ylab='percent',xlim=c(1,3), ylim=c(0,100),xaxt = "n", col='blue')
axis(1, at=1:3, labels = c('2019', '2020', '2021'))

plot(long.woman$pop, type='o',xlab = 'woman',ylab='percent',xlim=c(1,3), ylim=c(0,100),xaxt = "n",col='red')
axis(1, at=1:3, labels = c('2019', '2020', '2021'))


## 4. 나이대에 따라 년도별 OTT시청 비율 증가량 비교

age
age1 <- age %>% select(cat2, True.2019, True.2020, True.2021)
View(age1)
