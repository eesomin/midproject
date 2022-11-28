# 주제 : 연도별 OTT 시청인구 비율의 비교

##### 1. 데이터 파일 불러오기 #####

mydata <- read.csv('C:/Kimsunwoo/teamprojectdata/ksw/ott.csv', header=T)
View(mydata)

##### 2. 데이터 전처리-컬럼명 변경- 및 범주화 #####

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(reshape2)
library(gridExtra)
library(colorspace)

mydata[mydata$구분별.2.=='남',]
mydata[mydata$구분별.2.=='여',]

mydata1 <- mydata
names(mydata1)

mydata1<-rename(mydata1, cat1=구분별.1., cat2=구분별.2., 총인구.2019=X2019, 시청경험_있음.2019=X2019.1, 시청경험_없음.2019=X2019.2, 총인구.2020=X2020, 시청경험_있음.2020=X2020.1, 시청경험_없음.2020=X2020.2, 총인구.2021=X2021, 시청경험_있음.2021=X2021.1, 시청경험_없음.2021=X2021.2)
mydata1 <- mydata1[-1,]

View(mydata1)

summary(mydata1)

str(mydata1)

sex <- subset(mydata1, cat1 == '성별')
sex
age <- subset(mydata1, cat1 == '연령')
age
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

##### 3. 남자와 여자 년도별 OTT시청 비율 증가량 비교 #####

man <- subset(mydata1, cat2 == '남')

woman <- subset(mydata1, cat2 == '여')

man1 <- man %>% select(시청경험_있음.2019, 시청경험_있음.2020, 시청경험_있음.2021)
man1
woman1 <- woman %>% select(시청경험_있음.2019, 시청경험_있음.2020, 시청경험_있음.2021)
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


### 겹친그래프
par(mfrow=c(1,1))
plot(long.man$pop, type='o',xlab = 'man', ylab='percent',xlim=c(1,3), ylim=c(0,100),xaxt = "n", col='blue',lty=1)
lines(long.woman$pop, type='o',xlab = 'woman',ylab='percent',xlim=c(1,3), ylim=c(0,100),xaxt = "n",col='red',lty=1)
axis(1, at=1:3, labels = c('2019', '2020', '2021'))

legend("bottomright", c("남자시청비율", "여자시청비율"), 
       col = c('blue','red'),lty=1)


###### 4. 나이대에 따라 년도별 OTT시청 비율 증가량 비교 #####

age
age1 <- age %>% select(cat2, 시청경험_있음.2019, 시청경험_있음.2020, 시청경험_있음.2021)
View(age1)

age10d <- subset(age1, cat2 == '만10대미만')
age10to19 <- subset(age1, cat2 == '만10-19세')
age20to29 <- subset(age1, cat2 == '만20-29세')
age30to39 <- subset(age1, cat2 == '만30-39세')
age40to49 <- subset(age1, cat2 == '만40-49세')
age50to59 <- subset(age1, cat2 == '만50-59세')
age60to69 <- subset(age1, cat2 == '만60-69세')
age70u <- subset(age1, cat2 == '만70세이상')


long.age10d <- age10d %>% gather(key = years, value = pop)
long.age10d <-long.age10d[-1,]
long.age10to19 <- age10to19 %>% gather(key = years, value = pop)
long.age10to19 <- long.age10to19[-1,]
long.age20to29 <- age20to29 %>% gather(key = years, value = pop)
long.age20to29 <- long.age20to29[-1,]
long.age30to39 <- age30to39 %>% gather(key = years, value = pop)
long.age30to39 <- long.age30to39[-1,]
long.age40to49 <- age40to49 %>% gather(key = years, value = pop)
long.age40to49 <- long.age40to49[-1,]
long.age50to59 <- age50to59 %>% gather(key = years, value = pop)
long.age50to59 <- long.age50to59[-1,]
long.age60to69 <- age60to69 %>% gather(key = years, value = pop)
long.age60to69 <- long.age60to69[-1,]
long.age70u <- age70u %>% gather(key = years, value = pop)
long.age70u <- long.age70u[-1,]


par(mfrow=c(1,1))
plot(long.age10d$pop,type='o',xlim=c(1,4), ylim=c(0,100),xaxt = "n",xlab = 'year', ylab='percent',col=1)
axis(1, at=1:3, labels = c('2019', '2020', '2021'))
lines(long.age10to19$pop,type='o',col=2,pch=0)
lines(long.age20to29$pop,type='o',col=3,pch=2)
lines(long.age30to39$pop,type='o',col=4,pch=3)
lines(long.age40to49$pop,type='o',col=5,pch=4)
lines(long.age50to59$pop,type='o',col=6,pch=5)
lines(long.age60to69$pop,type='o',col=7,pch=6)
lines(long.age70u$pop,type='o',col=8,pch=7)

legend("topright", c("만10대 미만", "만10세-19세", "만20세-29세", "만30-39세", "만40-49세", "만50-59세", "만60-69세", "만70세 이상"), 
       col = c(1,2,3,4,5,6,7,8),lty=1)



##### 5. 가구수와 유료결제시청비율의 비교 #####

### 1) 데이터 준비
mydata2 <- read.csv('C:/Kimsunwoo/teamprojectdata/ksw/OTT유료결제_이용여부.csv', header=T)
View(mydata2)

moneydata <- mydata2
names(moneydata)

### 2) 전처리
moneydata<-rename(moneydata, cat1=구분별.1., cat2=구분별.2., 총인구.2019=X2019, 결제경험_있음.2019=X2019.1, 결제경험_없음.2019=X2019.2, 총인구.2020=X2020, 결제경험_있음.2020=X2020.1, 결제경험_없음.2020=X2020.2)
moneydata <- moneydata[-1,]

View(moneydata)

### 3) 가구원 수 뽑아오기

family.money <- subset(moneydata, cat1 == '가구원수1')
family.money

family.money1 <- family.money %>% select(cat2, 결제경험_있음.2019, 결제경험_있음.2020)
family.money1

people1 <- subset(family.money1, cat2 == '1인가구')
people2 <- subset(family.money1, cat2 == '2인가구')
people3 <- subset(family.money1, cat2 == '3인이상가구')

long.people1 <- people1 %>% gather(key = years, value = pop)
long.people2 <- people2 %>% gather(key = years, value = pop)
long.people3 <- people3 %>% gather(key = years, value = pop)

par(mfrow=c(1,1))
plot(long.people1$pop,type='o',xlim=c(1,4), ylim=c(0,30),xaxt = "n",xlab = 'year', ylab='percent',col=1)
axis(1, at=2:3, labels = c('2019', '2020'))
lines(long.people2$pop,type='o',col=2,pch=0)
lines(long.people3$pop,type='o',col=3,pch=2)

legend("topright", c("1인가구", "2인가구", "3인이상가구"), 
       col = c(1,2,3),lty=1)

### 4) 결론 : OTT는 한 번 구독하면 최대 4명까지 공유 가능하기에 1인가구가 유료 구독 비율이 가장 높았고 가구수가 많아질수록 유룍독 비율은 줄어들었다. 이로써 OTT서비스는 10대에서 39세의 독신층을 겨냥한 상품을 판매하는 것이 가장 이익이 많이 날 것임을 예상할 수 있다.


##### 6. 소득별 분석 #####
View(mydata1)
income

income.y <-  income %>% select(cat2, 시청경험_있음.2019, 시청경험_있음.2020, 시청경험_있음.2021)
View(income.y)

income1 <- subset(income.y, cat2 == '소득없음')
income2 <- subset(income.y, cat2 == '50만원미만')
income3 <- subset(income.y, cat2 == '50-100만원미만')
income4 <- subset(income.y, cat2 == '100-200만원미만')
income5 <- subset(income.y, cat2 == '200-300만원미만')
income6 <- subset(income.y, cat2 == '300-400만원미만')
income7 <- subset(income.y, cat2 == '400-500만원미만')
income8 <- subset(income.y, cat2 == '500만원이상')

long.income1 <- income1 %>% gather(key = years, value = pop)
long.income2 <- income2 %>% gather(key = years, value = pop)
long.income3 <- income3 %>% gather(key = years, value = pop)
long.income4 <- income4 %>% gather(key = years, value = pop)
long.income5 <- income5 %>% gather(key = years, value = pop)
long.income6 <- income6 %>% gather(key = years, value = pop)
long.income7 <- income7 %>% gather(key = years, value = pop)
long.income8 <- income8 %>% gather(key = years, value = pop)

par(mfrow=c(1,1))
plot(long.income1$pop,type='o',xlim=c(2,4), ylim=c(0,100),xaxt = "n",xlab = 'year', ylab='percent',col=1)
axis(1, at=2:4, labels = c('2019', '2020', '2021'))
lines(long.income2$pop,type='o',col=2,pch=0)
lines(long.income3$pop,type='o',col=3,pch=2)
lines(long.income4$pop,type='o',col=4,pch=3)
lines(long.income5$pop,type='o',col=5,pch=4)
lines(long.income6$pop,type='o',col=6,pch=5)
lines(long.income7$pop,type='o',col=7,pch=6)
lines(long.income8$pop,type='o',col=8,pch=7)

legend("bottomright", c("소득없음", "50만원미만", "50-100만원미만", "100-200만원미만", "200-300만원미만", "300-400만원미만", "400-500만원미만", "500만원이상"), 
       col = c(1,2,3,4,5,6,7,8),lty=1, cex=0.5)

### 소득없음이 비율이 높은것은 대부분 만 10대 미만이거나 10대-19세의 나이대가 이곳에 있기 때문. 

##### 6. 평균이용시간과 평균이용빈도 #####
### 데이터 불러오기
mydata3 <- read.csv('C:/Kimsunwoo/teamprojectdata/ksw/(미디어 통계포털 - KISDI STAT)OTT서비스_이용시간.csv', header=T)
View(mydata3)
mydata4 <- read.csv('C:/Kimsunwoo/teamprojectdata/ksw/(미디어 통계포털 - KISDI STAT)OTT서비스_이용빈도.csv', header=T)
View(mydata4)

time <- mydata3
many <- mydata4


### 평균이용시간 전처리
time <- rename(time, cat1=구분별.1., cat2=구분별.2., 총인구.2020=X2020, '5m-Under'=X2020.1, '5m-10m'=X2020.2, '10m-30m'=X2020.3, '30m-1h'=X2020.4, '1h-2h'=X2020.5, '2h-Up'=X2020.6)
time <- time[-1,]

ages <- subset(time, cat1 == '연령')

ages.2020 <- ages %>% select(cat2 ,'5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')

View(ages.2020)

X <- ages.2020[(2:4),]

ages1 <- subset(ages.2020, cat2 == '만10-19세')
ages1 <- ages1 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages2 <- subset(ages.2020, cat2 == '만20-29세')
ages2 <- ages2 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages3 <- subset(ages.2020, cat2 == '만30-39세')
ages3 <- ages3 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
View(ages1)

long.ages1 <- ages1 %>% gather(key = times, value = percent)
long.ages2 <- ages2 %>% gather(key = times, value = percent)
long.ages3 <- ages3 %>% gather(key = times, value = percent)

### 평균이용시간 그래프
color <- c('5m-Under'='#999999', '5m-10m'='#999999', '10m-30m'='#000000', '30m-1h'='#000000', '1h-2h'='#999999', '2h-Up'='#999999')

color1 <- c('5m-Under'='#999999', '5m-10m'='#999999', '10m-30m'='#999999', '30m-1h'='#000000', '1h-2h'='#000000', '2h-Up'='#999999')

a <- ggplot(long.ages1, aes(times, percent)) + xlab("만10-19세") + geom_col(fill=color)+ scale_fill_manual(values=color) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')) + theme_bw() 

b <- ggplot(long.ages2, aes(times, percent))+ xlab("만20-29세") + geom_col(fill=color1)+
  scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()

c <- ggplot(long.ages3, aes(times, percent)) + xlab("만30-39세") + geom_col(fill=color)+
  scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()

grid.arrange(a,b,c, nrow=2, ncol=2)



### 빈도수 전처리
many <- mydata4

many <- rename(many,cat1=구분별.1., cat2=구분별.2., population=X2020, 'manytimes.day'=X2020.1, 'one.day'=X2020.2, 'week.5-7'=X2020.3, 'week.3-4'=X2020.4, 'week1-2'=X2020.5, 'month.1-3'=X2020.6,'month.U1'=X2020.7, 'year.1'=X2020.8)
many <- many[-1,]

m.ages <- subset(many, cat1 == '연령')

m.ages.2020 <- m.ages %>% select(cat2 ,'manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')

View(m.ages.2020)

Y <- m.ages.2020[(2:4),]
View(Y)

m.ages1 <- subset(m.ages.2020, cat2 == '만10-19세')
m.ages1 <- m.ages1 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages2 <- subset(m.ages.2020, cat2 == '만20-29세')
m.ages2 <- m.ages1 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages3 <- subset(m.ages.2020, cat2 == '만30-39세')
m.ages3 <- m.ages1 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')

long.m.ages1 <- m.ages1 %>% gather(key = times, value = percent)
long.m.ages2 <- m.ages2 %>% gather(key = times, value = percent)
long.m.ages3 <- m.ages3 %>% gather(key = times, value = percent)

### 시각화
color2 <- c('manytimes.day'='#000000', 'one.day'='#000000', 'week.5-7'='#999999', 'week.3-4'='#999999', 'week1-2'='#999999', 'month.1-3'='#999999','month.U1'='#999999', 'year.1'='#999999')



A <- ggplot(long.m.ages1, aes(times, percent)) + xlab("만10-19세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))
B <- ggplot(long.m.ages2, aes(times, percent)) + xlab("만20-29세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))
C <- ggplot(long.m.ages3, aes(times, percent)) + xlab("만30-39세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))

#묶어서 한 화면에 나타내기
grid.arrange(A,B,C, nrow=2, ncol=2)


##### 7.평균이용시간 전체 나이대로 ######

View(ages.2020)

ages0 <- subset(ages.2020, cat2 == '만10대미만')
ages0 <- ages0 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages1 <- subset(ages.2020, cat2 == '만10-19세')
ages1 <- ages1 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages2 <- subset(ages.2020, cat2 == '만20-29세')
ages2 <- ages2 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages3 <- subset(ages.2020, cat2 == '만30-39세')
ages3 <- ages3 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages4 <- subset(ages.2020, cat2 == '만40-49세')
ages4 <- ages4 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages5 <- subset(ages.2020, cat2 == '만50-59세')
ages5 <- ages5 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages6 <- subset(ages.2020, cat2 == '만60-69세')
ages6 <- ages6 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')
ages7 <- subset(ages.2020, cat2 == '만70세이상')
ages7 <- ages7 %>% select('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')


long.ages0 <- ages0 %>% gather(key = times, value = percent)
long.ages1 <- ages1 %>% gather(key = times, value = percent)
long.ages2 <- ages2 %>% gather(key = times, value = percent)
long.ages3 <- ages3 %>% gather(key = times, value = percent)
long.ages4 <- ages4 %>% gather(key = times, value = percent)
long.ages5 <- ages5 %>% gather(key = times, value = percent)
long.ages6 <- ages6 %>% gather(key = times, value = percent)
long.ages7 <- ages7 %>% gather(key = times, value = percent)

### 평균이용시간 그래프
color <- c('5m-Under'='#999999', '5m-10m'='#999999', '10m-30m'='#000000', '30m-1h'='#000000', '1h-2h'='#999999', '2h-Up'='#999999')

color1 <- c('5m-Under'='#999999', '5m-10m'='#999999', '10m-30m'='#999999', '30m-1h'='#000000', '1h-2h'='#000000', '2h-Up'='#999999')

color3 <- c('5m-Under'='#999999', '5m-10m'='#999999', '10m-30m'='#000000', '30m-1h'='#999999', '1h-2h'='#999999', '2h-Up'='#000000')

color4 <- c('5m-Under'='#999999', '5m-10m'='#000000', '10m-30m'='#000000', '30m-1h'='#999999', '1h-2h'='#999999', '2h-Up'='#999999')

d <- ggplot(long.ages0, aes(times, percent)) + xlab("만10대미만") + geom_col(fill=color)+ scale_fill_manual(values=color)  + 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')) + theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1))


a <- ggplot(long.ages1, aes(times, percent)) + xlab("만10-19세") + geom_col(fill=color)+ scale_fill_manual(values=color)+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up')) + theme_bw()  +
  theme(axis.text.x=element_text(angle=90, hjust=1)) 

b <- ggplot(long.ages2, aes(times, percent))+ xlab("만20-29세") + geom_col(fill=color1)+
  scale_fill_manual(values=color)+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

c <- ggplot(long.ages3, aes(times, percent)) + xlab("만30-39세") + geom_col(fill=color)+
  scale_fill_manual(values=color)+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

e <- ggplot(long.ages4, aes(times, percent)) + xlab("만40-49세") + geom_col(fill=color)+
  scale_fill_manual(values=color)+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

f <- ggplot(long.ages5, aes(times, percent)) + xlab("만50-59세") + geom_col(fill=color3)+
  scale_fill_manual(values=color3)+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

g <- ggplot(long.ages6, aes(times, percent)) + xlab("만60-69세") + geom_col(fill=color4)+
  scale_fill_manual(values=color4)+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

h <- ggplot(long.ages7, aes(times, percent)) + xlab("만70세이상") + geom_col(fill=color3)+
  scale_fill_manual(values=color3)+ 
  scale_x_discrete(limits = c('5m-Under', '5m-10m', '10m-30m', '30m-1h', '1h-2h', '2h-Up'))+ theme_bw()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

grid.arrange(d,a,b,c,e,f,g,h, nrow=3, ncol=3)



##### 8.평균 이용빈도 전체 나이대로 ######

m.ages.2020 <- m.ages %>% select(cat2 ,'manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')

View(m.ages.2020)

m.ages0 <- subset(m.ages.2020, cat2 == '만10대미만')
m.ages0 <- m.ages0 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages1 <- subset(m.ages.2020, cat2 == '만10-19세')
m.ages1 <- m.ages1 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages2 <- subset(m.ages.2020, cat2 == '만20-29세')
m.ages2 <- m.ages2 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages3 <- subset(m.ages.2020, cat2 == '만30-39세')
m.ages3 <- m.ages3 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages4 <- subset(m.ages.2020, cat2 == '만40-49세')
m.ages4 <- m.ages4 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages5 <- subset(m.ages.2020, cat2 == '만50-59세')
m.ages5 <- m.ages5 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages6 <- subset(m.ages.2020, cat2 == '만60-69세')
m.ages6 <- m.ages6 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')
m.ages7 <- subset(m.ages.2020, cat2 == '만70세이상')
m.ages7 <- m.ages7 %>% select('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1')

long.m.ages0 <- m.ages0 %>% gather(key = times, value = percent)
long.m.ages1 <- m.ages1 %>% gather(key = times, value = percent)
long.m.ages2 <- m.ages2 %>% gather(key = times, value = percent)
long.m.ages3 <- m.ages3 %>% gather(key = times, value = percent)
long.m.ages4 <- m.ages4 %>% gather(key = times, value = percent)
long.m.ages5 <- m.ages5 %>% gather(key = times, value = percent)
long.m.ages6 <- m.ages6 %>% gather(key = times, value = percent)
long.m.ages7 <- m.ages7 %>% gather(key = times, value = percent)

### 그래프
color2 <- c('manytimes.day'='#000000', 'one.day'='#000000', 'week.5-7'='#999999', 'week.3-4'='#999999', 'week1-2'='#999999', 'month.1-3'='#999999','month.U1'='#999999', 'year.1'='#999999')

D <- ggplot(long.m.ages0, aes(times, percent)) + xlab("만10대미만") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))

A <- ggplot(long.m.ages1, aes(times, percent)) + xlab("만10-19세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))
B <- ggplot(long.m.ages2, aes(times, percent)) + xlab("만20-29세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))
C <- ggplot(long.m.ages3, aes(times, percent)) + xlab("만30-39세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))

E <- ggplot(long.m.ages4, aes(times, percent)) + xlab("만40-49세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))

G <- ggplot(long.m.age5, aes(times, percent)) + xlab("만50-59세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))

H <- ggplot(long.m.ages6, aes(times, percent)) + xlab("만60-69세") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))

I <- ggplot(long.m.ages7, aes(times, percent)) + xlab("만70세이상") + geom_col(fill=color2)+ scale_fill_manual(values=color)+
  theme(axis.text.x=element_text(angle=90, hjust=1)) + 
  scale_x_discrete(limits = c('manytimes.day', 'one.day', 'week.5-7', 'week.3-4', 'week1-2', 'month.1-3','month.U1', 'year.1'))

grid.arrange(D,A,B,C,E,G,H,I, nrow=3, ncol=3)

