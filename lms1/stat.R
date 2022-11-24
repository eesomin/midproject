##### 다시실습 #####

# 전국 코로나
cov <- read.csv("C:/sm/mid_project/data/전국코로나확진자수.csv")
cov <- cov
View(cov)

mov <- read.csv("C:/sm/mid_project/data/연도별영화관객수.csv")
mov <- mov[c(2,3,5)]  # 인덱스 및 매출액 제외(필요한 것만)
View(mov)

library(dplyr)
library(moonBook)
library(ggplot2)

# 시각화




# 분석 1
# H0: 연도별로 영화 관람객 수에 차이가 없다
# H1: 연도별로 영화 관람객 수에 차이가 있다
out1 <- aov(movie ~ year, data=mov) 
shapiro.test(resid(out))   # 정규분포 만족 x

kruskal.test(movie ~ year, data=mov)  # 차이가 있다


# 사후검정 - (2019-2020)과 (2019-2021)은 차이가 있다
library(pgirmess)

kruskalmc(movie ~ year, data=mov)

# 분석 2 (2020.03 ~ 2021.12)
# H0 : 코로나 확진자 수와 영화 관람객 수가 상관이 없다
# H1 : 코로나 확진자 수와 영화 관람객 수가 상관이 있다

# 코로나 확진자 수가 2020년 1,2월이 없기 때문에 영화 1,2월 행 삭제

mov.1 <- mov %>% filter((year=="2020")|(year=="2021"))
mov.1 <- mov.1[-2:-1,]  # 1,2행 삭제
cov.1 <- cov %>% filter((year=="2020")|(year=="2021"))
cov.1 <- cov.1[,-1]  # x 컬럼 삭제

cbind(cov.1, mov.1)

data <- merge(cov.1,mov.1, by=c("year","month"))
data <- data %>% group_by(year) %>% arrange(year,month)
View(data)
str(data)

plot(cov.1$incDec)
# 코로나와 영화관객수 산점도
ggplot(data, aes(x=incDec,y=movie)) + geom_point()

# data$year_dummy <- ifelse(data$year=="2020",0,1)
# data$year <- factor(data$year)
# data$month <- factor(data$month)

cor(data$incDec,data$movie,method="pearson")   # r = 0.5651239
cor.test(data$incDec,data$movie,method="pearson")    # 0.05보다 작으므로 상관 있다

data.1 <- data[1:21,]
ggplot(data.1, aes(x=incDec,y=movie)) + geom_point()
cor.test(data.1$incDec,data.1$movie,method="pearson") 

# 분석 3
# H0 : 코로나 확진자 수가 영화관객수에 영향을 안준다
# H1 : 코로나 확진자 수가 영화관객수에 영향을 준다

View(data)
fit <- lm(movie ~ incDec, data=data)
fit

plot(movie ~ incDec, data=data)
abline(fit, col="red")

summary(fit)   # 결정계수 볼 수 있음   28%로 올랐다......

# 분석 4
# H0 : 2020년도에 월별로 


ott서비스 영화 시청과 영화관 관객수 상관분석