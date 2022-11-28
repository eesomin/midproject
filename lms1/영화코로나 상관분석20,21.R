##### 그냥 #####
mov <- read.csv("C:/sm/mid_project/data/상관분석을위한movie.csv")
cov <- read.csv("C:/sm/mid_project/data/상관분석을위한covid.csv")


data <- melt(mov, id.var="X", value.name="movie")

data1 <- melt(cov, id.var="X", value.name="covid")

# 코로나는 3월 1일 ~ 12월 30일까지, 영화는 3월 2일 ~ 12월 31일
data <- data[-1,]
data1 <- data1[-682,]

data <- data[3]
data1 <- data1[3]

is.na(data)

data <- na.omit(data)
data1 <- na.omit(data1)

View(data)
# 데이터 합치기

d <- data.frame(data1$covid,data$movie)

View(d)
is.na(d)
str(d)

# 상관계수
cor(d$data1.covid,d$data.movie)


# 상관분석
cor.test(d$data1.covid,d$data.movie, mothod="pearson")

plot(data.movie~data1.covid, data=d)

fit <- lm(data.movie~data1.covid, data=d)
fit

abline(fit, col="red")

summary(fit)  



View(mov)









c <- data1$covid
data <- data[2:3]
data1 <- data1[2:3]
View(data)









sum(mov$X20c3m4, na.rm = T)





d <- data.frame(data$movie, data1$covid)
View(d)

cor(d$data.movie,d$data1.covid,use="pairwise.complete.obs")

cor.test(d$data.movie,d$data1.covid,use="pairwise.complete.obs")

data <- read.csv("C:/sm/mid_project/data/d.csv")
View(data)
data <- data[2:3]

cor(data$covid,data$movie)
cor.test(data$covid,data$movie)

plot(movie~covid, data=data)

fit <- lm(movie~covid, data=data)
fit

abline(fit, col="red")

summary(fit)  






##### 정규화 #####
mov1 <- read.csv("C:/sm/mid_project/data/상관분석을위한movie정규화.csv")
cov1 <- read.csv("C:/sm/mid_project/data/상관분석을위한covid정규화.csv")


mydata <- melt(mov1, id.var="X", value.name="movie")

mydata1 <- melt(cov1, id.var="X", value.name="covid")
View(mydata)
# 코로나는 3월 1일 ~ 12월 30일까지, 영화는 3월 2일 ~ 12월 31일
mydata <- mydata[-1,]
mydata1 <- mydata1[-682,]

mydata <- mydata[3]
mydata1 <- mydata1[3]

is.na(mydata)

mydata <- na.omit(mydata)
mydata1 <- na.omit(mydata1)


View(mydata1)
# 데이터 합치기

d1 <- data.frame(mydata1$covid,mydata$movie)

View(d1)
is.na(d)
str(d)


# 상관계수
cor(d1$mydata1.covid,d1$mydata.movie)



# 상관분석
cor.test(d1$mydata1.covid,d1$mydata.movie, mothod="pearson")

plot(data.movie~data1.covid, data=d)

fit <- lm(data.movie~data1.covid, data=d)
fit

abline(fit, col="red")

summary(fit)  









##### 20년도만 #####
mov20 <- read.csv("C:/sm/mid_project/data/20년영화상관분석할데이터.csv")
cov20 <- read.csv("C:/sm/mid_project/data/20년코로나상관분석할데이터.csv")

data1 <- melt(cov20, id.var="X", value.name="covid")
data2 <- melt(mov20, id.var="X", value.name="movie")

View(data1)
View(data2)
# 코로나는 3월 1일 ~ 12월 30일까지, 영화는 3월 2일 ~ 12월 31일
data1 <- data1[-310,]
data2 <- data2[-1,]

data1 <- data1[3]
data2 <- data2[3]

is.na(data)

data1 <- na.omit(data1)
data2 <- na.omit(data2)

# 데이터 합치기

data <- data.frame(data1$covid,data2$movie)

View(data)

# 상관계수
cor(data$data1.covid,data$data2.movie)


# 상관분석
cor.test(data$data1.covid,data$data2.movie, mothod="pearson")

plot(data$data2.movie~data$data1.covid, data=data)

fit <- lm(data$data2.movie~data$data1.covid, data=data)
fit

abline(fit, col="red")

summary(fit)  


##### 21년도만 #####
mov21 <- read.csv("C:/sm/mid_project/data/21년영화상관분석할데이터.csv")
cov21 <- read.csv("C:/sm/mid_project/data/21년코로나상관분석할데이터.csv")

data1 <- melt(cov21, id.var="X", value.name="covid")
data2 <- melt(mov21, id.var="X", value.name="movie")

View(data1)
View(data2)
# 코로나는 3월 1일 ~ 12월 30일까지, 영화는 3월 2일 ~ 12월 31일
data1 <- data1[-372,]
data2 <- data2[-1,]

data1 <- data1[3]
data2 <- data2[3]

is.na(data)

data1 <- na.omit(data1)
data2 <- na.omit(data2)

# 데이터 합치기

data <- data.frame(data1$covid,data2$movie)

View(data)

# 상관계수
cor(data$data1.covid,data$data2.movie)


# 상관분석
cor.test(data$data1.covid,data$data2.movie, mothod="pearson")

plot(data$data2.movie~data$data1.covid, data=data)

fit <- lm(data$data2.movie~data$data1.covid, data=data)
fit

abline(fit, col="red")

summary(fit)  


# 20년도 보다 21년도에 사람들이 규제를 잘 안따랐다 - 지들 멋대로;;
# 21.4.1부터 확진자 동선 안내 재난 문자 중지
# 21.2.26 정도부터 백신 접종 시작
# 워드클라우드로 20년도와 21년도의 코로나에 대한 사람들의 인식? 반응 보여주기!! 되면 좋겠다
#     20년도는 규제좀 지키자 이런거, 21년도는 이제 못참겠다;;;;