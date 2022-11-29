

data <-read.csv("C:/BIGDATA analysis/project/nsm1/data/rt_screen_cnt_ott_ex.csv")

data

# 정규성
shapiro.test(data$스크린증감률)
shapiro.test(data$이용경험있음_증감률)
shapiro.test(data$이용경험없음_증감률) # p-value = 0.4056

# h0 : (귀무)상관계수가 0이다 - 상관관계 없다....ㅠㅠㅠㅠㅠ
# h1 : (대립)상관계수가 0이 아니다 - 유의하다... 

plot(data$이용경험있음_증감률~data$스크린증감률, data)

cor.test(data$스크린증감률, data$이용경험있음_증감률,method = "pearson")
cor.test(data$스크린증감률, data$이용경험있음_증감률, method = "spearman")
######################################################################
cor.test(data$스크린증감률, data$이용경험있음_증감률, method = "kendall")


model<-lm(data$이용경험있음_증감률~data$스크린증감률, data)
plot(model)
summary(model)

# h0 : 상관계수가 0이다
# h1 : 상관계수가 0이 아니다

plot(data$이용경험없음_증감률~data$스크린증감률,data)

cor.test(data$스크린증감률, data$이용경험없음_증감률, method = "pearson")
cor.test(data$스크린증감률, data$이용경험없음_증감률, method = "spearman")
####################################################################
cor.test(data$스크린증감률, data$이용경험없음_증감률, method = "kendall")


model2<-lm(data$이용경험없음_증감률~data$스크린증감률, data)
plot(model2)
summary(model2)


# 스크린수 증감량에 따른 ott 이용경험 있음 또는 없음의 증감량과는 상관관계가 없다.






