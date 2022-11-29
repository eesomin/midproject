region <-read.csv("C:/BIGDATA analysis/project/nsm1/data/region_test.csv")


region
View(region)

str(region)

region$좌석수 <- gsub(",","", region$좌석수)
region$좌석수<-as.integer(region$좌석수)

str(region)


# 정규성
shapiro.test(region$상영관수)
shapiro.test(region$스크린수)
shapiro.test(region$좌석수)

shapiro.test(region$이용경험있음)
shapiro.test(region$이용경험없음)


plot(region$이용경험있음~region$스크린수, region)
plot(region$이용경험있음~region$상영관수, region)


cor.test(region$상영관수, region$이용경험있음, method = "pearson")
cor.test(region$상영관수, region$이용경험있음, method = "spearman")

cor.test(region$상영관수, region$이용경험없음, method = "pearson")
cor.test(region$상영관수, region$이용경험없음, method = "spearman")

cor.test(region$스크린수, region$이용경험있음, method = "pearson")
cor.test(region$스크린수, region$이용경험있음, method = "spearman")

cor.test(region$스크린수, region$이용경험없음, method = "pearson")
cor.test(region$스크린수, region$이용경험없음, method = "spearman")

cor.test(region$좌석수, region$이용경험있음, method = "pearson")
cor.test(region$좌석수, region$이용경험있음, method = "spearman")

cor.test(region$좌석수, region$이용경험없음, method = "pearson")
cor.test(region$좌석수, region$이용경험없음, method = "spearman")

# 상영관수 좌석수 스크린수에 따른 ott 이용경험 있음 또는 없음에 상관관계가 없다.
