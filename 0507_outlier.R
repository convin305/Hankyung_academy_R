library(dplyr)


data <- read.csv('c:/ken/data/three_sample.csv',header=T)
data %>% head()

data %>%  summary()

#데이터 정제, 전처리 
data <- subset(data , !is.na(score),c(method,score))
data

#차트를 이용해서 outlier확인
plot(data$score)
barplot(data$score)
mean(data$score) #평균이 14이므로 14이상은 아웃라이어로 간주하고 제거
boxplot(data$score)

#아웃라이어 제거
length(data$score)
data2 <- subset(data,score <= 14)
data2$score %>% length()

#정제된 데이터 보기
boxplot(data2$score)

#_____________________________________________________
val_1 <- c(2.5,3.2,5.7,4.6,5.8,60)
year_1 <- c(2016:2021)

fit_2 <- lm(val_1 ~ year_1)

plot(year_1,val_1)
abline(fit_2,col="blue")
summary(fit_2)
#__________________________________________________
히스토그램의 시작점과 끝점에 따라 그래프 모양이 달라지는 단점을 보왆기 위한 대안으로 
<<밀도함수>>를 이용해보자. 
#density()

density()
plot()
str(iris)

hist(iris$Sepal.Width)
ds_iris <- density(iris$Petal.Width)
plot(ds_iris) #기본 형태의 밀도 곡선

#내부 색상을 위해서는 먼저 기존 데이터를 가져오기
iris
ds_iris <- density(iris$Petal.Width)
plot(ds_iris,main="확률 밀도") #기본 형태의 밀도 곡선 완성
polygon(ds_iris,col='red',border = "blue") #내부와 외부의 경계선 만들기
rug(iris$Sepal.Width,col="brown")

#______________________
x <- iris$Sepal.Length
par(mfrow=c(1,2))
qqnorm(x)
qqline(x,col='red',lwd=2)

hist(x,breaks = 15,probability = T,)