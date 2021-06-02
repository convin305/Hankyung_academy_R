rm(list=ls())
라이브러리
library(dplyr)



#가메 194p____________________________________________________
getwd()

dataset <- read.csv("c:/ken/data/dataset.csv")
dataset %>% head

View(dataset)

#변수명 조회
names(dataset) 

#names(열이름),class, row.names(행이름)
attributes(dataset)

str(dataset)

#데이터 셋에서 특정 변수 조회하기
dataset$age
dataset$resident
length(dataset$age) #데이터 수 확인

#특정 변수의 조회 결과를 변수에 저장하기
x <- dataset$gender
y <- dataset$price

#산점도 그래프로 변수 조회
plot(dataset$price)

#칼럼명을 사용하여 특정 변수 조회
dataset['gender']
dataset['price']

#색인을 사용하여 특정 변수 조회
dataset[2]
dataset[3,]

#2개 이상의 칼럼 조회
dataset[c('job','price')]
dataset[c(2,6)]
dataset[c(2,4:6,3,1)]

#특정 행/열을 조회
dataset[,c(2:4)]
dataset[c(2:4),]
dataset[-c(1:100),]

#결측치 확인
summary(dataset$price)

#결측치 제거
#na.rm = T를 적용하여 결측치 제거
sum(dataset$price,na.rm = T)
#제거 함수를 이용하여 결측치 제거
price2 <- na.omit(dataset$price)
sum(price2)
length(price2)

#결측치를 0으로 대체하기
x <- dataset$price
x[1:30]

dataset$price2 = ifelse(!is.na(x),x,0) #NA면 0으로 대체체
dataset$price2[1:30]

#결측치를 평균으로 대체
x <- dataset$price
dataset$price3 <- ifelse(!is.na(x),x,round(mean(x,na.rm=T),2))

#3개 칼럼 확인
dataset[c('price','price2','price3')] %>% head()

#범주형 변수의 극단치 처리하기
table(dataset$gender)
pie(table(dataset$gender))

#gender변수 정제
dataset <- subset(dataset,gender==1|gender==2)
dataset

length(dataset$gender)
pie(table(dataset$gender)) #정제 결과 확인

#연속형 변수의 극단치 처리
dataset <- read.csv("c:/ken/data/dataset.csv")
length(dataset$price)
plot(dataset$price)

#price 변수의 데이터 정제와 시각화 (2에서 8사이)
dataset2 <- subset(dataset, price >= 2 & price <= 8)
length(dataset2$price)
stem(dataset2$price) #줄기와 잎 그림보기

#age변수의 데이터 정제와 시각화
summary(dataset$age)
length(dataset2$age)
dataset2 <- subset(dataset2, age >= 20 & age <= 69)
length(dataset2)
boxplot(dataset2$age)

#회귀분석______________________
pro_df <- read.csv("c:/ken/data/promote.csv")
pro_df
ls(pro_df)
cor(pro_df) #상관계수분석
#1.
pro_lm = lm(data = pro_df, price ~ promote )
#2.
summary(pro_lm)  
#3.
par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(pro_lm)

#정규성 분석
shapiro.test(pro_df$promote)
# : p-value가 0.05이하이므로 데이터는 정규분포를 따르지 않는다. 
hist(pro_df$promote)
#________________________
shapiro.test(pro_df$price)
# : p-value가 0.05이상이므로 데이터는 정규성을 가진다. 
hist(pro_df$price)

#cars데이터 셋을 이용해서 lm함수를 대입해서 속도가 얼마나 내야지 거리가 얼마만큼 변하는가?__________________
data(cars)
str(cars)
head(cars)

change <- lm(data=cars, dist ~ speed)
summary(change)
change
