rm(list=ls())

#install.packages('reshape2')
#install.packages("psych")

library(reshape2)
library(dplyr)
library(psych)

#지난시간 복습____________________________________
iris

#아이리스데이터 1행에서 5행 가져오기
head(iris,5)
iris[1:5,]
iris[c(1:5),]

#아이리스데이터의 1열과 5열의 위에서 6개 행
head(iris[,c(1,5)])

#아이리스데이터에서 species만 빼고 위에서 6개
ls(iris)
head(iris[,-5])
iris %>% select(-Species)  %>%  head()

#Sepal.Length의 평균 / 중위값
iris$Sepal.Length %>% mean()
iris$Sepal.Length %>% median()

summary(iris$Sepal.Length)

#아이리스 데이터의 열방향 최대값
apply(iris,2,max)

#열방향의 합계
apply(iris[,1:4],2,sum)

#Petal.Length 4.35초과이고, width가 1.3초과인 데이터를 df.large로 저장
df.large <- iris %>% filter(Petal.Length > 4.35 & Petal.Width > 1.3)
df.large
table(df.large[,5])

#그래프그리기
plot(iris) #전체 나오기
plot(df.large)
pairs.panels(iris)

plot(iris$Petal.Width, iris$Petal.Length, pch=5)

#가메교재 150p___________________________
summary(iris$Sepal.Length)
hist(iris$Sepal.Length,xlab="iris$Sepal.Length",col='grey',main="iris 꽃받침 길이 Histogram", xlim=c(4.3,7.9)) 

summary(iris$Sepal.Width)
hist(iris$Sepal.Width,xlab="iris$Sepal.Width", col='mistyrose',main="iris 꽃받침 너비 histogram",xlim=c(2,4.5))

par(mfrow=c(1,2))
hist(iris$Sepal.Width,xlab='iris$Sepal.Width',col='green',main='iris 꽃받침 너비 histogram : 빈도수',xlim=c(2,4.5))
hist(iris$Sepal.Width,xlab="iris$Sepal.Width",col='mistyrose',freq = F, main="iris 꽃받침 너비 histrogram : 확률밀도", xlim=c(2,4.5))
lines(density(iris$Sepal.Width,col='red'))

#멜트함수 교재 166p____________________
head(airquality)

#소문자로 변환한 변수명으로 기본 변수명 대체
names(airquality) <- tolower(names(airquality))
airquality %>% head()

#melt함수 실행
melt_test <- melt(airquality)
head(melt_test)
tail(melt_test)


#월과 바람에 따른 오존 확인
melt_test2 <- melt(airquality,id.vars=c("month","wind"),measure.vars = "ozone")
melt_test2 %>% head()


#airq데이터 중 NA가 하나라도 있으면 제외하고 NA.RM_AIR로 저장
table(is.na(airquality))
NA.RM_AIR <- na.omit(airquality)

#월일을 기준으로 한 solar.r값을 확인
aq_melt <- melt(airquality,id.vars=c("month","day"),measure.vars = "solar.r")
 
dim(airquality)
dim(melt_test2)

#캐스트/ 교재 171p________________
aq_dcast <- dcast(aq_melt , month + day ~ variable)
head(aq_dcast)

#오존,태양,바람,온도의 측정값이 출력
acast(aqmelt, day ~ month ~ variable) #기준 2개 / array형식 출력

#평균으로 요약하기
acast(aqmelt, month ~ variable, mean) #기준 1개

#통계_____________________________________
'''
범위 (Range) = 최댓값 - 최솟값
사분위수 편차 = (Q3 - Q1)
분산 : 산포도의 척도로 가장 널리 사용
  -분산 계산 시 : 데이터가 모집단 전체일 경우, 데이터의 수 n개로 나누고, 표본일 경우 n-1 로 나눈다.
                  단, 데이터의 표본의 크기가 큰 경우는 큰 차이가 없다. 
왜도 : 분포의 모양이 대푯값을 중심으로 좌우의 모양이 대칭인가를 측정.
  데이터의 치우친 정도(한 방향)
  왜도가 0에 가까운 값 = 좌우대칭
  왜도가 0보다 작은 음수 = 오른쪽에 치우쳐져서 왼쪽 꼬리가 길게 나옴.
  왜도가 0보다 큰 양수, 왼쪽으로 치우쳐져서 오른쪽 꼬리가 길다. 
첨도 : 대표값을 중심으로 얼마나 모였는가. 
  첨도 > 3 : 가운데 뾰족한 모양
  첨도 = 3
  첨도 < 3 : 좌우로 퍼져 완만한 모양. 정규분포랑 같다. 

편차 :  각 데이터 값 - 평균
분산 : 각 변량들이 퍼져 있는 정도. 분산이 크면 불안정
표준편차 : 분산은 수치가 너무 커서 제곱근을 이용하여 줄인 값
최빈값 : 최빈수라고도 하며 데이터에서 가장 자주 나타나는 숫자


'''

#고등학교 2학년 남학생의 키 분포도를 통해서 기초 통계량을 구하시오.
height <- c(164,166,168,170,172,174,176)
sd(height)

#정규성 여부 판단 - shapiro.test() : 0.05보다 p값이 더 커야지만 정규성을 만족한다고 판단한다.
hist(rnorm(10))
shapiro.test(rnorm(100,5,3))

??ToothGrowth
str(ToothGrowth)


'''
shapiro 검정
: 정규성을 띄는지 여부. 0.05보다 크면 귀무가설을 채택함.
귀무가설 - 정규성을 따른다. p값 > 0.05
대립가설 - 정규성을 따르지 않는다. p값 <= 0.05
'''

data1 <- sample(50:100, 100, replace = T)
data1 %>% hist(col="orange",las=1)
shapiro.test(data1)

data2 <- rnorm(100,80,2)
data2 %>% hist(col='skyblue',las=1)
shapiro.test(data2)

#가메184p____________________
data <- read.csv("c:/ken/data/data.csv")
data
wide <- dcast(data, Customer_ID ~ Date,sum)
wide

setwd("c:/ken/data")
write.csv(wide,"wide.csv",row.names = FALSE)

wide <- read.csv("wide.csv")
colnames(wide) <- c("Customer_ID","day1","day2",'day3','day4','day5','day6','day7')
wide

long <- melt(wide, id="Customer_ID")
name <- c("Customer_ID", "Date", "Buy")
colnames(long) <- name
head(long)

long <- melt(id = 1:2,smiths)
long

dcast(long,subject + time~...)

names(airquality) <- toupper(names(airquality))
head(airquality)

air_melt <- melt(airquality,id = c("MONTH","DAY"),na.rm=TRUE)
air_melt

acast <- acast(air_melt, DAY ~ MONTH~ variable)
acast
class(acast)

acast(air_melt, MONTH ~ variable, sum, margins = TRUE)

smiths
