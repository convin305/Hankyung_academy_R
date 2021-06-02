########################################
## Chapter12. 교차분석과 카이제곱검정  p400
########################################


# 1) 일원카이제곱 

# (1) 적합성 검정 예
# 카이제곱 검정 ㅣ chisq.test()를 이용하여 관찰빈도와 기대빈도의 일치 여부를 검정
# HO : 기대치와 관찰치는 차이가 없다
# H1 : 기대치와 관찰치는 차이가 있다.

# 60회 주사위를 던져서 나온 관측도수/기대도수
# 관측도수 : 4, 6, 17,16 ,8,9
# 기대도수 : 10,10,10,10,10,10

chisq.test(c(4,6,17,16,8,9))

# (2) 선호도 분석 
#-----------------------------------------
# 귀무가설 : 기대치와 관찰치는 차이가 없다. 
#       예) 스포츠음료의 선호도에 차이가 없다.
# 대립가설 : 기대치와 관찰치는 차이가 있다. 
#       예) 스포츠음료의 선호도에 차이가 있다.



# 2) 이원카이제곱 - 교차분할표 이용

# (1) 독립성

# 이원 카이제곱 검정
# 독립성 검정(관련성 검정)
# HO :경제력과 대학 진학 합격과 독립성이 있다(=관련이 없다)
# H1 :경제력과 대학 진학 합격과 독립성이 없다(=관련이 있다)
# 독립변수(x)=설명변수, 종속변수(y)=반응변수 생성 
library(gmodels)

data <- read.csv("c:/ken/data/cleanDescriptive.csv", header=TRUE)
x <- data$level2 # 부모의 학력수준
y <- data$pass2 # 자녀의 대학진학여부 

a1 <-data.frame(x,y)
head(a1)
CrossTable(x, y, chisq = TRUE) #p =  0.2507057    


# (2) 동질성 검정  p 402
# 귀무가설 : 교육방법에 따라 만족도에 차이가 없다.
# 대립가설 : 교육방법에 따라 만족도에 차이가 있다.


# 1. 파일 가져오기
#setwd("f:/ken/data3")
data <- read.csv("homogenity.csv", header=TRUE)
head(data) 
# method와 survery 변수만 서브셋 생성
data <- subset(data, !is.na(survey), c(method, survey)) 
data

# 2. 변수리코딩 - 코딩 변경

# 교육방법2 필드 추가
data$method2[data$method==1] <- "방법1" 
data$method2[data$method==2] <- "방법2"
data$method2[data$method==3] <- "방법3"

# 만족도2 필드 추가
data$survey2[data$survey==1] <- "1.매우만족"
data$survey2[data$survey==2] <- "2.만족"
data$survey2[data$survey==3] <- "3.보통"
data$survey2[data$survey==4] <- "4.불만족"
data$survey2[data$survey==5] <- "5.매우불만족"

# 3. 교차집계 
table(data$method2, data$survey2)  # 교차표 생성 -> table(행,열)

# 4. 동질성 검정 - 모수 특성치에 대한 추론검정  
chisq.test(data$method2, data$survey2) 


# 5. 동질성 검정 해석
# 교육방법에 따른 집단 간의 만족도에 차이가 없다.

######################################
#########################################################p417 
## 2.2 단일집단 평균검정(단일표본 T검정) 

# (1) 단일표본 평균 계산 

# [실습] 단일표본 평균 계산하기
귀무가설 : 국내에서 생산된 노트북과 A사 생산된 노트북의 평균 사용시간에 차이가 없다.
대립가설 :  "                             "                             차이가 있다.
# 단계 1. 실습파일 가져오기
#setwd("")
data <- read.csv("c:/ken/data/one_sample.csv", header=TRUE)
str(data) # 150
head(data)
x <- data$time
head(x)

# 단계 2. 데이터 분포/결측치 제거
summary(x) # NA-41개
# 단계 3. 데이터 정제 
mean(x, na.rm=T) # NA 제외 평균(방법1)
x1 <- na.omit(x) # NA 제외 평균(방법2)
mean(x1)

# (3) 정규분포 검정
shapiro.test(x1) #0.05보다 크므로 정규분포이다.

# (4) 정규분포 시각화 
#####
par(mfrow= c(1,2))
hist(x1)  # 히스트

qqnorm(x1)
qqline(x1, lty=1, col= 'blue')

######
# (5) 평균차이 검정 


# 단계 1. 양측 검정 : x1 객체와 기존 모집단의 평균 5.2시간 비교

모집단과 표본은 평균이 차이가 없다/ 있다
summary(x1)
t.test(x1, mu=5.2) 
t.test(x1, mu=5.2, alter="two.side", conf.level=0.95)  # 
#### 차이가 있다.. 그럼.. 더 큰가?

# 단계 2. 방향성을 갖는 단측가설 검정
t.test(x1, mu=5.2, alter="greater", conf.level=0.95) 
####  더 크다.. 즉.. time 변수이니  시간이 더 길다..


## 3. 두 집단 검정

## 3.2 두 집단 평균검정(독립표본 T검정) 

# (1) 독립표본 평균 계산 


1. 실습파일 가져오기
data <- read.csv("two_sample.csv", header=TRUE)
data 
head(data) #4개 변수 확인
summary(data) # score - NA's : 73개

# (3) 두 집단 평균 차이 검정 

# 단계 1.           "" 양측검정 ""
a1
# 귀무가설 :  교육방법에 따른 두 집단 간 실기시험의 평균에 차이가 있다.
t.test(a1, b1)
t.test(a1, b1, alter="two.sided",  conf.level=0.95)

# 2단계로..
# 단계 2. 방향성을 갖는     ""  단측가설 검정  P421
# 사용량이 더 큰가? (단측)
t.test(a1, b1, alter="greater", conf.level=0.95)
# 사용량이 더 작은가? (단측)
t.test(a1, b1, alter="less",  conf.level=0.95)


## 3.3 대응 두 집단 평균검정(대응표본 T검정) 

# (1) 대응표본 평균 계산 

# 단계 1. 실습파일 가져오기
data <- read.csv("paired_sample.csv", header=TRUE)
head(data) #  no before after

# 단계 2. 대응 두 집단 subset 생성
result <- subset(data, !is.na(after), c(before,after)) # 100 - 96
x <- result$before
y <- result$after

# 단계 3. 기술통계량 
length(x) # 96
length(y) # 96
mean(x) # 5.145
mean(y, na.rm = T) # 6.220833 -> 1.052  정도 증가


# (2) 동질성 검정  
var.test(x, y) 


# (3) 대응 두 집단 평균 차이 검정 

# 단계 1. 양측검정
t.test(x, y, paired=TRUE)  
# 차이가 있다

# 단계 2. 방향성을 갖는 단측가설 검정
t.test(x, y, paired=TRUE,alter="less") 
# x을 기준으로 비교 : x가 y보다 적다.



## 4.2 분산분석(F 검정) 


# (1) 데이터 전처리 

# 단계 1. 파일 가져오기
data <- read.csv("three_sample.csv", header=TRUE)
summary(data$method)
ls(data)

# 단계 2. 데이터 정제/전처리 - NA, outline 제거
data <- subset(data, !is.na(score), c(method, score)) 
data # method, score

# 단계 3. 차트이용 - ontlier 보기(데이터 분포 현황 분석)
plot(data$score) # 차트로 outlier 확인 : 50이상과 음수값
barplot(data$score) # 바 차트
mean(data$score) # 14.45
boxplot(data$score)
# 단계 4. outlier 제거 - 평균(14) 이상 제거
length(data$score)
data2 <- subset(data, score <= 14) # 14이하만 data2

# 단계 5. 정제된 데이터 보기 

boxplot(data2$score)
length(data2$score) 


# (2) 세 집단 subset 작성과 기술 통계량 

# 단계 1. 세 집단 subset 작성
data2$method2[data2$method==1] <- "방법1" 
data2$method2[data2$method==2] <- "방법2"
data2$method2[data2$method==3] <- "방법3"

# 단계 2. 교육방법 별 빈도수
table(data2$method2)  

# 단계 3. 교육방법을 x 변수에 저장
x <- table(data2$method2) 
x
# 단계 4. 교육방법에 따른 시험성적 평균 구하기
y <- tapply(data2$score, data2$method2, mean)

tapply(data2$score, data2$method2, shapiro.test)
# 단계 5. 교육방법과 시험성적으로 데이터프레임 생성
df <- data.frame(교육방법 = x, 성적 = y)
df

# (3) 세 집단 간 동질성 검정 p443
bartlett.test(score ~ method2, data=data2)
head(data2)

# (4) 분산분석(세 집단 간 평균 차이 검정) 
head(data2)
result <- aov(score ~ method2, data=data2)
result
names(result)
summary(result) 
result$residuals # 각 잔차
result$coefficient 


plot(result)  # 잔차들의 분산이 일정 p491

abline(result, col=red)
###################################





#################################################
##################################
## 2.3 다중 회귀분석  483

# - 여러 개의 독립변수 -> 종속변수에 미치는 영향 분석
# 연구가설 : 음료수 제품의 적절성(x1)과 친밀도(x2)는 제품 만족도(y)에 정의 영향을 미친다.
# 연구모델 : 제품 적절성(x1), 제품 친밀도(x2) -> 제품 만족도(y)
rm(list=ls())
# csv파일은 시트를 선택적으로 불러올수가 없다
rm(list=ls())
library(readxl)

product <- read.csv("c:/ken/data/product.csv")

#(1) 적절성 + 친밀도 -> 만족도  

y<- product[,c(3)]
x1<- product[,c(1)]
x2<- product[,c(2)]

######################################신뢰성 추가
install.packages("psy")
library("psy")

df <- data.frame(x1, x2, y)
class(df)

cronbach(df)
#$alpha
#[1] 0.7960089
#0.6이상
###############################################
cor(df) # 상관관계

# 회귀식 선형모델
result.lm <- lm(y ~ x1 + x2 , data=df)

# 계수 확인 
result.lm

(Intercept)     x1          x2  
0.66731      0.09593      0.68522 
절편          기울기     기울기

summary(result.lm)




# 0.66731(y절편)      0.09593(x1)  0.68522(x2)      

#residuals  leverage
# 분산팽창요인 
install.packages("car") # vif() 함수 제공 패키지 설치
library(car) # 메모리 로딩

#단계 2 : 분산팽창요인(VIF)
vif(result.lm) 
sqrt(vif(result.lm) ) > 2 # FALSE FALSE  2이하 다중공선성 아님


## 2.4 다중공선성 문제 해결과 모델 성능평가


# [실습] 다중공선성 문제 확인

# (1) 패키지 설치 및 데이터 로딩 
install.packages("car")
library(car)
data(iris)

# (2) iris 데이터 셋으로 다중회귀분석
fit <- lm(formula=Sepal.Length ~ Sepal.Width+Petal.Length+Petal.Width, data=iris)
vif(fit)

sqrt(vif(fit))>2 # root(VIF)가 2 이상인 것은 다중공선성 문제 의심 


##  2.5 기본가정 충족으로 회귀분석 수행

# [실습] 기본가정 충족으로 회귀분석 수행 

# 1. 회귀모델 생성 
# (1) 변수 모델링 : y:Sepal.Length <- x:Sepal.Width,Petal.Length,Petal.Width
formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width

# (2) 회귀모델 생성 
model <- lm(formula = formula,  data=iris)
model


# 2. 잔차[오차] 분석

# (1) 독립성과 등분산성 검정  
install.packages('lmtest')
library(lmtest) # 자기상관 진단 패키지 설치 
dwtest(model) # 더빈 왓슨 값(통상 1~3 사이)

# 등분산성 검정 
plot(model, which =  1) 
methods('plot') # plot()에서 제공되는 객체 보기 

# (2) 잔차 정규성 검정
attributes(model) # coefficients(계수), residuals(잔차), fitted.values(적합값)
res <- residuals(model) # 잔차 추출 
shapiro.test(res) # 정규성 검정 - p-value = 0.9349 >= 0.05
# 귀무가설 : 정규성과 차이가 없다.

# 정규성 시각화  
hist(res, freq = F) 
qnorm(res)

# 3. 다중공선성 검사 
library(car)
vif(model)
sqrt(vif(model)) > 2 # 
###################################
########

# 4. 회귀모델 생성/평가 
formula = Sepal.Length ~ Sepal.Width + Petal.Length #y~ x1+x2
model <- lm(formula = formula,  data=iris)
plot(model) #theoretical quantiles normal q-q

summary(model) # 모델 평가



#___________________________________
#처음배우는R p224

library(ggplot2)
ggplot(economics,aes(x=date,y=psavert)) + geom_line() + geom_abline(intercept = 12.18671,slope = -0.0005444)

#회귀식 모형 적합도 확인
a.lm <- lm(data=economics, psavert~date)
summary(a.lm)
a.lm$coef
plot(a.lm)
