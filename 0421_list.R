#리스트
a <- c(1,2,3)
b <- c(1,2,3,4)

#c <- data.frame(a,b) ; c
c <- list(a,b) ; c

#리스트 보강
ken_list <- list(
  a = 1:3,
  b = "a string",
  c = pi,
  d = list(-1,-5))

#행 뽑기
ken_list[[1]]
ken_list[[2]]
ken_list[[3]]
ken_list[[4]]

#특정 위치의 데이터 뽑기
ken_list[[1]][2]
ken_list[[4]][2]

##### matrix 추가
mt <- matrix(1:6,2,3)
mt + 3

#행의 합 / 열의 합 / 행의 평균 / 열의 평균
rowSums(mt) ; colSums(mt) ; rowMeans(mt) ; colMeans(mt)

#기본값 : 열기준 cbind / apply : 2

#transpose
t(mt)

y <- c(1:6)
array(y,dim=c(2,2,3))

#검색하기
??iris
??mtcars


.libPaths()


#내장데이터셋 : iris, mtcars
head(iris,3)

#비교연산자
5>3
5>=6
5<3
5<=6
5==8
5!=5


#데이터 불러오기
attach(iris)
#불러온 데이터 제거
detach(iris)

myiris <- iris #myiris에 iris 저장하기
head(myiris,10) #head 기본값 6개
tail(myiris,3) #밑에서 3개

dim(iris)
nrow(iris) 
ls(iris) #iris의 변수명출력

my_iris<- myiris[,1:4] ; my_iris
apply(my_iris,2,mean) #각 컬럼별 평균
apply(my_iris,2,sum) #각 컬럼별 총계
lapply(my_iris,mean) #무조건 열기준

install.packages("readxl")

library(readxl)
###################################
#엑셀 데이터 불러오기
exdata1 <- read_excel('data/Sample1.xlsx')
exdata1

View(exdata1) #view창으로 데이터 셋 확인하기

str(exdata1) #데이터 구조 확인

dim(exdata1) #데이터 차원 확인

ls(exdata1) #데이터 변수명 확인

install.packages("dplyr")
library(dplyr) #처리작업하는 기능이 많은 패키지

exdata1 <- rename(exdata1, Y17_AMT = AMT17, Y16_AMT = AMT16)
head(exdata1)

#파생변수
exdata1$AMT <- exdata1$Y17_AMT + exdata1$Y16_AMT
exdata1$CNT <- exdata1$Y17_CNT + exdata1$Y16_CNT
exdata1$AVG_AMT <- exdata1$AMT / exdata1$CNT

str(exdata1)
#__________________________________________________________________________

install.packages("ggplot2")
