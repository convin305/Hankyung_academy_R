1 + 1
6 %% 4
10 %/% 7

2^4
sqrt(2)

x <- 10
y <- 3
x + y
x % y
x-y

x1 <- c(10,9,7) 
'''
벡터 - 문자, 숫자 모두 입력 가능하다.
하나라도 문자가 섞이게 되면 전체가 다 문자화가 된다.(참고)

'''
y1 <- c("사과라떼","고구마라떼","딸기라떼")

x2 <- 1:10 ; x2 #연속된데이터

y2 <- 10:20

var1 <- c(1,2,3,4,5)
var1

var2 <- c(1:5)
var2

var1 + var2

var1 + x1 #연산은 되지만 경고메세지 출력. 


var3 <- seq(1,5) ; var3
var4 <- seq(1,10,2) ;var4


v1 <- rep("Q",6)
v1

v2 <- rep(c("A","B"),2) ; v2

v3 <- rep(c("A","B"),each=2)
v3

print("Hello World")

sample(1:10,5) #1에서 10까지 5개 출력
sample(1:7,5,replace=TRUE,prob=1:7) #1에서 7까지 5개 출력, 중복 허용, prob:끝자리로 갈수록 비율이 높게 만드는 것.
sample(1:10,3,replace=TRUE,prob=1:10)

#지울 때
rm(x)

#여태 했던 변수들을 다 지울 때
rm(list=ls())

#기술통계
x <- c(1,2,3)
mean(x)
max(x)
min(x)

#기술통계를 통틀어서 볼 수 있는 함수
summary(x)

#콘솔 창 지우기 : ctrl + L

x <- c(20:30)
# [] 인덱스

#마이너스의 경우 해당 숫자만 제외한다. 
x[-5]
x[-1]

#3번째에서 5번째를 제외
x[-3:-5]

rm(list=ls())
