cas_iris <- tolower( names(iris) )
cas_iris
#변수명을 변경하는 함수 rename, dplyr에 있음

table(iris$Species)
#출력함수 print / cat______________________________________
'''
print(출력할 데이터,..)
cat() : 줄바꿈 기능이 없음
'''
print(100)
print(pi)
data <- c("사과","딸기","포도")
print(data)
print(data,quote=FALSE) #""del
print(data,
      print.gap = 3, quote=FALSE) #gap, 데이터 사이의 공간 생성

cat(100)
cat(100,200)
1+1
###
cat(100,200,"\n")
1+1

#print("데이터는 : " data)

print("\n")

print("A"=="a")

print(100,200)
1+1

#if문__________________________________________________________
x <- 20ㅎ
if (x > 10){
  print("x is large number")
}

if (x > 10){
  cat(x," is large number")
}


#반복문________________________________________________________
'''
for (i in data) {i를 사용할 문장} 
: data에 들어있는 각각의 값을 변수 i에 할당하면서 각가겡 있는 블록 안의 문장 수행

while (con) {조건이 참일떄 수행할문장} 
: 조건 con이 참일떄 블록 안의 문장을 수행

repear {반복해서 수행할 문장} 
:  블럭 안의 문장을 반복해서 수행

break 종류 / next 다음 실행

paste0() : 공백제거

swtich() : 선택가능

'''

for (i in 1:10) {
  cat(i,"번째 실행중입니다","\n")       #\t : tab , \n : enter  
}

paste0(i,"번째 실행중입니다.")

for (i in 1:10) {
  cat(paste0(i,"번째 실행중입니다."),"\n")         
}

vec1 <- c(1:100)
for (i in vec1) {
  cat(paste0(i,"번째 실행중입니다."),"\n")         
}

gg <- 3
for (i in 1:10){
  print(paste(gg, "x",i,"=",gg * i))
}

sum <- 0
for (i in 1:10) {
  sum <- sum+i
}
sum

a <- 7
# %% : 나머지로, 짝수와 홀수를 구할 때 사용
if (a %% 2 == 0) {
  print(a)
  print("짝수")
} else {
  print(a)
  print("홀수")
}

data <- c(1:10)
switch(data[3],
       "1" = print("One"),
       "2" = print("Two"),
       "3" = print("Three"),
       "4" = print("Four"),
       print("NULL"))

user <- function(){
  answer <- readline("Input Data : ")
  
    if (substr(answer,1,1)=="n") cat("아니오") else cat("예")
}

user()

library(ggplot2) 
library(dplyr)
distinct(iris,Species)


#조건에 맞는 데이터만 추출하기__________________________________________________________

library(dplyr)

exam <- read.csv('C:/ken/data/csv_exam.csv')
head(exam)

#클래스가 1반인 사람들
exam %>% filter(class==1)

#1반이 아닌 경우 
exam %>% filter(class != 1)

#수학점수가 50점이 넘는 학생
exam %>% filter(exam$math > 50)

#수학 점수가 50점 초과이고 영어 점수가 90점 이상
exam %>%  filter(exam$math > 50 & exam$english >= 90)

#1반이면서 수학 점수가 50점 이상
exam %>% filter(class == 1 & math >= 50)

#id와 영어점수, 과학점수 열만 가지고 아래 데이터를 출력하시오.
exam %>% 
  filter(english < 90 | science < 50) %>% 
  subset(select= c("id",'english','science')) %>% 
  head(2)

# %in%기호 이용하기 
exam %>%filter(class %in% c(1,3,5))

#1반 수학 점수 평균 구하기
exam %>% filter(class == 1 ) %>% select(math) %>% lapply(mean)
class1 <- exam %>% filter(class == 1)
mean(class1$math)

#2반 영어점수 합계 구하기
exam %>% filter(class == 2) %>% select(english) %>% sum()


#mutate이용해서 파생변수 만들기 
exam1 <- exam %>% mutate(total = math + english + science,
                         mean = total/3,
                         result = ifelse (science >= 70,"pass","fail" )) %>% 
  arrange(total)

head(exam1)

#집단별로 요약하기
exam %>% group_by(class) %>% 
  summarise(mean_math = mean(math))

#___________________________________
exdata1 %>% summarise(TOT_Y17_AMT = sum(Y17_AMT))
exdata1 %>% group_by(AREA) %>% summarize(SUM_Y17_AMT = sum(Y17_AMT))


#제조사별로 suv자동차의 도시 및 고속도로 통합 연비 평균
#내림차순으로 정렬하고, 1~5위까지 출력하기___________________________________

mpg1 <- mpg %>% 
  mutate(tot = (cty+hwy)/2) %>% 
  filter(class == "suv") 

mpg1 %>% 
  group_by(manufacturer) %>% 
  summarise(tot_per_man =  (tot)) %>% 
  arrange(desc(tot_per_man)) %>% 
  head(5)

#____________________________ 

#중간고사
test1 <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,70,80,40,30))
#기말고사
test2 <- data.frame(id = c(1,2,3,4,5),
                    final = c(49,43,15,78,98))

test1 ; test2

total <- left_join(test1,test2, by="id")
total
#____________________________

name <- data.frame(class = c(1,2,3,4,5),
                   teacher = c("kim","lee","park",'choi',"jung"))
exam_new <- left_join(exam, name, by='class')

exam_new

#____________________________ 

#학생그룹1
group_a <- data.frame(id = c(1,2,3,4,5),
                    midterm = c(60,70,80,40,30))
#학생그룹2
group_b <- data.frame(id = c(6,7,8,9,10),
                    midterm = c(49,43,15,78,98))

group_all <- bind_rows(group_a,group_b)
group_all
