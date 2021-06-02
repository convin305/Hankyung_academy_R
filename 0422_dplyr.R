library(dplyr)

#header=F를 지정하지 않는 경우 자동으로 첫번째 행이 컬럼명이라고 인식하게 된다.
no_header <- read.csv('C:/ken/data/no_mid_exam2.csv',header = F)
no_header

rename_list <- c('class','number','English','Math')

make_header <- read.csv('C:/ken/data/no_mid_exam2.csv',header = F,
                        col.names = rename_list)
make_header

#비정형 데이터 불러오기
txt1 <- readLines('C:/ken/data/구매후기.txt')
txt1

txt2 <- readLines('C:/ken/data/구매후기_utf8.txt',encoding = 'UTF-8')
txt2

#테이블 형식이 있는 데이터 불러오기
txt4 <- read.table('C:/ken/data/전공.txt')
txt4

txt5 <- read.table('C:/ken/data/전공2.txt', sep = ":")
txt5

txt6 <- read.table('c:/ken/data/전공3.txt', sep = ",")
txt6

#____________________________________________________________
data(iris)
df.1 <- iris[iris$Species=='setosa',]
df.2 <- subset(iris,Species == 'setosa')
df.3 <- filter(iris,Species == "setosa")

head(df.1,3)
head(df.2,3)
head(df.3,3)

#____________________________________________________________

ex_data <- read.table('C:/ken/data/data_ex.txt',skip=2)
head(ex_data)
View(ex_data)

#______________________________________________________________
library(readxl)

exdata1 <- read_excel('c:/ken/data/sample1.xlsx')
exdata1 <- rename(exdata1, Y17_AMT = AMT17, Y16_AMT = AMT16)

exdata1$AMT <- exdata1$Y17_AMT + exdata1$Y16_AMT
exdata1$CNT <- exdata1$Y17_CNT + exdata1$Y16_CNT
exdata1$AVG_AMT <- exdata1$AMT / exdata1$CNT

exdata1$AGE50_YN <- ifelse(exdata1$AGE >= 50, "Y", "N")

exdata1$AGE_GR10 <- ifelse(exdata1$AGE >= 50, "A1.50++",
                           ifelse(exdata1$AGE >= 40, "A2.4049",
                                  ifelse(exdata1$AGE >= 30,"A3.3039",
                                         ifelse(exdata1$AGE >= 20,"A4.2029",
                                                "A5.0019"))))

View(exdata1)

#________________________________________________________________

library(ggplot2)

mpg <- ggplot2::mpg

mpg$total <- (mpg$cty + mpg$hwy)/2

mpg$con <- ifelse(mpg$total >= 25 & mpg$drv >= 20 , '우수' , "일반")

sep_mpg <- subset(mpg,select=c('manufacturer','trans','drv','cty','hwy','class','total','con'))

sep_mpg

View(mpg)

#집계 table()

table(mpg$con)

xtabs(~con, data=mpg)

#새창 띄우기 dev.new()
qplot(mpg$con)

#______________________________________

head(sep_mpg,2)
sep_mpg[2,2] <- "auto(90)"

sep_mpg[1,6] <- "suv"

#______________________________________

df <- data.frame(sex = c("M","F",NA,"M","F"),scorer = c(6,5,3,4,NA))
df

is.na(df)

#결측치 빈도수 출력
table(is.na(df))

mean(df$scorer, na.rm = T)
sum(df$scorer, na.rm=T)

#______________________________________
'''
단축키
한번에 주석처리 : C+S+C
파이프 : C+S+M
마크다운 청크 : C+A+I
'''
#______________________________________
library(dplyr)

df %>% filter(!is.na(scorer)) #score결측치 제거

df_nomiss <- df %>% filter(!is.na(scorer) & !is.na(sex)) 
df_nomiss
mean(df_nomiss$scorer)

df_nomiss2 <- na.omit(df)
df_nomiss2
#______________________________________P113

library(dplyr)
library(readxl)

exdata1 <- read_excel('c:/ken/data/sample1.xlsx')

exdata1 %>%  select(ID)
exdata1 %>%  select(ID, AREA, Y17_CNT)
exdata1 %>%  select(-AREA)
exdata1 %>%  select(-AREA, -Y17_CNT)

#filter : 조건절 추출
exdata1 %>%  filter(AREA == "서울")
exdata1 %>%  filter(AREA == "서울" & Y17_CNT >= 10)

#arrange : 정렬
exdata1 %>%  arrange(AGE)
exdata1 %>% arrange(desc(Y17_AMT))
exdata1 %>%  arrange(AGE,desc(Y17_AMT))


ls()
library()

a <- 4
rm(a)

search()

dim(iris)

summary(exdata1)
head(iris)

#______________________________________마무리과제

color <- iris %>%  filter(Species=="versicolor" & Sepal.Length >= 6)
color <- color %>% select(Sepal.Length,Petal.Length)
write.csv(color,"C:/ken/color.csv",row.names = F)

library(ggplot2)
