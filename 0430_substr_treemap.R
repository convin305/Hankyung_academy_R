rm(list=ls())

#데이터 불러오기 
library(stringr)
library(KoNLP)
library(wordcloud)
library(dplyr)
library(RColorBrewer)
useSejongDic()

txt <- readLines("c:/ken/data/hiphop.txt")
head(txt)

#특수문자 제거
#txt <- str_replace_all(txt,"\\W","") #단어가 아닌 것
txt <- str_replace_all(txt,"[^[:alpha:][:blank:]]","")
txt

#명사추출
noun <- extractNoun(txt)
head(noun)

#list로 문자열 벡터로 변환 후 단어별 빈도표 작성
wordcount <- table(unlist(noun))

#데이터 프레임으로 변환(stringAsFactors : 강제 팩터화)
df_word <- as.data.frame(wordcount, stringsAsFactors = F)

#변수명 수정
df_word <- rename(df_word,
                  word = Var1,
                  freq = Freq)
df_word

#2글자 이상만
df_word <- filter(df_word, nchar(word) >= 2)

head(df_word)
top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top20

#단어색상목록 만들기
#dev.new() #새창띄우기
display.brewer.all() #모든 컬러 확인인

#워드클라우드 만들기
pal <- brewer.pal(8,"Blues")

set.seed(1) #난수고정

wordcloud(words = df_word$word, freq = df_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4,0.5),
          colors = pal
          )

#교재 245p____________________________________
data("mtcars")

filter(mtcars,cyl == 4)

head(arrange(mtcars,wt))

head(select(mtcars,am,gear))

head(mutate(mtcars,years="1974"))

distinct(mtcars,cyl)

summarise(mtcars,cyl_mean=mean(cyl),cyl_min = min(cyl), cyl_max = max(cyl))

sample_n(mtcars,10)

group_by(mtcars,cyl) %>% summarise(n())

#교재 248p____________________________________
library(readxl)

#1.
mme <- read_xlsx('c:/ken/data/middle_mid_exam.xlsx')
head(mme)

#3.
mme %>% group_by(CLASS) %>% summarise(eng_mean = mean(ENGLISH),eng_sum = sum(ENGLISH), math_mean = mean(MATHEMATICS), math_sum = sum(MATHEMATICS))

#4.
mme %>% filter(MATHEMATICS >= 80 & CLASS == 'class1')

#5.
mme %>% arrange(desc(MATHEMATICS),ENGLISH)

#6.
mme %>% filter(MATHEMATICS >= 80 & ENGLISH >= 85) %>% summarise(n_students = n())

#교재 253p____________________________________

ck <- read_excel('c:/ken/data/치킨집_가공.xlsx')
head(ck)

#소재지전체주소 중에서 11번째 글자부터 16번째 글자 앞까지 추출
addr <-substr(ck$소재지전체주소, 12, 16) ; addr

add_num <- gsub('[0-9]',"",addr)
addr_trim <- gsub(" ","",add_num)
head(addr_trim)

addr_count <- table(addr_trim) %>% data.frame()
addr_count

install.packages('treemap')
library(treemap)

treemap(addr_count,index = "addr_trim",vSize="Freq",title="서대문구 동별 치킨집 분포표")

#강남구 병원 분포 트리맵____________________________________

hos <- read_excel('c:/ken/data/hospital.xlsx')
head(hos)

hosr <-substr(hos$도로명전체주소,11,16) ; hosr
hos_num <- gsub('[0-9]',"",hosr) ;hos_num
hos_num <- gsub(" ","",hos_num)
hos_num<- gsub("길","",hos_num) ; hos_num

hos_count <- table(hos_num) %>% data.frame()
hos_count

treemap(hos_count, index="hos_num", vSize="Freq",title="강남구 도로명주소별 병원 분포표")

#GNI2014____________________________________
data("GNI2014")
ls(GNI2014)
head(GNI2014)

treemap(GNI2014,index = c("continent","iso3"),vSize="population",title="세계인구",bg.labels="blue")

#인구수가 높은 순으로 20개나라를 GNI2014_po로 저장 -> 트리맵그리기
GNI2014_po <- GNI2014 %>% arrange(desc(population)) %>% head(20)
GNI2014_po

treemap(GNI2014_po,index=c('continent','iso3'),vSize='population',title="인구 높은 순 TOP20")

#1인당 국민소득을 per_pop변수에 할당 후 트리맵작성
ls(GNI2014)

GNI2014_per <- GNI2014 %>% mutate(per_pop = GNI/population) %>% arrange(desc(per_pop)) %>% head(20)
treemap(GNI2014_per,index=c('continent','country'),vSize='per_pop',title="1인당 국민소득 TOP20")

  n write.csv(GNI2014,"c:/ken/GNI2014.csv")
