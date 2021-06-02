library(dplyr)
library(ggplot2)
library(wordcloud2)
library(KoNLP)
library(stringr)
library(RColorBrewer)

#세종사전
useSejongDic()

#gsub : 불용어 제거

#NEW
data1 <- readLines('c:/ken/data/좋아하는과일2.txt') ; data1
data1 <- unique(data1) ; data1 #distinct랑 비슷
#특수문자 제거
data1 <- str_replace_all(data1,"[^[:alpha:][:blank:]]","") ; data1 #알파벳&공백 조합 => 특수문자 (외우기)
data2 <- extractNoun(data1) ;data2 #단어로 분리
data2 <-lapply(data2,unique) ; data2 #리스트라서 lapply
'''
띄어쓰기가 안되어 있는 긴 문장이나 단어를 제거해야 할 경우
Filter함수를 이용하여 벡터를 데이터로 넣어야 한다. 
그래서 리스트 형태의 데이터를 unlist함수를 이용해서 벡터형태로 변형해야 함.
'''
data3 <- unlist(data2) ; data3 
data4 <- Filter(function(x) {nchar(x) <= 20 & nchar(x) >1}, data3) ; data4
#필요없는 단어나 기호 제거하기
data4 <- gsub("\\.","",data4) #마침표 제거
data4 <- gsub("\\'","",data4) #홑따옴표 제거
data4 <- gsub("\\^","",data4) ; data4 #캐럿기호 제거
#집계
wordcount <- table(data4); wordcount
sort(wordcount,decreasing = T)
#저장형식 : ANSI
txt <- readLines('c:/ken/data/1_gsub.txt') ; txt
#불용어 단어 개수
cnt_txt <- length(txt) ; cnt_txt
#불용어 첫 행부터 마지막 행까지 cnt_txt를 gsub실행
for (i in 1:cnt_txt) {
  data4 <- gsub((txt[i]),"",data4)
}
data4 #제거된 애들은 없어졌다~
#위작업을 하고 나서 삭제된 공백이나 1글자 이하의 글자를 제거
data4 <- Filter(function(x) {nchar(x) <= 10 & nchar(x) >1}, data4) ; data4
#최종 결과를 집계하여 워드 클라우드로 시각화
wordcount <- table(data4); wordcount
sort(wordcount,decreasing = T)
palete <- brewer.pal(9,"Set3")
wordcloud(names(wordcount),freq = wordcount,scale=c(4,1),rot.per = 0.5, min.freq = 1,
          random.order = F, random.color = T, colors = palete)

#교재 196p_____________________________
filter(mtcars,cyl >= 6 & mpg > 20)

head(arrange(mtcars,wt))
head(arrange(mtcars,mpg,desc(wt)))

head(select(mtcars,am,gear))

head(mutate(mtcars,years="1974"))
head(mutate(mtcars,mpg_rank=rank(mpg)))

distinct(mtcars,cyl)
distinct(mtcars,gear)
distinct(mtcars,cyl,gear)
summarise(mtcars, cyl_mean=mean(cyl),cyl_min=min(cyl),cyl_max=max(cyl))

gr_cyl <- group_by(mtcars,cyl)
summarise(gr_cyl,n())

gr_cyl<-group_by(mtcars,cyl)
summarise(gr_cyl,n_distinct(gear))

sample_n(mtcars,10)
sample_frac(mtcars,0.2)

group_by(mtcars,cyl) %>% summarise(n())

mp_rank <- mutate(mtcars,mpg_rank=rank(mpg))
arrange(mp_rank,mpg_rank)

mutate(mtcars, mpg_rank = rank(mpg)) %>% arrange(mpg_rank)

#________________________________________________________
library(ggplot2)
head(mpg)
ggplot(mpg,aes(class,fill=drv)) + geom_bar()

       