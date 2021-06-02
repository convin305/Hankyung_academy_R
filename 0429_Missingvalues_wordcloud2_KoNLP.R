library(dplyr)

df = data.frame(class=c(2,1,2,1,1,2),english=c(98,97,86,98,80,89),science=c(50,60,78,58,65,98))
df_1 = df %>% filter(class == 1) ; df_1
df_2 = df %>% filter(class == 2) ; df_2
mean_sci = rbind(class1 = round(mean(df_1$science),1),class2 = round(mean(df_2$science),1));mean_sci

exam = read.csv('c:/ken/data/csv_exam.csv') ; head(exam)
exam %>% group_by(class) %>% summarise(mean_math=mean(math),sum_math = sum(math),median_math = median(math),n=n())

exam2 = exam
exam2$math[c(3,8,15)] = NA
exam2 %>% group_by(class) %>% summarise(mean_math=mean(math,na.rm=T),sum_math = sum(math,na.rm=T),median_math = median(math,na.rm=T),n=n())

exam3 = exam2
exam3 <- exam3[!is.na(exam3$math),]
exam3 %>%  filter(!is.na(exam3$math))


exam4 = na.omit(exam2)
exam4


#______________________________________________________
'''
결측치 대체하기
-결측치가 많을 경우 모두 제외하면 데이터의 손실이 크다.
-대안 : 다른 값 채워넣기

결측치 대체법 - Imputation
-대표값(평균,최빈값)등으로 일괄 대체
-통계분석 기법 적용, 예측값을 추정해서 대체
'''
exam_df = data.frame(class=c(2,1,2,1,1,2),english=c(98,97,40,98,20,89),science=c(50,60,78,58,65,98))
exam_df[c(3,5),'english'] <- NA
mean(exam_df$english,na.rm=T)
exam_df$english <- ifelse(is.na(exam_df$english),95,exam_df$english)
exam_df

#______________________________________________________

na_df = data.frame(class=c(2,1,NA,1,1,2),english=c(98,NA,40,98,20,89),science=c(50,60,NA,58,65,98))
na_df2 = na.omit(na_df)
na_df2
#______________________________________________________
library(readxl)
stem(exam4$math)

exdata <- read_xlsx('c:/ken/data/Sample1.xlsx')
stem(exdata$AGE)

#히스토그램 - 연속형 / 바플롯 - 이산형
hist(exdata$AGE)

par(mfrow=c(2,1))
round(rnorm(50,70,3),2) %>% hist(main='Histogram of sd 3')
round(rnorm(50,70,20),2) %>% hist(main = "Histogram of sd 20")

install.packages("descr")
library(descr)
par(mfrow=c(1,1))
freq(exdata$SEX, plot=T, main="성별(barplot)")

dist_sex <- table(exdata$SEX)
dist_sex
barplot(dist_sex)

barplot(dist_sex,ylim=c(0,8))
#그래프명칭, 항목값 변경
barplot(dist_sex,ylim=c(0,8),main="BARPLOT",xlab = "SEX", ylab="FREQUENCY",names=c("F","M")) 
barplot(dist_sex,ylim=c(0,8),main="BARPLOT",xlab = "SEX", ylab="FREQUENCY",names=c("F","M"),col=c("pink","navy")) #색상변경

boxplot(exdata$Y17_CNT,exdata$Y16_CNT)
boxplot(exdata$Y17_CNT,exdata$Y16_CNT,ylim=c(0,60),main='boxplot',names=c("17년건수","16년건수"))
boxplot(exdata$Y17_CNT,exdata$Y16_CNT,ylim=c(0,60),main='boxplot',names=c("17년건수","16년건수"),col=c("green","yellow"))

y1 <- c(1,2,3,4,5,6,7,8,9,10,20,25)
boxplot(y1)
#이상치제거
y1 <- na.omit( ifelse(y1 >= 15,NA,y1) )
y1

#________________________________________________________________________
'''
정적스크래핑
-웹 스크래핑 : 웹사이트 상에서 원하는 부분에 위치한 정보를 컴퓨터로 하여금 자동으로 추출
-웹 크롤링 : 자동화 봇인 웹 크롤러가 정해진 규칙에 따라서 복수 개의 웹 페이지를 브라우징 하는행위

정적스크래핑(크롤링) 네이버영화 사이트 댓글정보, 스크래핑 네이버 영화 사이트의 데이터 중 영화제목, 평점, 리뷰만을 추출하여 
CSV파일의 정형화된 형식으로 저장한다.
1.스크래핑 하려는 웹페이지의 URL구조와 문서구조를 파악

rvest패키지의 주요 함수 
html_nodes(x,css,xpath) / html_node(x,css,xpath)
html_test(x,trim=F)
html-attrs(x)
html-attr(x,name,default="")

html_text() : 텍스트를 추출
html_name() : 어트리뷰트의 이름을 가져옴
html_attrs() : 어트리뷰트의 밸류 추출
html_tag() : tag이름 추출
html_children() : 해당 요소의 하위 요소를 읽어온다. 

attribute name : <div align
attribute vaule : "left"> 안녕~
'''
install.packages('rvest')
library(rvest)
library(stringr)

#가져오려는 영화리뷰 URL
main_url = "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=173123&type=after&onlyActualPointYn=Y&onlySpoilerPointYn=N&order=sympathyScore&page="
main_url

reply_list <- character()
star_list <- numeric()
date_list <- character()

page_url = 1
for (page_url in 1:10) {
 
  url <- paste(main_url, page_url, sep="")
  
  content <- read_html(url)
  
  node_1 <- html_nodes(content, ".score_reple p")
  node_2 <- html_nodes(content,".score_result .star_score em")
  node_3 <- html_nodes(content,".score_reple em:nth-child(2)")
  #html_node() : 어트리뷰트의 이름을 가져옴. -매칭되는 한 요소만 반환
  #html_nodes() : 어트리뷰트의 밸류를 가져옴. - 매칭되는 모든 요소 반환. tag,css모두 가져올 경우.
  
  reply <- html_text(node_1)
  star <- html_text(node_2)
  date <- html_text(node_3)
  date <- as.Date(gsub("\\.","-",date)) #날,을을 -로 바꾸기
  
  reply_list <- append(reply_list,reply)
  star_list <- append(star_list, star)
  date_list <- append(date_list, date)
  }

#html nodes html text append
#위의 for문 필더명을 아래 colnames함수를 이용하여
df <- data.frame(reply_list, star_list, date_list) 
colnames(df) = c("댓글","평점","날짜")
df

#저장할 경로 설정, csv파일로 저장
write.csv(df,'c:/ken/data/movie.csv')
getwd()

#_____________________________
library(wordcloud2)
library(KoNLP)

#세종사전
useSejongDic()

reples <- df[,1]
reples <- unique(reples)
reples <- str_replace_all(reples,"[^[:alpha:][:blank:]]","")
words <- extractNoun(reples)
words <-lapply(words,unique)
words <- unlist(words)
words <- Filter(function(x) {nchar(x) <= 10 & nchar(x) >1}, words)
words

words <- gsub("\\.","",words) #마침표 제거
words <- gsub("\\'","",words) #홑따옴표 제거
words <- gsub("\\^","",words)

words <- gsub("맨이","맨",words)
words <- gsub("맨은","맨",words)
words <- gsub("맨과","맨",words)
words <- gsub("모습이랄","모습",words)
words <- gsub("씬은","씬",words)
words <- gsub("밍과","밍",words)
words <- gsub("볼꺼리","볼거리",words)
words <- gsub("블은","블",words)
words <- gsub("론이","론",words)
words <- gsub("피터게","피터",words)
words <- gsub("좋아구","좋음",words)
words <- gsub("좋았","좋음",words)

#불용어 제거
txt <- readLines('c:/ken/data/3_gsub.txt')
cnt_txt <- length(txt)
for (i in 1:cnt_txt) {
  words <- gsub((txt[i]),"",words)
}

#재미 - 단어 정리
fun <- readLines('c:/ken/data/4_gsubfun.txt')
cnt_fun <- length(fun)
for (i in 1:cnt_fun) {
  words <- gsub((fun[i]),"재미",words)
}


words <- Filter(function(x) {nchar(x) <= 5 & nchar(x) >1}, words)

wordcount <- table(words)
sort(wordcount,decreasing = T)

wordcloud2(wordcount)
