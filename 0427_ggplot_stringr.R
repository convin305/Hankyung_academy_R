library(ggplot2)
library(dplyr)

''' 
require(dplyr) = library(dplyr)

dplyr있는데도 파이프연산자 안될 때

if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}

'''

ggplot(data = mpg, aes(x=drv,y=hwy)) + geom_boxplot()

#______________________
par(mfrow=c(1,2)) #매트립의 subplot
y1 <- c(1,2,3,4,5,6,7,8,9,10,20,25)
summary(y1)
boxplot(y1,las=1)

y2 <- c(50,20,3,45,5,20,15,30,9,10,20,100)
summary(y2)
boxplot(y2)

#compact,subcompact,suv의 cty비교하는 박스플롯
mpg_compare <- mpg %>% filter(class == c("compact","subcompact","suv") )
ggplot(data = mpg_compare, aes (x = class , y=cty)) + geom_boxplot()

#제조사별 고속도로 연비를 박스플랏으로 보기
boxplot(hwy~manufacturer,data=mpg)


abc <- c(100,300,150,280,310)
def <- c(180,200,210,190,170)
ghi <- c(210,150,260,210,70)

df_box <- data.frame( A = abc, B = def, C = ghi)
df_box

summary(df_box)
boxplot(df_box)
ls(df_box)


#grep : 특정 텍스트 검색, 대소문자 구분_______________________________________
grep_ex <- c("a.txt","A.txt","ab.txt","123.txt","bal123.txt")

#기본 - 위치를 찾아준다.
grep("^a", grep_ex) 

#이렇게 하면 위치번째의 값이 나오게 된다. 
grep("^a", grep_ex,value = T) 

#숫자를 포함하는 애들 다 찾기
grep("[0-9]", grep_ex, value=T) 

#숫자로 시작하는 데이터 제외
grep("^[^0-9]", grep_ex, value=T) 

#두개를 가지고 하는 방법 : 잘못된 방법
ptn <- c("^a","^A")
grep(ptn, grep_ex,value = T)

#두개를 가지고 하는 방법 : 맞는 방법 - paste함수사용, collapse로 |추가
grep(paste(ptn,collapse="|"), grep_ex,value = T)

#nchar예제 - 글자수세기
nchar_ex1 <- "먹어도 먹어도 배고파요 ㅠㅠ... 정상입니다."
nchar(nchar_ex1)

#paste예제(교재 90p)
paste(1,2,3)

#paste와 paste0은 띄어쓰기 차이
paste(1,2,3, sep="")
paste0(1,2,3)

paste("a","b","c")
paste("a","b","c", sep="")
paste0("a","b","c")
paste("a","b","c", sep="-")

#substr예제(교재90p)
substr('820715',3,4)

#strsplit 예제 (교재91)
tel <- "031)123-4567"
strsplit(tel,")")
tel <- "031-123-4567"
strsplit(tel,"-")

#_____________________________________________
install.packages("stringr")
library(stringr)

str_extract("홍길동24유관순56이순신37","[1-9]{2}")
str_extract_all("홍길동24유관순56이순신37","[1-9]{2}")

#반복하는 경우
string <- "hongkd105leess1002you25강감찬2005"
str_extract_all(string,'[a-z]{3}') #3개인 경우만
str_extract_all(string,'[a-z]{3,}') #3개 이상인 경우만
str_extract_all(string,'[a-z]{3,5}') #3개 이상 5개 이하인 경우

#stringr() 패키지 예제(교재92p)
txt4 <- c("aa.txt","ba.txt","ab.txt","123.txt")
str_replace_all(txtr,'a','z')

txt5 <- "사랑 사랑 누가 말했나~~~"
str_replace(txt5,"사랑","간식")
str_replace_all(txt5,'사랑','간식')

#교재 87p_______________________________________________

#문자열의 길이 구하기
len <- str_length(string) ; len

#특정 문자열의 위치 구하기
str_locate(string,"강감찬")

#부분문자열 만들기
strin_sub <- str_sub(string,1,len-7) ; string_sub

#대소문자 변경
str_to_upper(string)
str_to_lower(string)


#연습문제 2장 4번__________________________________________

Data2 <- c("2017-02-05 수입3000원",
           "2017-02-06 수입4500원",
           "2017-02-07 수입2500원")

#조건1)일자별 수입만을 출력하시오
str_length(Data2)
str_sub(Data2,14,18)


#추가
str_extract(Data2,'[0-9]{4,}[가-힣]') #오 좋은듯

#조건2)
str_replace_all(Data2,'[0-9]{1}','')

#조건3)
str_replace_all(Data2,"-","/")

#조건4)
paste(Data2,collapse = ",")


#빅데이터 분석을 위한 R프로그래밍 3장 93p_____________________

#키보드로 직접 데이터를 입력받기 - 숫자의 경우
num <- scan()
num
#키보드로 직접 데이터를 입력받기 - 문자의 경우(what = character())
name <- scan(what = character()) 


#타이타닉______________________________
titanic <- read.csv('https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv')
head(titanic)
dim(titanic)
str(titanic)
summary(titanic)

tab <- table(titanic$survived,titanic$sex) ; tab
barplot(tab,col=rainbow(2),main="성별에 따른 생존여부",legend=T)


#파일저장_______________________________
#1
setwd("C:/ken/data")
install.packages("RSADBE")
library(RSADBE)
data(Severity_Counts)
sink("severity.txt")
severity <- Severity_Counts
severity
sink()

#2
write.table(titanic,"titanic.txt",row.names = F)

#3
st.df <- Data2
write.csv(st.df,"stdf.csv",row.names = F, quote = F)


#3장 연습문제_______________________
head(titanic)
write.csv(titanic,"titanic.csv",row.names = F, quote =F)
titanicData <- read.csv('titanic.csv')
head(titanicData)

str(titanicData)

tiDa <- titanicData[,c(-1,-3)]
head(tiDa)

library(dplyr)
head(CO2)

CO2 %>%  filter(CO2$Treatment == 'nonchilled') %>% write.csv("CO2_df1.csv",row.names = F)
CO2 %>%  filter(CO2$Treatment == 'chilled') %>% write.csv("CO2_df2.csv",row.names = F)


#NLPr관련 설치__________________________________________________________________
.libPaths() #패키지경로로

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_291") #자바설정
Sys.getenv("JAVA_HOME") #설정 확인

install.packages("rJava") # 자바패키지 설치
library(rJava)

install.packages("KoNLP") # 한글 패키지 설치
library(KoNLP)

install.packages("remotes") #한글패키지 설치
remotes::install_github('haven-jeon/KoNLP',upgrade='never',
                        INSTALL_opts=c("--no-multiarch"))
library(KoNLP)

install.packages("wordcloud2")
library(wordcloud2)

useSystemDic() #시스템 사전 설정
useSejongDic() #세종 사전 설정
useNIADic() #NIAdic 사전 설정

#_______________________________________________
#비정형 데이터 읽어올 때에는 readLines
word_data <- readLines("애국가(가사).txt") ; word_data

#sapply를 이용하여 명사추출
word_data2 <- sapply(word_data,extractNoun,USE.NAMES = F) ; word_data2

#단어추가하여 사용자 정의 사전에 별도 추가
#sapply를 이용하여 명사추출:리스트 형태로 가져오려고 - extract"N"oun
add_words <- c("백두산","남산","철갑","가을","하늘","달")
buildDictionary(user_dic = data.frame(add_words,rep("ncn",length(add_words))),replace_usr_dic = T)

#추가한 단어를 가지고 다시 명사추출
word_data2 <- sapply(word_data,extractNoun,USE.NAMES = F) ; word_data2

#word_data2를 벡터로 변환한 후 undata변수에 할당
#합산해야해서 리스트를 다시 벡터형태로 만들기
undata <- unlist(word_data2)

#undata의 빈도 수 확인 후 word_table에 할당
word_table <- table(undata)
word_table

#undata에서 쓸모있는 두글자 이상만 선별하여 할당.
undata2 <- Filter(function(x){nchar(x) >= 2}, undata)
word_table2 <- table(undata2)
word_table2
