# curl -v -X GET "https://dapi.kakao.com/v2/search/blog" \
# --data-urlencode "query=https://brunch.co.kr/@tourism 집짓기" \
# -H "Authorization: KakaoAK {REST_API_KEY}"



rm(list = ls())

url <- "https://dapi.kakao.com/v3/search/book"
query <- "빅데이터분석"


# 한글로 변환
query <- URLencode(iconv(query, to='UTF-8')) 

#사이트 url %s ?인자 넘기기 # ?인자 넘기기
query_str <- sprintf("%s?target=title&query=%s", url, query)  

query_str
ls()

# 카카오 인증키 넣기
#################################################################
kakao_api_key <-"dbba4b1cfc9cfa44dcb922f63d38d555"
################################################################
auth_key_no <- sprintf("KakaoAK %s",kakao_api_key)  # 아래 원본 보고 복사
          # -H "Authorization: KakaoAK {REST_API_KEY}" # 대소문자 주의!

#사이트 요청하는 패키지
#install.packages("httr")
library(httr)

resp <-GET(query_str, add_headers("Authorization"=auth_key_no)) #헤더부분에 전송키를 보냄.
resp

# 클래스 확인
class(resp)
#[1] "response" 문자로 강제 변환

resp_char <-  as.character(resp)
#json 
#install.packages("jsonlite")
library(jsonlite)

d = fromJSON(resp_char)
class(d)
df <- data.frame(d) 
# list를 매트릭스로 변환
txt_1 <- as.matrix(df)

write.csv(txt_1,"kakao_api.csv", row.names = F)


########################### 불러올때 에러 (괄호 및 여러 이름 다음칸에 )
# 카카오 책 이름 갖고 오기
a < -read.csv("c:/ken/kakao_api.csv")

View(a)
View(df)



에러 남................
############################

#Error in read.table(file = file, header = header, sep = sep, quote = quote,  : 
                     # 열의 개수가 열의 이름들보다 많습니다
View(df)
df[1,1]
df[1,2]

df[2,1]


library(stringr)


for (r in 1:nrow(df)) {  # nrow행의 개수
  for (c in 1:col(df)) {  # 열의수
    df[r,c] = paste(df[r,c], collapse = " ")     # 문자열 하나로 합칠때
    df[r,c] = str_replace_all(df[r,c], "," , ":") # ,를 :로 바꿔라
    
  }
}

View(df)

###################


df2 <- as.matrix(df)
write.csv(df2, "kakao_api_final.csv", row.names = F)
getwd()
