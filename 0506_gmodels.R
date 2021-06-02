library("dplyr")
library(readxl)

#책268p_________________________
dustdata <- read_excel("c:/ken/data/localAverage.xlsx")
dustdata %>% head()

str(dustdata)

dust_anal <- dustdata %>% filter(area %in% c("성북구","중구"))
dust_anal

count(dust_anal,yyyymmdd) %>% arrange(desc(n))
count(dust_anal, area) %>%  arrange(desc(n))

da_sb <- subset(dust_anal, area == "성북구")
da_ju <- subset(dust_anal, area == "중구")

library(psych)
describe(da_sb$finedust)
describe(da_ju$finedust)

boxplot(da_sb$finedust,da_ju$finedust,
        main = "fine_dust_compare",xlab="AREA",ylab="FINEDUST_PM",
        names=c("성북구","중구"),col=c("blue","green"))

t.test(data=dust_anal,finedust~area,var.equal=T)


#가메416p___________________

m = 50
sa_1 <- rnorm(50,58,3) #표본뽑기

#상자그림
boxplot(sa_1)

#정규성검정
shapiro.test(sa_1)
#p값이 0.05보다 크므로 정규성을 만족한다. 

#단측검정
t.test(sa_1,mu=m,alternaive="less")
t.test(sa_1,mu=m,alternaive="greater")
#양측검정
t.test(sa_1,mu=m,var.equal = T)

#________________
mid = c(16,20,21,22,23,22,27,25,27,28)
final = c(19,22,24,24,25,25,26,26,28,32)

#정규성확인
shapiro.test(mid)
shapiro.test(final)

a = data.frame(mid,final)
a

#귀무가설 : 평균이 차이가 없다. / 대립가설 : 평균이 차이가 있다.
#평균이 같은지
t.test(final,mid,paired = T, var.equal = T, data=a)
t.test(mid,final,paired = T, var.equal = T, data=a)

#______________________________
install.packages("corrplot")
library(corrplot)

data <- mtcars
data %>% head()

car_cor <- cor(mtcars)
round(car_cor,2)

corrplot(car_cor)

str(car_cor)
summary(car_cor)

#________________________
install.packages("gmodels")


#________________________
sheep <- read_xlsx("c:/ken/sheep.xlsx")
sheep
t.test(sheep$wo,sheep$wi,paired=T,alter="l")
