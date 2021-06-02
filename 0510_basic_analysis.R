library(ggplot2)
library(psych) #기초통계
library(dplyr)

data(diamonds)
str(diamonds)

dc <- diamonds$carat
dp <- diamonds$price

cor(diamonds$carat,diamonds$price)
summary(diamonds$price)

describe(dp)


#다이아몬드 가격이 3933이상인 개수
diamonds %>% filter(price >= 3933) %>% summarize(n=n())

#trim 
diamonds %>% filter(price >= 3933) %>% summarise(mean_1 = mean(carat),
                                    trim_mean = mean(carat, trim=0.05))

install.packages("e1071")
library(e1071)

diamonds %>% filter(price >= 3933) %>% summarize(mean_1 = mean(carat),
                                                 trim_mean = mean(carat,trim=0.05),
                                                 SKEW = e1071::skewness(carat),
                                                 KURT = e1071::kurtosis(carat))
