rm(list=ls())
data(cars)
attach(cars)

cor.test(speed,dist)

LM <- lm(dist~speed,data=cars)
names(LM)

fitted.values(LM)
residuals(LM)
plot(speed,dist)
abline(LM,col='red',lwd='2')

dist[23:49]
LM$residuals

plot(LM)
#_______________________________
install.packages("lmtest")
library(lmtest)


result1<-lm(stock~house+job+moneyrate,data=without.coin)

acf(result1)

install.packages("tidyverse")
install.packages("fpp3")

library(fpp3)
library(tidyverse)

ts <- TSLM(stock ~ trend())


without.coin %>% model(SNAIVE(stock))

new <- read_excel("C:/ken/r_pr_data.xlsx", sheet = "2")
new <- new[-c(40,41),c(1:7)]
ls(new)
newlm <- lm(stock~.,data=new)
summary(newlm)

shapiro.test(resid(newlm))

durbinWatsonTest(newlm)


cor(new)
corrplot::corrplot.mixed(cor(new),upper='ellipse')
vif(newlm)

plot(newlm)

library(MASS)

full <- lm(stock~.,data=data2)
stepresult <- stepAIC(full,direction = "both")

library(lmtest)
dwtest(stepresult)

durbinWatsonTest(stepresult)
shapiro.test(resid(stepresult))

summary(stepresult)
_________________________________________

result3 <- lm(stock ~ house + moneyrate + coin + foreigner + customerprice,data=data2)
summary(result3)
durbinWatsonTest(result3)
shapiro.test(resid(result3))
plot(result3)
___________________________________________
result4<-lm(stock~house+moneyrate,data=data1)
summary(result4)

durbinWatsonTest(result4)
dwtest(result4)

hmm <- lm(stock~house+moneyrate,data=data1)
summary(hmm)
durbinWatsonTest(hmm)
shapiro.test(resid(hmm))


hmm2 <- lm(stock~)
_____________________________________________________

hmmm <- lm(stock~house+moneyrate+foreigner+customerprice+job,data=data2)
summary(hmmm2)

hmmm2 <- lm(stock~house+moneyrate+foreigner+customerprice,data=data2)



rm(list=ls())
______________________________________________________________

hmmm2 <- lm(stock~house+moneyrate+foreigner+customerprice + ,data=data2)

par(mfrow=c(2,1))
plot(hmmm2,1)











data4 <- data2[-c(40,41),]
hmm3 <- lm(stock~.,data=data4)
aa <-stepAIC(hmm3)
summary(aa)
durbinWatsonTest(aa)
shapiro.test(resid(aa))



par(mfrow=c(2,1))
plot(aa,1)
plot(hmm3,3)
plot(fitted.values(hmm3),resid(hmm3))

lm.beta(hmm3)


