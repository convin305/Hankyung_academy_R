install.packages("glmnet")
library(glmnet)
library(dplyr)

data2 %>% head()

y <- data2$stock
x <- data2 %>% select(house,moneyrate,coin,foreigner,customerprice,job) %>% data.matrix()

lambdas <- seq(0,0.5,by=0.05)

lasso_fit <- cv.glmnet(x,y,alpha=0,lambda = lambdas)
plot(lasso_fit)

opt_lambda <- lasso_fit$lambda.min
opt_lambda

fin_lasso <- glmnet(x,y,alpha=0,lambda = opt_lambda)
coef(fin_lasso)

plot

#_______________________________________________________________
install.packages("tidymodels")
library(tidymodels)

set.seed(1000)
hmm <- initial_split(data2,prop=0.7)
TR <- training(hmm)
TS <- testing(hmm)

TROUT <- TR %>% dplyr::select(stock)
TSOUT <- TS %>% dplyr::select(stock)

full <- lm(stock~.,data=TR)
new <- stepAIC(full)

TSOUT$fitted <- predict(new, newdata = TS)

TSOUT$diff <- (TSOUT$stock - TSOUT$fitted)

TSOUT


#회귀식 예측_______________________________________________________________

house = c(127.3211,130.1539,132.9868)
moneyrate = c(0.465969708 , 0.435346231 , 0.404722754)
foreigner = c(31.53127521, 31.47181332, 31.41235144)
customerprice = c(106.6462492, 106.7524916, 106.5993608)  

new_data = data.frame(house,moneyrate,foreigner,customerprice)
new_data

predict(result4,newdata = new_data)
