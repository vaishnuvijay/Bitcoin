library(anytime)
library(xts)
library(hts)
library(forecast)
train = read.csv("C:/Users/vaish/OneDrive/Desktop/BTC2.csv")
head(train)
summary(train)
train$Date = as.Date(anytime(train$Date))
train$Date = as.Date(anytime(train$Date))
train$Volume = gsub(',','',train$Volume)
train$Market.Cap = gsub(',','',train$Market.Cap)
train$Volume <- NULL
train$Market.Cap = as.numeric(train$Market.Cap)
head(train)
Train = xts(train[, -1], order.by=as.POSIXct(train$Date))
plot(ses(Train[,'Close']))
plot(holt(Train[,'Close']))
plot(forecast(Train[,'Close']))
bitcoin  <- data.frame(close=train$Close,
                       open=log(train$Open+1),
                       high=log(train$High),
                       low=log(train$Low+1),
                       market=log(train$Market.Cap+1))
fit = auto.arima(Train[,"Close"])
tsdisplay(arima.errors(fit), main="ARIMA errors")
plot(fitted(fit), bitcoin$close,ylab="Closing Price", xlab="Predicted Closing price")
m <- HoltWinters(Train[,'Close'], gamma = FALSE)
plot(Train, xlab="Year",main="The Closing value of Bitcoins")
y <- hts(Train,  nodes=list(3, c(3,1,1)))
allf <- forecast(y, h=80)
plot(allf)
fit <- step(lm(close ~ open  + high 
               + low + market, data=bitcoin))