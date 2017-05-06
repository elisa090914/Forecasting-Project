# Original time plot
plot(received.ts, xlab = "Week", main = "Received Mails")
plot(active.ts, xlab = "Week", main = "Daily Active Users")
plot(register.ts, xlab = "Week", main = "Daily Register Users")

# Fitting Models
## regression model and arima

received.tslm <- tslm(recv.train.ts ~ season)
forecast.recv <- forecast(received.tslm, h = nValid)
accuracy(forecast.recv, recv.valid.ts)
plot(received.tslm$residuals, xlab = "Week", ylab = "residual", main = "Residual")
lines(recv.train.ts, col = "blue")
lines(received.tslm$fitted.values, col = "red")
Acf(received.tslm$residuals ,lag.max = 7)
recv.res.arima <- auto.arima(received.tslm$residuals)
Acf(recv.res.arima$residuals ,lag.max = 7)

##naive forecasts using roll-forward partitioning
stepsAhead <- 1
error <- rep(0, nValid - stepsAhead +1)
pred.value <- rep(0, nValid - stepsAhead +1)
for (j in nTrain : (nTrain + nValid - stepsAhead)){
  train.ts <- window(register.ts, start = c(1, 1), end = c(1, j), frequency = 7)
  valid.ts <- window(register.ts, start = c(1, j + stepsAhead), end = c(1, nTrain + nValid), frequency = 7)
  naive.pred <- naive(train.ts, h = stepsAhead)
  pred.value [j - nTrain +1] <- naive.pred$mean[stepsAhead]
  error[j - nTrain +1] <- valid.ts - pred.value [j - nTrain +1]
}

rolling.naive.recv <- ts(pred.value, start = c(11, 1), end = c(11,7), frequency = 7)
error.ts <- ts(error, start = c(11, 1), end = c(11,7), frequency = 7)

plot(received.ts, xlab = "Week", main = "# of received mail by using naive forecast")
lines(rolling.naive.recv, col = "blue")
plot(error.ts, main = "Forecast error of received mail by using naive forecast")


