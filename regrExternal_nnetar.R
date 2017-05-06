# Select Model
# regression model with external data
x <- as.data.frame(JunYi_data[,c(3,5,8:10)])
x.train <- x[1:nTrain, ]
x.valid <- x[(nTrain + 1):nTotal, ]
recv.exn.tslm <- tslm(recv.train.ts ~ season + lag7_A + SchoolDay + Outlier
                      , data = x.train)
forecast.exn <- forecast(recv.exn.tslm, h = nValid, newdata = x.valid)
train.residual <- recv.train.ts - forecast.exn$fitted
valid.residual <- recv.valid.ts - forecast.exn$mean

plot(received.ts, ylim = c(-20,50), xlab = "Week", main = "Received Mails")
lines(forecast.exn$fitted, col = "blue", lwd = 2, lty = 2)
lines(forecast.exn$mean, col = "red", lwd = 2, lty = 2)
lines(train.residual, col = "green", lwd = 2, lty = 2)
lines(valid.residual, col = "green", lwd = 2, lty = 2)

accuracy(forecast.exn$fitted, recv.train.ts)
accuracy(forecast.exn$mean, recv.valid.ts)


##
#NN
origin_junyi <- read.csv("Data_Junyi_0102.csv")
received.ts <- ts(origin_junyi$Received, start = c(1,1), frequency = 7)
nTotal = length(origin_junyi$Received)
nValid = 7
nTrain = nTotal - nValid
train.ts <- window(received.ts, start = c(1,1), end = c(1,nTrain), frequency = 7)
valid.ts <- window(received.ts, start = c(1, nTrain + 1),
                   end = c(1, nTotal), frequency = 7)
set.seed(201)
recv.nnetar <- nnetar(train.ts, p = 6) # default repeats=20, P=1
recv.nnetar.pred <- forecast(recv.nnetar, h = nValid)
accuracy(recv.nnetar.pred, valid.ts)  #overfit(?)

plot(received.ts, ylim = c(-20,50), xlab = "Week", main = "Received Mails")
lines(recv.nnetar.pred$fitted, col = "blue", lwd = 2, lty = 2)
lines(recv.nnetar.pred$mean, col = "red", lwd = 2, lty = 2)
lines(train.residual, col = "green", lwd = 2, lty = 2)
lines(valid.residual, col = "green", lwd = 2, lty = 2)
 