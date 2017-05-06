# Neural Nerwork by avNNet
scaled <- function(x) {(x - min(x)) / (max(x) - min(x))}
scale_back <- function(x) {(x * (max(Junyi_data$Received) - min(Junyi_data$Received)) + min(Junyi_data$Received))}

## didn't deseasonalize receive series
junyi.scaled  <- data.frame(received = scaled(Junyi_data$Received),
                            season = Junyi_data$season,
                            scaled.lag7_A = scaled(Junyi_data$lag7_A),
                            scaled.lag7_R = scaled(Junyi_data$lag7_R),
                            SchoolDay=Junyi_data$SchoolDay, Outlier=Junyi_data$Outlier)
junyi.scaled.train <- junyi.scaled[1:63,]
junyi.scaled.valid <- junyi.scaled[64:70,]
fit  <- avNNet(received ~ season + scaled.lag7_A + scaled.lag7_R + SchoolDay + Outlier, 
               data = junyi.scaled.train, size = 25, linout=TRUE)
pred.train <- scale_back(predict(fit, junyi.scaled.train, type = "raw"))
pred.valid <- scale_back(predict(fit, junyi.scaled.valid, type = "raw"))

# forecast future value
forecast_external <- read.csv("Data_Junyi_future-forecast-1.csv")
forecast_external$scaled.lag7_A <- scaled(forecast_external$lag7_A)
forecast_external$scaled.lag7_R <- scaled(forecast_external$lag7_R)

fit.forecast  <- avNNet(received ~ season + scaled.lag7_A + scaled.lag7_R + SchoolDay + Outlier, 
               data = junyi.scaled, size = 25, linout=TRUE)
pred.train <- scale_back(predict(fit.forecast, junyi.scaled, type = "raw"))
pred.train.ts <- ts(pred.train, start = c(1,1), frequency = 7)
future.pred <- data.frame(scale_back(predict(fit.forecast, forecast_external, type = "raw")))
accuracy(pred.train.ts, received.ts)
# Rolling forward to generate error
# manually change validation from 1 to 7 
nTotal = length(Junyi_data$Received)
roll_backward = 21
nValid = 1
error_1 <- c()
for (j in 0:(roll_backward - nValid - 1)){
  nTrain = nTotal - nValid
  train.ts <- window(received.ts, start = c(1,1), end = c(1,nTrain), frequency = 7)
  valid.ts <- window(received.ts, start = c(1, nTrain + 1),
                     end = c(1, nTotal), frequency = 7)
  
  junyi.scaled.train <- junyi.scaled[1:nTrain,]
  junyi.scaled.valid <- junyi.scaled[(nTrain + 1):nTotal,]

  fit  <- avNNet(received ~ season + scaled.lag7_A + scaled.lag7_R + SchoolDay + Outlier, 
                 data = junyi.scaled.train, size = 25, linout=TRUE)
  pred.train <- scale_back(predict(fit, junyi.scaled.train, type = "raw"))
  pred.valid <- scale_back(predict(fit, junyi.scaled.valid, type = "raw"))
  
  residuals <- valid.ts - pred.valid
  error_1 <- c(error_1, residuals)
  nTotal = nTotal - 1
}

write.table(round(error_1,8), file = "Junyi_rolling error_seperate_0102.CSV", sep = ";",append = TRUE)
