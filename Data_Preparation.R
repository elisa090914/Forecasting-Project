# Basic setting
library(lubridate)
library(forecast)
library(caret)

JunYi_data <- data.frame(read.csv("Data_Junyi_1230new.csv"))

# Data Partition
received.ts <- ts(JunYi_data$Received, start = c(1,1), frequency = 7)
active.ts <- ts(JunYi_data$Active.user, start = c(1,1), frequency = 7)
register.ts <- ts(JunYi_data$Registration, start = c(1,1), frequency = 7)
nValid = 7
nTrain = length(received.ts) - nValid
recv.train.ts <- window(received.ts, start = c(1,1), end = c(1,nTrain), frequency = 7)
recv.valid.ts <- window(received.ts, start = c(1, nTrain + 1), end = c(1, nTrain + nValid), frequency = 7)
active.train.ts <- window(active.ts, start = c(1,1), end = c(1,nTrain), frequency = 7)
active.valid.ts <- window(active.ts, start = c(1, nTrain + 1), end = c(1, nTrain + nValid), frequency = 7)
