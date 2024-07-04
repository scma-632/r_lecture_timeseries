setwd('D:\\SCMA 2024\\Data')
library(readxl)
sheet= excel_sheets('US_coco.xlsx')
sheet

df1= read_excel('US_coco.xlsx', sheet='c')
df2= read_excel('US_coco.xlsx', sheet='c0')
df3= read_excel('US_coco.xlsx', sheet='c1')
df4= read_excel('US_coco.xlsx', sheet='c2')
df = rbind(df1,df2,df3,df4)

dim(df1)
dim(df2)
dim(df3)
dim(df4)
dim(df)
View(df)

df$Date = as.Date(df$Date, format='%d-%m-%Y')
View(df)
str(df)
any(is.na(df$Price))
sum(is.na(df$Price))

library(dplyr)

sum(duplicated(df))
df_w = df %>%
  distinct(.keep_all = TRUE)
dim(df)
dim(df_w)
View(df_w)

df_w = df_w[order(df_w$Date),]
View(df_w)

plot(df_w$Date,df_w$Price, ty='l', col='red', main='World Cocoa Prices'



library(lubridate)
library(dplyr)

#round dates down to week
df_w$month <- floor_date(df_w$Date, "month")

#find mean sales by month
df_m = df_w %>%
  group_by(month) %>%
  summarize(Price = last(Price))

dim(df_m)
View(df_m)

plot(df_m$month,df_m$Price, ty='l', col='blue')

library(strucchange)

str(df_m)
df_m$month <- as.Date(df_m$month, format= '%Y-%m-%d')

bp = breakpoints(df_m$Price~1)
summary(bp)

plot(df_m$Price, ty='l', col='blue')
abline(v=98, col='red')
abline(v=257, col='red')
abline(v=337, col='red')
abline(v=456, col='red')


str(df_m)
breakdates(bp)
View(df_m)
??decompose

#CONVENTIONAL TIME SERIES DECOMPOISTION
df_ts = ts(df_m$Price, start=c(1979,12), end=c(2024,7),12)
plot(df_ts)
m = decompose(df_ts,type = c("multiplicative"))
plot(m)

plot(m$seasonal[1:12], ty='l')


a = decompose(df_ts,type = c("additive"))
plot(a)

plot(a$seasonal[1:12], ty='l')

#HOLT WINTERS MODEL
#FITTING
hw <- HoltWinters(df_ts)
plot(hw)
plot(fitted(hw)

#FORECASTING
p <- predict(hw, 12, prediction.interval = TRUE)
plot(hw, p)

#FITTING AN ARIMA MODEL TO DATA ON COCOA

#DETERMINING WHETHER THE SERIES IS STATONALRY OR NOT
library(tseries)
adf.test(df_m$Price)
adf.test(diff(df_m$Price))# d=1
#STEP 1 MODEL IDENTIFICATION (p,d,q)
acf(diff(df_m$Price))# q=1

pacf(diff(df_m$Price))# p=1

#STEP 2 PARAMETER ESTIMATION
fit_coco = arima(df_m$Price, order=c(1,1,1))
fit_coco
names(fit_coco)
#STEP 3 DIAGNOSTIC CHECKING
plot(fit_coco$residuals)
acf(fit_coco$residuals)
Box.test(fit_coco$residuals)

#STEP 4 FORECASTING
library(forecast)
forc_coco = forecast(fit_coco,12)
plot(forc_coco, 25)
forc_coco

#SEASONAL ARIMA CALLED SARIMA

#STEP 1 MODEL IDENTIFICATION (p,d,q)
acf(diff(diff(df_m$Price),12))# q=0, Q=1

pacf(diff(diff(df_m$Price),12),40)# p=0, P=2
??arima
#STEP 2 PARAMETER ESTIMATION
fit_coco_s = arima(df_m$Price, order=c(1,1,1), seasonal = list(order=c(2,1,1),period=12))
fit_coco_s
names(fit_coco)
#STEP 3 DIAGNOSTIC CHECKING
plot(fit_coco_s$residuals)
acf(fit_coco_s$residuals)
Box.test(fit_coco_s$residuals)

#STEP 4 FORECASTING
library(forecast)
forc_coco_s = forecast(fit_coco_s,12)
plot(forc_coco, 25)
forc_coco

#FITTING A PROPHET MODEL TO DATA
library(prophet)
names(df_m)
names(df_m) = c('ds','y')
names(df_m)
str(df_m)
m <- prophet(df_m)

future <- make_future_dataframe(m, periods = 12)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)










