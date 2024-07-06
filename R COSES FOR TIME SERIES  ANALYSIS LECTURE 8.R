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

plot(df_w$Date,df_w$Price, ty='l', col='red', main='World Cocoa Prices')



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
abline(v=258, col='red')
abline(v=338, col='red')



str(df_m)
breakdates(bp)


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

p

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
# FITTING A TREND TO THE DATA
df_m$time = 1:length(df_m$ds)
head(df_m)
trend = lm(y~time, data=df_m) # Y = 1166.7 +3.2 t
trend 
names(trend)
plot(df_m$y, ty='l', col='red', main= "Trend in world Cocoa Prices")
lines(trend$fitted.values)
forecast(trend, 536)
forecast(trend, 548)
forecast(trend, 596)

#TO IDENTIFY LONG CYCLES SPECTAL ANALYSIS

library(zoo)
dengue <- read.csv("https://web.stanford.edu/class/earthsys214/data/San_Juan_Training_Data.csv", header=TRUE)
tcases <- zoo(dengue$total_cases, as.Date(dengue$week_start_date))
plot(tcases, xlab="Date", ylab="Total Cases")

## now all cases
acases <- zoo(dengue[,4:7],as.Date(dengue$week_start_date))
plot(acases, xlab="Date", ylab=list("Dengue 1","Dengue 2","Dengue 3","Dengue 4"), main="")

dspect <- spectrum(dengue$total_cases, log="no", spans=c(5,5), plot=FALSE)
delta <- 7/365
specx <- dspect$freq/delta
specy <- 2*dspect$spec
plot(specx[1:100], specy[1:100], xlab="Period (years)", ylab="Spectral Density", type="l")



#SPECTRAL ANANLYSIS WITH COCOA PRICE DATA
dspect <- spectrum(df_m$y, log="no", spans=c(5,5), plot=FALSE)
delta <- 1/12
specx <- dspect$freq/delta
specy <- 2*dspect$spec
plot(specx[1:100], specy[1:100], xlab="Period (years)", ylab="Spectral Density", type="l")

#USING A TREND REMOVED SERIES
dspect <- spectrum(diff(df_m$y), log="no", spans=c(5,5), plot=FALSE)
delta <- 1/12
specx <- dspect$freq/delta
specy <- 2*dspect$spec
plot(specx[1:100], specy[1:100], xlab="Period (years)", ylab="Spectral Density", type="l")

#FITTING ARCH AND GARCH MODELS
plot(fit_coco_s$residuals)
acf(fit_coco_s$residuals)
acf(fit_coco_s$residuals^2)
pacf(fit_coco_s$residuals^2)
summary(fit_coco_s$residuals)

library(forecast)

install.packages('TSA')
library(TSA)

require(FinTS)

install.packages('rmgarch')
require(rmgarch)

spec = ugarchspec()
print(spec)

def.fit = ugarchfit(spec = spec, data = fit_coco_s$residuals)
print(def.fit)
0.38399+0.59418
# Lets chnage GARCH Specs.. Lets use GARCH(1,1)..

garch11.spec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
               variance.model = list(garchOrder = c(1,1), 
               model = "sGARCH"), distribution.model = "norm")

print(garch11.spec)

garch.fit = ugarchfit(garch11.spec, data = fit_coco_s$residuals, fit.control=list(scale=TRUE))
garch.fit

library(rugarch)
# Assuming garch.fit is an object of class 'uGARCHfit'
forecast <- ugarchforecast(garch.fit, n.ahead=12)


plot(garch.fit, which="all")
plot(garch.fit)

library(rugarch)
gspec.ru <- ugarchspec(mean.model=list(
      armaOrder=c(0,0)), distribution="std")
gfit.ru <- ugarchfit(gspec.ru, fit_coco_s$residuals)
gfit.ru
plot(gfit.ru,which="all")
library(fGarch)
gfit.fg <- garchFit(data=fit_coco_s$residuals, cond.dist="std")
coef(gfit.fg)
plot(gfit.fg,which="all")

install.packages("devtools")



library(devtools)
install_github("cran/AutoSEARCH")
library(AutoSEARCH)


library(forecast)

forc <- ugarchforecast(gfit.ru, n.ahead = 12)
# Print the forecast
print(forc)

# Extract the forecasted mean values
forecasted_values <- fitted(forc)
print(forecasted_values)

library(rugarch)

# Assuming gfit.ru is an object of class 'uGARCHfit'
if (class(gfit.ru) == "uGARCHfit") {
    # Forecast the next 12 periods
    forc <- ugarchforecast(gfit.ru, n.ahead = 12)
    
    # Print the forecast object
    print(forc)
    
    # Extract and print the forecasted mean values
    forecasted_values <- fitted(forc)
    print(forecasted_values)
} else {
    stop("gfit.ru is not a valid uGARCHfit object.")
}


eacf(oil.fit$residuals)
m.21<-ugarchfit(spec=garch11.spec,data=fit_coco_s$residuals, out.sample = 100)
plot(m.21,which="all")

forc = ugarchforecast(m.21, data = fit_coco_s$residuals, n.ahead = 30, n.roll =10)

print(forc)
plot(forc, which='all')
plot(forc)






