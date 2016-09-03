###################################################################################################################
#Analyzed the 32 years weekly and monthly historical crude oil future prices (1980-2012) and predicted crude oil future price via ARIMA model. Discovered the seasonal pattern of weekly-based price, and Markov-chain like of monthly-based price. Model validated by checking the residual pattern (white noise) and testing data error. Coded by R.                                                                                        
###################################################################################################################


oil=read.csv("C://Users/Rain/Desktop/RCLC1w.csv") ##load the csv file on my computer
attach(oil) ##attach the data
ts.plot(Price) ## check timeseries plot of future price
logprice=log(Price) ## The time series graph of the raw data had time varying variance. So I use log transformation to pretreat the data.
ts.plot(logprice)
##Draw acf and pacf plot
par(mfrow=c(1,3))  
ts.plot(logprice)
acf(logprice)
pacf(logprice)
library('fUnitRoots')
 adfTest(logprice,type="ct") ##unit root test, the results confirmed the existence of unit root and therefore considered differencing the data (name as: dp). Dp was stationary.
 dp=diff(logprice)
 ##Draw acf and pacf plot of DP
 par(mfrow=c(1,3))
 ts.plot(dp)
 acf(dp)
 pacf(dp)
 ## unit root test again, result showed no unit root
 adfTest(dp,type='c')
 
 library('TSA')
 eacf(dp) ##Use eacf table to check the arima model parameters p=3, q=3
 t.test(dp) ## test if the true mean is 0. one way of testing stationary
 est=arima(logprice,order=c(3,1,3)) ##build arima model
est
tsdiag(est,gof.lag=100) ##check the residuals, found there was a clear pattern.

##add a yearly parttern to our original model
 logprice=ts(logprice,frequency=53)
 dsp=diff(logprice,lag=53)
 ts.plot(dsp)
 acf(dsp,lag.max=200)
 pacf(dsp,lag.max=200)
 adfTest(dsp)
 eacf(dsp,ar.max=15,ma.max=15)
 t.test(dsp)
 ##The new seasonal arima model
 est2=arima(logprice,order=c(4,1,3),seasonal=list(order=c(0,1,1),period=53))
 est2
 ## The residuals' pattern was reduced significantly
 T=length(logprice)
  resid=est2$resid[1:T-1]
  lagresid=est2$resid[2:T]
  plot(resid,lagresid,main="lagged residual plot") 
 
 
 
