rm(list = ls())
#install.packages("Quandl",dependencies=TRUE)
library(Quandl)
Quandl.api_key("UMo9bYuaDpyAy55mMfGS")
Sys.setenv("LANGUAGE"="En")
Sys.setlocale("LC_ALL", "English")
library("rstudioapi")
setwd(dirname(getActiveDocumentContext()$path)) 


# http://econbrowser.com/archives/2015/01/whats-driving-the-price-of-oil-down-2
# Obtain data from FRED

macrodata=Quandl(c('FRED/DTWEXM', "FRED/DCOILWTICO","FRED/WGS10YR"), collapse="weekly",start_date="1988-12-26", end_date="2017-12-31", type="raw")
wti=ts(macrodata$`FRED.DCOILWTICO`,end=c(2017,12),frequency=365.25/7)
dollar=ts(macrodata$`FRED.DTWEXM`,end=c(2017,12),frequency=365.25/7)
bond=ts(macrodata$`FRED.WGS10YR `,end=c(2017,12),frequency=365.25/7)
data=read.csv("Copper Futures Historical Data.csv",header=TRUE)
copper=ts(data$Price,end=c(2017,12),frequency=365.25/7)
plot(wti,main="Crude Oil Prices: West Texas Intermediate",type="l",col="blue",xlab="date",ylab="Dollars per Barrel")
plot(dollar,main="Trade Weighted U.S. Dollar Index",type="l",col="blue",xlab="date",ylab="Index Mar 1973=100")
plot(bond,main="10-Year Treasury Constant Maturity Rate",type="l",col="blue",xlab="date",ylab="Percent")
plot(copper,main="Copper Futures",type="l",col="blue",xlab="date",ylab="Dollars")

#Compute Two Growth Rates and Plot
log_wti=100*diff(log(wti)) # log transformation
g_wti=100*diff(wti)/lag(wti,-1)

data_combine=cbind(log_wti,g_wti)
plot(data_combine,main="Crude Oil Price Growth Rate",plot.type=c("single"),xlab = "date",ylab="growth Rate",col=c("blue","red"),lty=1:2)
     legend("bottomright",legend=c("log transformation","definition"),col=c("blue","red"),lty=1:2)

     
log_copper=100*diff(log(copper))     
g_copper=100*diff(copper)/lag(copper,-1)
data_combine=cbind(log_copper,g_copper)
plot(data_combine,main="Copper Futures Growth Rate",plot.type=c("single"),xlab = "date",ylab="growth Rate",col=c("blue","red"),lty=1:2)
legend("bottomright",legend=c("log transformation","definition"),col=c("blue","red"),lty=1:2)
     
log_dollar=100*diff(log(dollar))
g_dollar=100*diff(dollar)/lag(dollar,-1)
data_combine=cbind(log_dollar,g_dollar)
plot(data_combine,main="Trade Weighted U.S. Dollar Growth Rate",plot.type=c("single"),xlab = "date",ylab="growth Rate",col=c("blue","red"),lty=1:2)
legend("bottomright",legend=c("log transformation","definition"),col=c("blue","red"),lty=1:2)


# Regression
log_bond=100*diff(log(bond))
fullmodel=lm(log_wti~log_dollar+log_bond+log_copper)
summary(fullmodel)     


# For the exam

model1 = lm(log_wti~log_dollar)
summary(model1) 

model2 = lm(log_wti~log_copper)
summary(model2) 


# In-sample fit
fit=ts(fullmodel$fitted.values,end=c(2017,12),frequency=365.25/7)
data_combine=cbind(log_wti,fit)
plot(data_combine,main="WTI growth rate",plot.type=c("single"),xlab = "date",ylab="growth Rate",col=c("blue","red"),lty=1:2)
legend("bottomright",legend=c("data","in-sample fit"),col=c("blue","red"),lty=1:2)

# Out of sample fit
pastmodel=lm(window(log_wti,end=c(2014,12))~window(log_dollar,end=c(2014,12))+window(log_bond,end=c(2014,12))+window(log_copper,end=c(2014,12)))
summary(pastmodel)
predict=pastmodel$coef[1]+window(log_dollar,start=c(2015,01))*pastmodel$coef[2]+window(log_bond,start=c(2015,01))*pastmodel$coef[3]+window(log_copper,start=c(2015,01))*pastmodel$coef[4]
data_combine_fit=cbind(window(fit,start=c(2015,01)),predict)
plot(data_combine_fit,main="Predicted WTI growth rate",plot.type=c("single"),xlab = "date",ylab="growth rate",col=c("blue","red"),lty=1:2)
legend("bottomright",legend=c("in-sample fit","out of sample fit"),col=c("blue","red"),lty=1:2)

# Prediction
predict_demand=matrix(0,length(window(log_wti,start=c(2015,01))),1)
predict_demand=wti[length(window(wti,end=c(2014,12)))]*(1+predict[1]/100)
for (i in 2:length(window(log_wti,start=c(2015,01)))) {
  predict_demand[i]=predict_demand[i-1]*(1+predict[i]/100)
}
predict_demand=ts(predict_demand,end=c(2017,12),freq=365.25/7)
data_combine_fit_outsample=cbind(window(wti,start=c(2015,01)),predict_demand)
plot(data_combine_fit_outsample,main="Crude Oil Prices: West Texas Intermediate",plot.type=c("single"),xlab = "date",ylab="Dollars per Barrel",col=c("blue","red"),lty=1:2)
legend("bottomright",legend=c("data","demand"),col=c("blue","red"),lty=1:2)