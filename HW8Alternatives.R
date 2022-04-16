# Example for the homework
rm(list = ls())
library(Quandl)
#library(lubridate)
library(lmtest)
library(sandwich)
#library(forecast)
Quandl.api_key('aAzfWxRPhhLpNAtLzydb')

# Obtain data from FRED and make sure there are no missing values
hw8 <- Quandl(c('FRED/GDPC1','FRED/T10Y2Y'), collapse='quarterly', type="xts")

# Rename the variables
names(hw8)[1] <- "rgdp"
names(hw8)[2] <- "spread"
hw8 <- na.omit(hw8)  # 1976 Q2 ~ 2021 Q3

# Obtain the growth rates and make sure everything is aligned with respect to time t.
ld_rgdp <- na.exclude(diff(log(hw8$rgdp))  * 100)
T = dim(hw8)[1]
spread <- hw8[2:T,2]

m2_ld_rgdp = window(ld_rgdp, end = "2011-12-31")
m2_spread = window(spread,end = "2011-12-31")
T2 = dim(m2_ld_rgdp)[1]
# Consider the situation with a quarterly growth rate

for (h in c(1,4,8)){
  reg = lm(m2_ld_rgdp[(1+h):T2,]~m2_spread[1:(T2-h)])
  cat("Current forecasting horizon: ", h)
  print(summary(reg))
  print(cbind(head(m2_ld_rgdp[(1+h):T2,],10),head(m2_spread[1:(T2-h),],10)))
}


for (h in c(1,4,8)){
  
  # One way to define the left hand side variable
  ld_rgdp_alt <- na.exclude((log(hw8$rgdp)-log(lag(hw8$rgdp,h)))*100)
  ld_rgdp_alt <- ld_rgdp_alt[2:dim(ld_rgdp_alt)[1],]
  spread_alt <- hw8[(h+1):(T-h),2]
  
  # Another way to define the left hand side variable --- be careful here, 
  # since if you go this route, you might need to be careful about the dates
  
  csum_ld_rgdp = 0*m2_ld_rgdp
  for (i in (1:(T2-h))){
    csum_ld_rgdp[i,] =sum(m2_ld_rgdp[(i+1):(i+h),])
  }
  cat("Current forecasting horizon: ", h)
  print(cbind(head(m2_ld_rgdp,10),head(csum_ld_rgdp,10)))
  print(cbind(head(ld_rgdp_alt,10),head(csum_ld_rgdp,10)))
  reg = lm(csum_ld_rgdp[1:(T2-h),]/h~m2_spread[1:(T2-h),])
  print(summary(reg))
  print(coeftest(reg,vcov. = NeweyWest(reg, lag = round(0.75*T2^(1/3)))))
}

