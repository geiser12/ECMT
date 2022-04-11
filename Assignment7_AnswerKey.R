# ECMT 674: Economic Forecasting, Assignment 7 Solution Code
# This answer key is only a suggested solution, there are many ways to write the code

# Clear the Workspace
rm(list=ls())

# Install the packages
Packages <- c("Quandl")
# install.packages(Packages, dependencies = TRUE)
invisible(update.packages(Packages, dependencies = TRUE))
invisible(lapply(Packages, library, character.only = TRUE))

#===============================================================================================================================================================

# Q1. Import and Plot data (quarterly frequency)

# We are interested in predicting real GDP growth in the US with interest rate spread

# Real GDP (rgdp): Quandl code -> FRED/GDPC1 
# I Spread (sprd): Quandl code -> FRED/T10Y2Y

# Do not run the following line more that 50 times a day
hw7 <- Quandl(c('FRED/GDPC1','FRED/T10Y2Y'), collapse='quarterly', type="xts")

# Rename the variables
names(hw7)[1] <- "rgdp"
names(hw7)[2] <- "spread"
sum(with(hw7, is.na(rgdp)))
sum(with(hw7, is.na(spread)))
hw7 <- na.omit(hw7)  # 1976 Q2 ~ 2021 Q3

# Plot the raw data
dev.new(noRStudioGD = TRUE)
par(mfrow = c(2,1))
plot(hw7[,1], main = "Real Gross Domestic Product", xlab = "Quarter", ylab = "Bills of Chained 2005 Dollars", type = "l", col = "blue", yaxis.left = FALSE)
plot(hw7[,2], main = "Interest Rate Spread", xlab = "Quarter", ylab = "Percent", type="l", col="blue", yaxis.left = FALSE)
# graphics.off()

#===============================================================================================================================================================
# Q2. Write a simple linear regression model
# Data transformation (%)

# Simple definition of growth rate vs. log-difference approximation
ld_rgdp <- na.exclude(diff(log(hw7$rgdp))  * 100)
T = dim(hw7)[1]
spread <- hw7[2:T,2]

# Plot the transformed data
dev.new(noRStudioGD = TRUE)
par(mfrow = c(2,1))
plot(ld_rgdp, main = "Real Gross Domestic Product", xlab = "Quarter", ylab = "Growth Rate", type = "l", col = "blue", yaxis.left = FALSE)
plot(spread, main = "Interest Rate Spread", xlab = "Quarter", ylab = "Percent", type="l", col="blue", yaxis.left = FALSE)
#graphics.off()

dev.new(noRStudioGD = TRUE)
plot(spread, type = "l", main="Term Spread", ylab = "Percentage", xlab="Dates", col = "red", yaxis.right = FALSE)
lines(ld_rgdp, type = "l",col = "blue",main="Real GDP Growth Rate")
addLegend('bottomright', c('Term Spread','Real GDP Growth Rate'), pch = c(1,-1), lty = c(1,1), col = c("red", "blue"))
#graphics.off()

# Regression by full sample
full_sample_reg <- lm(ld_rgdp ~ spread)
summary(full_sample_reg)

# 1pp increase in the spread is associated with a 0.05pp decline in the quarterly GDP growth. The effect is not statistically significant at any commonly used significance level.
# However, the magnitude seems to be non-trivial.
#===============================================================================================================================================================

# Q3. In-sample fit


# One argument here could be that the coefficient is not significant so it is not fitting well.
# Another argument would be on the R^2, which is very low. Essentially, this omdel is explaining 0.12% of the total variation in the real GDP growth.


# Another approach would be the visualization, which basically shows that the fitted value is flat, though we are not sure if this is mainly driven by COVID or not


# Plot the fitted and actual values
dev.new(noRStudioGD = TRUE)
plot(coredata(ld_rgdp), col = "blue", type='l', lty=1,main = 'In-sample Fit', ylab='Percentage Points', xlab="dates")
lines(coredata(full_sample_reg$fitted.values), type='l', lty=2)
legend('bottomleft', c('True value','Fitted value'), col=c("blue","black"), lty = 1:2)
# graphics.off()

#===============================================================================================================================================================
# Q4. Estimate model by using a subsample

# Run the regression for a sub-sample
# Regression by full sample
sub_sample_reg <- lm(window(ld_rgdp, end = "2011-12-31") ~ window(spread,end = "2011-12-31"))
summary(sub_sample_reg)
sub_sample_mean <- lm(window(ld_rgdp, end = "2011-12-31") ~ 1)

# The sub-sample results show that the fit is not driven by COVID
oos_predict_1 = sub_sample_reg$coef[1] + sub_sample_reg$coef[2]*window(spread, start = "2012-01-31")
oos_predict_2 = sub_sample_mean$coef[1] + 0*window(spread, start = "2012-01-31")

realization = window(ld_rgdp, start = "2012-01-31")

# Plot the fitted and actual values
dev.new(noRStudioGD = TRUE)
plot(realization, col = "blue", type='l', lty=1, main = 'OOS Prediction', ylab='Percentage Points', xlab="dates")
lines(oos_predict_1, col = "red",type='l', lty=2)
lines(oos_predict_2, type='l', lty=3)
addLegend('bottomleft', c('True value','Model with Term Spread', 'Model with Mean'), col=c("blue","red","black"), lty = 1:3)

# It is a very similar prediction
fe1 = realization-oos_predict_1
fe2 = realization-oos_predict_2

MSFE = c(mean(fe1^2), mean(fe2^2))

# The model with the term spread is slightly better, but is it significant?

reg_eval1 = lm(fe1^2-fe2^2~1)
summary(reg_eval1)

# There is no statistial evidence that models are different in their predictive performance
#===============================================================================================================================================================

# Q5. Evaluate the models up until the end of 2019,
# Would the model prediction be better than the prediction with a sample mean? 


# The sub-sample results show that the fit is not driven by COVID
oos_predict_1s = sub_sample_reg$coef[1] + sub_sample_reg$coef[2]*window(spread, start = "2012-01-31", end = "2019-12-31")
oos_predict_2s = sub_sample_mean$coef[1] + 0*window(spread, start = "2012-01-31", end = "2019-12-31")

realization_s = window(ld_rgdp, start = "2012-01-31", end = "2019-12-31")

# Plot the fitted and actual values
dev.new(noRStudioGD = TRUE)
plot(realization_s, col = "blue", type='l', lty=1, main = 'OOS Prediction - pre-COVID', ylab='Percentage Points', xlab="dates")
lines(oos_predict_1s, col = "red",type='l', lty=2)
lines(oos_predict_2s, type='l', lty=3)
legend('bottomleft', c('True value','Model with Term Spread', 'Model with Mean'), col=c("blue","red","black"), lty = 1:3)

# It is a very similar fit.

fe1 = realization-oos_predict_1
fe2 = realization-oos_predict_2

MSFE = c(mean(fe1^2), mean(fe2^2))

# The model with the term spread is slightly better, but is it significant?

reg_eval1 = lm(fe1^2-fe2^2~1)
summary(reg_eval1)

# There is no statistial evidence that models are different in their predictive performance