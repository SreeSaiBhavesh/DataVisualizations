
data("AirPassengers")
sum(is.na(AirPassengers))
frequency(AirPassengers)
plot.ts(AirPassengers, main = 'Air Passengers Vs Time (20BCE0348)')
abline(reg = lm(AirPassengers~time(AirPassengers)))
cycle(AirPassengers)
par(mfrow=c(3,3))
plot(AirPassengers)
title('20BCE0348')
plot(log(AirPassengers))
title('Log of data (20BCE0348)')
plot(diff(log(AirPassengers)))
title('20BCE0348')
# Trend
plot(aggregate(AirPassengers, FUN = mean))
title('20BCE0348')
boxplot(AirPassengers~cycle(AirPassengers))   
title('20BCE0348')
plot(decompose(AirPassengers))
title('Decomposed Plot(20BCE0348)')

par(mfrow=c(2,2))
acf(AirPassengers)
#title('20BCE0348')
acf(log(AirPassengers))
#title('20BCE0348')
pacf(diff(log(AirPassengers)))
#title('20BCE0348')
acf(diff(log(AirPassengers)))
title('20BCE0348')
fit <- arima(log(AirPassengers), order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
fit
pred <- predict(fit, n.ahead = 10*12)
pred1 <- round(2.178^pred$pred,0)
pred1

# plot this model
# line type (lty) can be specified using either text ("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash") or number (0,1,2,3,4,5,6).
ts.plot(AirPassengers,pred1,log="y",lty=c(1,3))
title('Model Plot (20BCE0348)')
# Comparing predicted values with original values
data1 <- head(pred1,12)
data1
# Predicted Values
predicted_1960 <- round(data1)
predicted_1960
# original
original_1960 <- tail(AirPassengers,12)
original_1960
# Lets Test this model we are going to take a dataset till 1959, and then we predict value of 1960, 
# then validate that 1960 from already existing value we have it in dataset.
# Recreate model till 1959
datawide <- ts(AirPassengers, frequency = 12, start = c(1949,1),end = c(1959,12))
datawide
# Create Model
fit1 <- arima(log(datawide),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
pred <- predict(fit1,n.ahead = 10*12)
pred1 <- 2.718^pred$pred
pred1
data11=round(head(pred1,12),0)
data22=round(tail(AirPassengers,12),0)
plot(data11,col="red",type = "l")
lines(data22,col="blue")
title('Model Creation (20BCE0348)')
# Check normality using Q-Q plot

#qqnorm is a generic function that default method of which produces a normal QQ plot of the values
#in y. qqline adds a line to a "theoritical", by default normal, quantile-quantile plot which passes through the probs quantiles, by default the first and third quartiles.
qqnorm(residuals(fit))
qqline(residuals(fit))
