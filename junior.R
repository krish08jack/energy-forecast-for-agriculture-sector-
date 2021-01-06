# Read file

library(forecast)
library(readxl)
my_data <- read_excel(file.choose())
my_data <- read_excel("Demand.xlsx")
frequency(my_data)
str(my_data)
head(my_data)
sum(is.na(my_data))
summary(my_data)
ts(my_data, frequency = 1, start=c(1971))
attributes(my_data)
plot(my_data$Agriculture)
# Log transform
my_data <- log(my_data$Agriculture)
plot(my_data)

library(forecast)
model <- auto.arima(my_data)
attributes(model)
# ACF and PACF plots
acf(model$residuals, main = 'Correlogram')
pacf(model$residuals, main = 'Partial Correlogram' )
# Ljung-Box test
Box.test(model$residuals, lag=20, type = 'Ljung-Box')
# Residual plot
hist(model$residuals,
     col = 'red',
     xlab = 'Error',
     main = 'Histogram of Residuals',
     freq = FALSE)
lines(density(model$residuals))
# Forecast
f <- forecast(model, 10)
library(ggplot2)
autoplot(f)
accuracy(f)
