library(TSA)
library(forecast)
library(tidyr)
library(tidyverse)
library(pracma)
library(tseries)


data = read.csv("team2.csv")



names(data)[1] = "Hour"
names(data)[2] = "Temperature"

plot(data, main="Temperature VS Time", ylab=" Temperature(°C)",xlab="Time(Hour)")


Projectdiff = diff(data$Temperature,difference=1)
plot(Projectdiff,type = 'l', main="Temperature VS Time(First Difference)", ylab=" Temperature(°C)",xlab="Time(Hour)")
seriesd1 = as.ts(Projectdiff)
plot(seriesd1)
acf(seriesd1) # exponential decay possibly ma(3) or ARMA(pq)
acf(seriesd1,lag.max=10,plot=FALSE)
pacf(seriesd1) # 0 K>P ar(2)
eacf(seriesd1) # Possibly MA(3), AR(2), ARMA(1,3), ARMA(2,2),...


series<-as.ts(data$Temperature)
acf(series) # exponential decay possibly AR(p) or ARMA(pq)
acf(series,lag.max=10,plot=FALSE)
pacf(series) # 0 K>P ar(2)
eacf(series) # Possibly MA(1), AR(1), ARMA(1,1), ARMA(1,2),...



#arima 211
model_aim211<-arima(series,order=c(2,1,1))
model_aim211
res211<-residuals(model_aim211)
acf(res211, main="ACF of Residuals arima 211")
y211=ARMAacf(ma=c(0.2032),ar=c(0.9215,-0.2623),lag.max=25)
plot(x=1:25,y211[-1],ylab=expression(rho[k]),xlab="k",type="h", main= "ACF of ARIMA(2,1,1)")
sd(y013)


# TRy ARMA(013)
model_aim013<-arima(series,order=c(0,1,3))
model_aim013
res013<-residuals(model_aim013)
acf(res013, main="ACF of Residuals arima 013")
y013=ARMAacf(ma=c(1.214,0.6745,0.2054),lag.max=25)
plot(x=1:25,y013[-1],ylab=expression(rho[k]),xlab="k",type="h", main= "ACF of ARIMA(0,1,3)")
sd(y013)


# TRy ARMA(210)
model_aim210<-arima(series,order=c(2,1,0))
model_aim210
res210<-residuals(model_aim210)
acf(res210, main="ACF of Residuals arima 210")
y210=ARMAacf(ar=c(1.0936,-.3976),lag.max=25)
plot(x=1:25,y210[-1],ylab=expression(rho[k]),xlab="k",type="h", main= "ACF of ARIMA(2,1,0)")
sd(y210)

# TRy ARMA(113)
model_aim113<-arima(series,order=c(1,1,3))
model_aim113
res113<-residuals(model_aim113)
acf(res113, main="ACF of Residuals arima 113") 
y113=ARMAacf(ma=c(0.6094  ,0.1778  ,0.0163), ar=c(0.5161),lag.max=25)
plot(x=1:25,y113[-1],ylab=expression(rho[k]),xlab="k",type="h", main= "ACF of ARIMA(1,1,3)")
sd(y113)

# TRy ARMA(212)
model_aim212<-arima(series,order=c(2,1,2))
summary(model_aim212)
res212<-residuals(model_aim212)
acf(res212, main="ACF of Residuals arima 212")
y212=ARMAacf(ma=c(-0.5746  ,-0.4082),ar=c(1.6371,-0.6862),lag.max=25)
plot(x=1:25,y212[-1],ylab=expression(rho[k]),xlab="k",type="h", main= "ACF of ARIMA(2,1,2)")
sd(y212)


##############################################################################
# model diagnostic
par(mfrow = c(2, 2))
hist(res211)
qqnorm(res211, main = "QQ Plot for ARIMA(2,1,1)")
qqline(res211, col = "steelblue", lwd = 2)
plot(res211, main= "Residual vs Time")
plot(x=fitted(model_aim211), y=res211)
##################################################################

par(mfrow = c(2, 2))
hist(res210)
qqnorm(res210, main = "QQ Plot for ARIMA(2,1,0)")
qqline(res210, col = "steelblue", lwd = 2)
plot(res210, main= "Residual vs Time")
plot(x=fitted(model_aim210), y=res210)
##################################################################
par(mfrow = c(2, 2))
hist(res013)
qqnorm(res013, main = "QQ Plot for ARIMA(0,1,3)")
qqline(res013, col = "steelblue", lwd = 2)
plot(res013, main= "Residual vs Time")
plot(x=fitted(model_aim013), y=res013)

##################################################################

par(mfrow = c(2, 2))
hist(res113)
qqnorm(res113, main = "QQ Plot for ARIMA(1,1,3)")
qqline(res113, col = "steelblue", lwd = 2)
plot(res113, main= "Residual vs Time")
fitted <- fitted(model_aim113)
plot(x=fitted(model_aim113), y=res113)
##################################################################

par(mfrow = c(2, 2))
hist(res212)
qqnorm(res212, main = "QQ Plot for ARIMA(2,1,2)")
qqline(res212, col = "steelblue", lwd = 2)
plot(res212, main= "Residual vs Time")
fitted <- fitted(model_aim212)
plot(x=fitted(model_aim212), y=res212)
##################################################################






# Load the necessary library
library(forecast)

# Set the seed for reproducibility

# Define the ARIMA parameters
ar_params <- c(1.0936, -0.3976)

# Initialize an empty list to store the simulated series
simulated_series_list <- list()

n=100
sequence <- 1:n

set.seed(2024)
# Generate 20 ARIMA(2,1,0) simulations
for (i in 1:n) {
  simulated_series <- arima.sim(n = 243, model=list(order=c(2,1,0), ar=ar_params))
  simulated_series <- simulated_series + 20.59 - simulated_series[1]  # Shift the series to start from 20.59
  simulated_series_list[[i]] <- simulated_series
}

# # Define a vector of colors
# # Define a vector of colors
colors <- colorRampPalette(rainbow(7))(n)

# Plot the simulated series on the same plot
plot(simulated_series_list[[1]], type="n", ylim=c(-60,100), main="20 Simulated ARIMA(2,1,0) Series")  # Set up the plot
for (i in 1:n) {
  lines(simulated_series_list[[i]], col=colors[i])
}
# create a color sequen
# add a legend
# legend("bottomleft", legend = sequence, fill = colors, title = "Sequence", ncol = length(sequence), cex = 0.8)
abline(h=45)




#========================================Simulations====================================================``
set.seed(2024)
for (i in 1:n) {
  simulated_series <- arima.sim(n = 243, model=list(order=c(2,1,0), ar=ar_params))
  simulated_series <- simulated_series + 20.59 - simulated_series[1]  # Shift the series to start from 20.59
  simulated_series_list[[i]] <- simulated_series
}

#========================================creating dataframe====================================================``
sequence <- 1:244
dataframe_list <- list()
for (i in 1:length(simulated_series_list)) {
  df <- data.frame(Column1 = sequence, Column2 = unlist(simulated_series_list[[i]]))
  dataframe_list[[i]] <- df
}
##============================================making boolean lists==============================================
bool_list <- list()
for (i in 1:length(dataframe_list)) {
  filtered_df <- subset(dataframe_list[[i]], Column2 > 45)
  sequence <- rle(diff(filtered_df$Column1) == 1)$lengths
  bool_list[[i]] <- any(sequence >= 72)
}
bool_df <- data.frame('BoolValues' = unlist(bool_list))

#===============================================calculating probs===============================================
true_count <- sum(bool_df$BoolValues)
probability <- true_count / n
print(probability)






