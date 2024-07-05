# Dataset Import
world_df <- read_csv("D:/Dataset/UK_tourists.csv")
head(world_df)

#Descriptive statistics
str(world_df)
summary(world_df)

sum(is.na(world_df))

world_df <- na.omit(world_df)
sum(is.na(world_df))
str(world_df)

mean(world_df$Charges)
median(world_df$Charges)
mode(world_df$Charges)
range(world_df$Charges)
var(world_df$Charges)
sd(world_df$Charges)
skewness(world_df$Charges)
kurtosis(world_df$Charges)

# EDA
hist(world_df$Year)
hist(world_df$Charges)
gg1 <- ggplot(world_df, aes(x=world_df$Charges)) + 
  geom_density()
gg1 + geom_vline(aes(xintercept=mean(Charges)),
                color="green", linetype="dashed", size=1)

gg2 <- ggplot(world_df, aes(x=world_df$Year)) + 
  geom_density()
gg2 + geom_vline(aes(xintercept=mean(Year)),
                color="green", linetype="dashed", size=1)


#install.packages("tidyverse")
#install.packages("fpp2")
library(tidyverse)
library(fpp2)

#EXponential smoothing
# create training and validation
df_train <- window(world_df$Charges, end = 12000)
df_test <- window(world_df$Charges, start = 100)

# Performing SES on  the raw data
df_ses <- ses(df_train,
              alpha = .2,
              h = 100)
autoplot(df_ses)


# Time series and Residual Analysis for raw data

world_df$Quarter <- NULL
ts_data = ts(world_df, start = c(2010,1), frequency = 12)
plot(ts_data)
#str(df)
compo_ts = decompose(ts_data)
plot(compo_ts)
str(compo_ts)

#install.packages("fUnitRoots")
library(fUnitRoots)
urkpssTest(ts_data, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
ts_stationary = diff(ts_data, differences=1)
plot(ts_stationary)

acf(ts_data, lag.max = 30)

simpletimeseriesseasonal <- ts_data- compo_ts$seasonal
ts_stationary <- diff(simpletimeseriesseasonal, differences=1)

acf(ts_stationary, lag.max=30)
pacf(ts_stationary, lag.max=30)


#Arima 
library(forecast)

time_series <- ts(world_df$Year, world_df$Charges)
time_series
plot(time_series, xlab ="Year",
     ylab ="Charges",
     main ="Charges for travelling for UK tourists",
     col.main ="darkgreen")

fit_arima <- auto.arima(time_series)
forecast(fit_arima, 3)
plot(forecast(fit_arima, 3))

coeftest(fit_arima)
confint(fit_arima)

acf(fit_arima$residuals)

auto.arima(time_series, trace = TRUE)

predict(fit_arima, n.ahead = 5)
future_val <- forecast(fit_arima, h=10, level=c(99.5))
autoplot(future_val)
# Linear Regression model

#install.packages("caTools")
#install.packages("ROCR")

# Loading package
library(caTools)
library(ROCR)
library(moments)

set.seed(424)
# Splitting training and testing data
split_data <- sample.split(world_df$Charges, SplitRatio = 0.8)
train_data <- subset(world_df, split_data == TRUE)
test_data <- subset(world_df, split_data == FALSE)

# Linear regression model
lr_model <- lm(world_df$Charges ~ world_df$Year, data = train_data)
summary(lr_model)
coef(lr_model)


# Correlational Analysis
cor(world_df$Charges, world_df$Year)
cor.test(world_df$Charges, world_df$Year)


# Hypothesis
t.test(world_df$Charges, mu = 5)
t.test(world_df$Charges, world_df$Year)



wilcox.test(world_df$Charges, exact = FALSE)
wilcox.test(world_df$Charges, world_df$Year, exact = FALSE)

