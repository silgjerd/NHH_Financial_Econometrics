library(stats)
library(quantmod)
library(caTools)
library(tidyverse)
library(stargazer)
library(gvlma)
library(lindia)
library(car)
library(lmtest)
library(DescTools)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# PART 1

data <- read_csv("equity_premium.csv",
                 col_types = cols(date = col_date(format = "%Y%m")))

# New variables
data <- mutate(data, equity.premium = lead(Delt(sp500) - rf)) #equity premium t+1
data <- mutate(data, dpy = log(dividends12month) - log(lag(sp500, 1))) #dpy
data <- mutate(data, drs = yieldBAA - yieldAAA) #drs
names(data)[4] <- "bm" #bm
names(data)[8] <- "ntis" #ntis

data <- na.omit(data) #removing first and last row containing NAs due to lagging and leading
data <- data[,c("date", "equity.premium", "dpy", "bm", "drs", "ntis")] #extract relevant variables

# Winsorizing
data <- as.data.frame(data)
for (i in 2:6) {data[,i] <- Winsorize(data[,i], probs = c(.005, .995))}

# Train test split
split_date <- as.Date("1980-01-01")
train <- subset(data, date < split_date)
test <- subset(data, date >= split_date)

# Models 
m1 <- lm(equity.premium ~ dpy, data = train)
m2 <- lm(equity.premium ~ bm, data = train)
m3 <- lm(equity.premium ~ drs, data = train)
m4 <- lm(equity.premium ~ ntis, data = train)
mb <- lm(equity.premium ~ 1, data = train) #mean benchmark model


# Function to check if heteroskedasticity is present in the data...
# ...and get robust standard errors if found, NULL if homoskedastic
checkAndGetHCSE <- function(model, sig_level) { #takes two arguments: model, signifiance level to test for
  if (bptest(model)[[4]] < sig_level) { #if p-value less than (reject h0 and find heteroskedasticity)
    cat("Found heteroskedasticity, returning robust standard errors. p-value:", round(bptest(model)[[4]], 6))
    return(coeftest(model, vcov = hccm)[,2]) #return the hcse
  } else {
    cat("Did not find heteroskedasticity, fail to reject H0, returning NULL. p-value:", round(bptest(model)[[4]], 6))
    return(NULL)
  }
}

# Heteroskedasticity consistent standard errors
m1.hcse <- checkAndGetHCSE(m1, 0.01)
m2.hcse <- checkAndGetHCSE(m2, 0.01)
m3.hcse <- checkAndGetHCSE(m3, 0.01)
m4.hcse <- checkAndGetHCSE(m4, 0.01)

data %>%
  select(-date) %>%
  cor %>%
  round(2)


# Stargazer output
stargazer(m1, m2, m3, m4, mb,
          se = list(m1.hcse, m2.hcse, m3.hcse, m4.hcse, NULL),
          type = "text",
          keep.stat = c("rsq", "n"),
          report = "vc*stp")

# Getting predictions
pred <- tibble("m1" = predict(m1, test),
               "m2" = predict(m2, test),
               "m3" = predict(m3, test),
               "m4" = predict(m4, test),
               "mb" = predict(mb, test))

# Computing errors
errors <- test$equity.premium - pred #actual value - predicted value

error_stats <- tibble("MSE" = colMeans(errors^2), #computing error statistics
                      "MAE" = colMeans(abs(errors)),
                      "RMSE" = sqrt(colMeans(errors^2)))
error_stats$avg <- rowMeans(error_stats) #average

comparison_stats <- tibble("MSE" = error_stats$MSE - error_stats$MSE[5], #comparison = X - B (predictor error - benchmark error)
                           "MAE" = error_stats$MAE - error_stats$MAE[5],
                           "RMSE" = error_stats$RMSE - error_stats$RMSE[5])
comparison_stats$avg <- rowMeans(comparison_stats) #average
comparison_stats <- comparison_stats[-5,] #removing benchmark


# # Plot comparison stats
# par(mfrow=c(3,1))
# plot(comparison_stats$MSE[-5], type = "o")
# plot(comparison_stats$MAE[-5], type = "o")
# plot(comparison_stats$RMSE[-5], type = "o")
# 
# ggplot(error_stats, aes())+
#   geom_point(aes(1:5, MSE), col = "red")+
#   geom_point(aes(1:5, MAE), col = "blue")+
#   geom_point(aes(1:5, RMSE), col = "green")+
#   geom_line(aes(1:5, avg), col = "black")+
#   theme_classic()
# 
# ggplot(comparison_stats, aes())+
#   geom_point(aes(1:4, MSE), col = "red")+
#   geom_point(aes(1:4, MAE), col = "blue")+
#   geom_point(aes(1:4, RMSE), col = "green")+
#   geom_line(aes(1:4, avg), col = "black")+
#   theme_classic()
# 
# plot(cbind(pred, test$equity.premium)) #scatterplot all predictions



# PART 2

# Rolling window one month strategy
mon_predictions <- tibble() #empty df for appending results

for (num_years in c(10, 20, 30)) { #for each number of years rolling window
  
  current_date <- as.Date("1980-01-01") #start date (resets each iteration of for loop)
  repeat { #repeat loop adding one month each iteration until it reaches last month
    
    current_subset <- subset(data, date >= current_date) #getting the current subset
    current_subset <- current_subset[1:(num_years * 12 + 1),] #next 10, 20 or 30 years
    
    current_m3 <- lm(equity.premium ~ drs, data = head(current_subset, nrow(current_subset)-1)) #estimate model (without most recent month)
    current_prediction_m3 <- predict(current_m3, tail(current_subset, 1))[[1]] #get current prediction for last month
    
    current_m4 <- lm(equity.premium ~ ntis, data = head(current_subset, nrow(current_subset)-1)) #estimate model (without most recent month)
    current_prediction_m4 <- predict(current_m4, tail(current_subset, 1))[[1]] #get current prediction for last month
    
    mon_predictions <- bind_rows(mon_predictions, c("num_years" = num_years,
                                                    "pred" = current_prediction_m4,
                                                    "actual_ep" = tail(current_subset$equity.premium, 1))) #append prediction and actual equity premium
    
    if (tail(current_subset$date, 1) == tail(data$date, 1)) {break} #if last date, break repeat loop
    current_date <- AddMonths(current_date, 1) #add month to start date
  }
}

# Error stats
mon_predictions$error <- mon_predictions$actual_ep - mon_predictions$pred #error = actual - predicted

mon_error_stats <- mon_predictions %>% #reporting error stats
  group_by(num_years) %>%
  summarise("MSE" = mean(error^2),
            "MAE" = mean(abs(error)),
            "RMSE" = sqrt(mean(error^2)))

mon_error_stats$avg <- rowMeans(mon_error_stats[,c(-1)]) #adding average


# Plot value of SP500 and forecasted
spplotdata <- subset(mon_predictions, num_years == 30)[,c("pred", "actual_ep")] #data to plot

ret2price <- function(retdf, base){ #function to convert return data to price data
  retdf <- as.data.frame(retdf)
  newdf <- tibble(base)
  names(newdf) <- names(retdf)
  for (i in 1:nrow(retdf)) {newdf[i+1,1] <- newdf[i,1] * (1 + retdf[i,1])}
  return(newdf)}

spplotdata_price <- data.frame(ret2price(spplotdata$pred, 100), #using function
                               ret2price(spplotdata$actual_ep, 100))
names(spplotdata_price) <- c("Forecast", "Actual") #column names

ggplot(spplotdata_price, aes()) + #plotting forecasted sp500 value vs actual
  geom_line(aes(1:108, Forecast), col = "blue") +
  geom_line(aes(1:108, Actual), col = "red") +
  theme_classic() +
  labs(x = "Date", y = "Value (based to 100)", title = "Actual vs Forecasted S&P500")









#END ======================




par(mfrow=c(1,1))
plot(subset(mon_predictions, num_years == 30)[c("pred", "actual_ep")], pch = 19)

par(mfrow=c(1,1))
plot(mon_predictions$pred, mon_predictions$actual_ep)



wide_mon_pred <- mon_predictions %>% #spread tidy df to wide for calculations
  spread(num_years, pred_m3)

mon_errors <- wide_mon_pred$actual_ep - wide_mon_pred #actual value - predicted value
mon_errors <- mon_errors[-1] #drop actual ep

mon_error_stats <- tibble("MSE" = colMeans(mon_errors^2, na.rm = T),
                          "MAE" = colMeans(abs(mon_errors), na.rm = T),
                          "RMSE" = sqrt(colMeans(mon_errors^2, na.rm = T)))



plot(wide_mon_pred[,-2])


par(mfrow=c(1,1))
plot(subset(mon_predictions, num_years == 10)[c("pred", "actual_ep")])




# Scatterplot matrix of linear models
par(mfrow = c(2,2))
plot(equity.premium ~ dpy, data = train, pch = 19)
abline(m1, col = "red")
plot(equity.premium ~ bm, data = train, pch = 19)
abline(m2, col = "red")
plot(equity.premium ~ drs, data = train, pch = 19)
abline(m3, col = "red")
plot(equity.premium ~ ntis, data = train, pch = 19)
abline(m4, col = "red")




# EDA
plotdata <- data[,c("equity.premium", "dpy", "bm", "drs", "ntis")]
plot(plotdata, pch = 19, lower.panel = NULL) #scatterplot matrix

par(mfrow = c(2,2))
plot(data$dpy, type = "l")
plot(data$bm, type = "l")
plot(data$drs, type = "l")
plot(data$ntis, type = "l")

round(cor(plotdata), 2)

