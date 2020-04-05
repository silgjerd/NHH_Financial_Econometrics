library(stats)
library(graphics)
library(quantmod)
library(caTools)
library(TTR)
library(ggthemes)
library(tidyverse)
library(stargazer)
library(gvlma)
library(lindia)
library(DescTools)
library(car)
library(lmtest)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# TASK 1

# Import data
data <- read_csv("MB_Own_1993_assignment.csv")

# Clean data
data <- data[,-1] #drop first column

# Appending variables
data <- data %>%
  mutate(mtbe.win = Winsorize(data$mtbe, probs = c(0.01, 0.99))) %>% #winsorised
  mutate(own.win = Winsorize(data$own, probs = c(0.01, 0.99))) %>% #winsorised
  mutate(D.own.win.above.median = ifelse(own.win > median(own.win), 1, 0)) #dummy

# Appending logs
data$log.mtbe.win <- log(data$mtbe.win)
data$log.own.win <- log(data$own.win)

# Write the data to csv
#write.csv(data, "data.csv", row.names = F)

# Models
fit0 <- lm(mtbe ~ own, data = data)
fit1 <- lm(mtbe.win ~ own.win, data = data)
fit2 <- lm(mtbe ~ own.win + I(own.win ^ 2), data = data)
fit3 <- lm(log(mtbe.win) ~ own.win, data = data)
fit4 <- lm(mtbe.win ~ log(own.win), data = data)
fit5 <- lm(log(mtbe.win) ~ log(own.win), data = data)
fit6 <- lm(log(mtbe.win) ~ D.own.win.above.median, data = data)
fit7 <- lm(log(mtbe.win) ~ own.win + D.own.win.above.median + (own.win * D.own.win.above.median), data = data)

# Summarizing models
stargazer(fit0, fit1, fit2, fit3, fit4, fit5, fit6, fit7,
          type = "text",
          keep.stat = c("adj.rsq", "rsq", "n"),
          report = "vc*stp")



# TASK 2

# Computing delta
data_diff <- data

for (i in 3:(ncol(data_diff)-1)) { #for each relevant column in data
  data_diff[,i] <- Delt(data_diff %>% pull(i)) #compute and assign delta (change) data
  data_diff[1,i] <- 0 #first data point = 0
}

# Models

fit0_diff <- lm(mtbe ~ own, data = data_diff)
fit1_diff <- lm(mtbe.win ~ own.win, data = data_diff)
fit2_diff <- lm(mtbe ~ own.win + (own.win ^ 2), data = data_diff)
fit3_diff <- lm(log(mtbe.win) ~ own.win, data = data_diff)
fit4_diff <- lm(mtbe.win ~ log(own.win), data = data_diff)
fit5_diff <- lm(log(mtbe.win) ~ log(own.win), data = data_diff)
fit6_diff <- lm(log(mtbe.win) ~ D.own.win.above.median, data = data_diff)
fit7_diff <- lm(log(mtbe.win) ~ own.win + D.own.win.above.median + (own.win * D.own.win.above.median), data = data_diff)

# Summarizing models
stargazer(fit0_diff, fit1_diff, fit2_diff, fit3_diff, fit4_diff, fit5_diff, fit6_diff, fit7_diff,
          type = "text",
          keep.stat = c("adj.rsq", "rsq", "n"),
          report = "vc*stp")


# TASK 3

# declare sequence
x.axis <- seq(0.0001, 0.3, by = 0.0001)
# empty plot
plot(NULL, xlim = c(0, 0.3), ylim = c(2, 4), xlab = "Ownership", ylab = "Firm value")
# include a line with the first model
lines(x = x.axis, y = (fit0$coefficients[1] + fit0$coefficients[2]*x.axis), col = "red")
lines(x = x.axis, y = (fit1$coefficients[1] + fit1$coefficients[2]*x.axis), col = "red")
lines(x = x.axis, y = (fit2$coefficients[1] + fit2$coefficients[2]*x.axis), col = "red")
lines(x = x.axis, y = (fit3$coefficients[1] + fit3$coefficients[2]*x.axis), col = "red")

lines(x = x.axis, y = (fit2$coefficients[1] + fit2$coefficients[2]*x.axis + fit2$coefficients[3] * (x.axis^2)), col = "green")


lines(x = x.axis, y = exp(fit6$coefficients[1] + fit6$coefficients[2] * (x.axis > median(data$own.win))), col = "cyan")



# # data.frame indicating the values and changes in the independent variable
# p10 <- quantile(data$own.win, .10)
# p50 <- quantile(data$own.win, .50)
# sd.x <-sd(data$own.win)
# 
# # make a dataframe
# new.df <-data.frame(description.of.value =c("p10", "p10+sd.x", "p50", "p50+sd.x"),
#                     own =c(p10, p10+sd.x,p50, p50+sd.x),
#                     own.win =c(p10, p10+sd.x,p50, p50+sd.x))
# 
# new.df$own.above.median <- new.df$own.win> median(data$own.win)
# 
# # decide on model
# m <- fit3
# 
# temp.pred <- predict(m, new.df)
# 
# # change at p10
# temp.pred[2] - temp.pred[1]




# Regular plot

# empty plot
plot(NULL, xlim = c(0.0001, 0.3), ylim = c(2, 4), xlab = "Ownership", ylab = "Firm value", main = "Models")

# include a line with the first model
abline(fit0$coefficients[1], fit0$coefficients[2], col = "#6ba4ff")
abline(fit1$coefficients[1], fit1$coefficients[2], col = "#428bff")
abline(fit2$coefficients[1], fit2$coefficients[2], col = "#1c74ff")
abline(fit3$coefficients[1], fit3$coefficients[2], col = "#0063ff")
abline(fit4$coefficients[1], fit4$coefficients[2], col = "#0055db")
abline(fit5$coefficients[1], fit5$coefficients[2], col = "#0044b0")
abline(fit6$coefficients[1], fit6$coefficients[2], col = "#003180")
abline(fit7$coefficients[1], fit7$coefficients[2], col = "#001f52")

# ggplot version
ggplot(data.frame("x" = c(0.0001, 0.3), "y" = c(2,4))) +
  geom_blank(aes(x,y)) +
  geom_abline(intercept = fit0$coefficients[1], slope = fit0$coefficients[2], col = "#6ba4ff") +
  geom_abline(intercept = fit1$coefficients[1], slope = fit1$coefficients[2], col = "#428bff") +
  geom_abline(intercept = fit2$coefficients[1], slope = fit2$coefficients[2], col = "#1c74ff") +
  geom_abline(intercept = fit3$coefficients[1], slope = fit3$coefficients[2], col = "#0063ff") +
  geom_abline(intercept = fit4$coefficients[1], slope = fit4$coefficients[2], col = "#0055db") +
  geom_abline(intercept = fit5$coefficients[1], slope = fit5$coefficients[2], col = "#0044b0") +
  geom_abline(intercept = fit6$coefficients[1], slope = fit6$coefficients[2], col = "#003180") +
  geom_abline(intercept = fit7$coefficients[1], slope = fit7$coefficients[2], col = "#001f52") +
  labs(x = "Ownership", y = "Firm value") +
  theme_classic()


# Log plot

# declare sequence
x.axis <- seq(0,0.3, by = 0.0001)
# empty plot
plot(NULL, xlim = log(c(0.0001, 0.3)), ylim = log(c(2, 4)), xlab = "Ownership", ylab = "Firm value", main = "Models (log)")
# include a line with the first model
lines(x = log(x.axis), y = log(fit0$coefficients[1] + fit0$coefficients[2]*x.axis), col = "#ff0000")
lines(x = log(x.axis), y = log(fit1$coefficients[1] + fit1$coefficients[2]*x.axis), col = "#d90000")
lines(x = log(x.axis), y = log(fit2$coefficients[1] + fit2$coefficients[2]*x.axis), col = "#b50000")
lines(x = log(x.axis), y = log(fit3$coefficients[1] + fit3$coefficients[2]*x.axis), col = "#8a0000")
lines(x = log(x.axis), y = log(fit4$coefficients[1] + fit4$coefficients[2]*x.axis), col = "#6b0000")
lines(x = log(x.axis), y = log(fit5$coefficients[1] + fit5$coefficients[2]*x.axis), col = "#4d0000")
lines(x = log(x.axis), y = log(fit6$coefficients[1] + fit6$coefficients[2]*x.axis), col = "#360000")
lines(x = log(x.axis), y = log(fit7$coefficients[1] + fit7$coefficients[2]*x.axis), col = "#240000")

# Plots showing all data with models
ggplot(plotdata) +
  geom_point(aes(mtbe, own), color = "red") +
  geom_point(aes(mtbe, own.win), color = "blue") +
  geom_point(aes(mtbe, log.own.win), color = "green") +
  geom_abline(intercept = fit0$coefficients[1], slope = fit0$coefficients[2], col = "#6ba4ff") +
  geom_abline(intercept = fit1$coefficients[1], slope = fit1$coefficients[2], col = "#6ba4ff") +
  geom_abline(intercept = fit2$coefficients[1], slope = fit2$coefficients[2], col = "#6ba4ff") +
  theme_clean()

ggplot(plotdata) +
  geom_point(aes(mtbe.win, own), color = "red") +
  geom_point(aes(mtbe.win, own.win), color = "blue") +
  geom_point(aes(mtbe.win, log.own.win), color = "green") +
  geom_abline(intercept = fit0$coefficients[1], slope = fit0$coefficients[2], col = "#6ba4ff") +
  theme_clean()

ggplot(plotdata) +
  geom_point(aes(log.mtbe.win, own), color = "red") +
  geom_point(aes(log.mtbe.win, own.win), color = "blue") +
  geom_point(aes(log.mtbe.win, log.own.win), color = "green") +
  geom_abline(intercept = fit0$coefficients[1], slope = fit0$coefficients[2], col = "#6ba4ff") +
  #geom_smooth(aes(log.mtbe.win, own), method = "lm", se = F) +
  theme_clean()

ggplot(plotdata) +
  geom_point(aes(log.own.win, log.mtbe.win), color = "black") +
  geom_abline(intercept = fit5$coefficients[1], slope = fit5$coefficients[2], col = "#6ba4ff") +
  #geom_smooth(aes(log.mtbe.win, own), method = "lm", se = F) +
  theme_clean()



# Scatterplot matrix
plotdata <- data[,c("mtbe", "mtbe.win", "log.mtbe.win", "own", "own.win", "log.own.win")]
plot(plotdata, pch = 19, lower.panel = NULL)


plot(plotdata[,c("log.mtbe.win", "log.own.win")], pch = 19)
x.axis <- seq(-0.5, 2.5, by = 0.0001)
lines(fit5$coefficients[1], fit5$coefficients[2], col = "#0044b0")


