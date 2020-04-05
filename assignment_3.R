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
library(tseries)
library(dynlm)
library(lmtest)
library(sandwich)
library(plm)
graphics.off() #reset graphics
rm(list = ls()) #clear workspace, variables
options(scipen=999) #disable scientific notation

# Import data
data_tesla <- read_table2("tesla.txt",
                          col_types = cols(PERMNO = col_skip(),
                                           date = col_date(format = "%Y%m%d")))
data_factor <- read_csv("factor_returns.txt",
                        col_types = cols(date = col_date(format = "%Y%m%d")))

# Join dataframes
data <- full_join(data_tesla, data_factor, by = "date") #full outer join
data <- arrange(data, date) #sort by date


# 1
# Window variables
dtstar <-  as.Date("2018-08-07") #date t star
ntstar <- which(grepl(dtstar, data$date)) #rownum t star (using rownums in df to account for weekends and other non trading days)

k = 1
h = 5
e = 250

nestwin <- (ntstar-k-h-e):(ntstar-k-h) #rownum estimation window
nholwin <- (ntstar-k-h):(ntstar-k) #rownum holdout window
nevtwin <- (ntstar-k):(ntstar+k) #rownum event window

# Windows
estimation <- data[nestwin,]
event <- data[nevtwin,]

# 2
# plot tesla stock price and event date
ggplot(data_tesla, aes()) +
  geom_line(aes(date, PRC)) +
  geom_vline(xintercept = dtstar, col = "red") +
  theme_classic() +
  labs(title = "TSLA stock price and event date",
       x = "Date",
       y = "Price")


# 3 & 6
# RGP
rgp_const <- lm(RET ~ 1, data = estimation)
rgp_ff <- lm((RET - rf) ~ mkt.rf + smb + hml, data = estimation)


# Computing abnormal return
event <- event %>%
  mutate(ERconst = predict(rgp_const, event)) %>% #expected returns
  mutate(ERff = predict(rgp_ff, event)) %>%
  mutate(ARconst = RET - ERconst) %>% #abnormal returns
  mutate(ARff = RET - ERff)

# 4
# Computing cumulative abnormal returns
event <- event %>%
  mutate(CARconst = cumsum(ARconst)) %>%
  mutate(CARff = cumsum(ARff))

# 5
# Hypothesis testing
tvalueconst <- event$CARconst / sqrt(nrow(event) * var(rgp_const$residuals))
tvalueff <- event$CARff / sqrt(nrow(event) * var(rgp_ff$residuals))


# 8
# Dummy variables
d1 <- data$date == data$date[ntstar-1]
d2 <- data$date == data$date[ntstar]
d3 <- data$date == data$date[ntstar+1]

# Time series regression
fit <- lm((RET - rf) ~
          + d1
          + d2
          + d3
          + mkt.rf
          + smb
          + hml, data = data)

summary(fit)
stargazer(fit,
          type = "text",
          keep.stat = c("rsq", "n"),
          report = "vc*stp")






