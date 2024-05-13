# Time Series Econometrics
# Cointegration Testing (Engel-Granger)
# between 2 variables

library(urca)         #For dataset
library(tseries)      #For stationarity testing
library(tidyverse)    #For lag() function

data("Raotbl4")
x <- Raotbl4$ger
y <- Raotbl4$den

### PART 1: Long-Run Cointegration Testing ###

# Step 1: Find Order of Integration of Variables

adf.test(x)
adf.test(y)

dx <- diff(x)
dy <- diff(y)

adf.test(dx)
adf.test(dy)


# Step 2: Model Selection


# Step 3: Estimate Residuals using Regression

t <- 1:117

reg1 <- lm(y ~ x + t)

e <- reg1$residuals

# Step 4: Test stationarity of residuals using ADF

adf.test(e)


### PART 2: Short-Run Error Correction Model ###

e.lag <- lag(e, 1)

e.lag <- e.lag[-117]

reg2 <- lm(dy ~ dx + e.lag)

summary(reg2)


# Watch the tutorial at:
# https://youtu.be/TZ807BIC3hA
# https://youtu.be/Y3IreEt3Qlc
# https://youtu.be/PkCwYIGBtJY
# Thanks for stopping by :)

