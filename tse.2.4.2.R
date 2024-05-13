# Time Series Econometrics
# Cointegration Testing (Engel-Granger)
# between more than 2 variables

library(urca)         #For dataset
library(tseries)      #For stationarity testing
library(tidyverse)    #For lag() function

data("Raotbl4")

y <- Raotbl4$den
x1 <- Raotbl4$ger
x2 <- Raotbl4$aus

### PART 1: Long-Run Cointegration Testing ###

# Step 1: Find Order of Integration of Variables

adf.test(y)
adf.test(x1)
adf.test(x2)

dy <- diff(y)
dx1 <- diff(x1)
dx2 <- diff(x2)

adf.test(dy)
adf.test(dx1)
adf.test(dx2)


# Step 2: Model Selection


# Step 3: Estimate Residuals using Regression

t <- 1:117  #change this to length of y and x

reg1 <- lm(y ~ x1 + x2 + t)

e <- reg1$residuals

# Step 4: Test stationarity of residuals using ADF

adf.test(e)


### PART 2: Short-Run Error Correction Model ###

e.lag <- lag(e, 1)

e.lag <- e.lag[-117]

reg2 <- lm(dy ~ dx1 + dx2 + e.lag)

summary(reg2)



# Watch the tutorial at:
# https://youtu.be/TZ807BIC3hA
# https://youtu.be/Y3IreEt3Qlc
# https://youtu.be/PkCwYIGBtJY
# Thanks for stopping by :)
