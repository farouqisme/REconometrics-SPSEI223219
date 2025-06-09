
# install.packages("vars")
library(tseries) # Transform into TS format
library(urca)    # ADF test
library(vars)    # Optimal lag selection & Cointegration Test
library(tsDyn)   # VECM Estimation


data("denmark")
df.lev <- denmark[,c("LRM","LRY")]

## Make Time-series Data
lrm <- ts(df.lev$LRM, start = c(1974,1), frequency = 12) ## Logarithm of real money, M2.
lry <- ts(df.lev$LRY, start = c(1974,1), frequency = 12) ## Logarithm of real income.

## Stationarity Test

pp.test(lrm) ## Nonstationary
pp.test(lry) ## Nonstationary

pp.test(diff(lrm)) ## Stationary
pp.test(diff(lry)) ## Stationary

ts.plot(lrm)
ts.plot(diff(lrm))
ts.plot(lry)
ts.plot(diff(lry))


## Create the system

dset <- cbind(lrm,lry)
diffdset <- diff(dset)


## lag Length
lagselect <- VARselect(dset, lag.max = 12, type = "const")
lagselect
lagselect$selection
lagselect$criteria

#Since 3 came up the most, we use (3-1) or 2 lags

## Cointegration Test
ctest1t <- ca.jo(dset, type = "trace", ecdet = "const", K = 2)
summary(ctest1t)

ctest1e <- ca.jo(dset, type = "eigen", ecdet = "const", K = 2)
summary(ctest1e)

## From the tests above, we'll use VAR estimation
VARest <- VAR(diffdset, p = 3, type = "const")
summary(VARest)

## Stability Test
roots(VARest) ## <- Eigenvalues < 1 [[Stable]]

## Testing for serially correlated errors (either Portmentaeau and Breusch-Godfrey test)
serial.test(VARest)


## IRF
lrm_irf <- irf(VARest, impulse = "lrm", n.ahead = 10)
plot(lrm_irf)

lry_irf <- irf(VARest, impulse = "lry", n.ahead = 10)
plot(lry_irf)

fevd(VARest, n.ahead = 10)



## Granger Causality
lrmGranger <- causality(VARest, cause = "lrm")
lryGranger <- causality(VARest, cause = "lry")

