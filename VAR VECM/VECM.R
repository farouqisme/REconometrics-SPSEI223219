
library(tseries) # Transform into TS format
library(urca)    # ADF test
library(vars)    # Optimal lag selection & Cointegration Test
library(tsDyn)   # VECM Estimation

## Import Data
india <- openxlsx::read.xlsx("VECM data.xlsx", sheet = "My Series")

india$imp <- log(india$imp)
india$exp <- log(india$exp)
india$exp_us <- log(india$exp_us)

## Transform to TS data
cpi <- ts(india$cpi, start= c(2005,1), frequency = 12)
exrate <- ts(india$exrate, start= c(2005,1), frequency = 12)
imp <- ts(india$imp, start= c(2005,1), frequency = 12)
exp_us <- ts(india$exp_us, start= c(2005,1), frequency = 12)

ts.plot(cpi)
ts.plot(imp)
ts.plot(exrate)
ts.plot(exp_us)

## Unit-root test
adf.test(cpi); adf.test(diff(cpi)) 
adf.test(imp); adf.test(diff(imp))
adf.test(exrate); adf.test(diff(exrate))
adf.test(exp_us); adf.test(diff(exp_us))

## Create the system
dset <- cbind(exrate,cpi,exp_us,imp)
diff_dset <- diff(dset)

## Choose optimal lag
lagselect <- VARselect(dset, lag.max = 12, type = "const")
lagselect

VARawal <- VAR(dset, p = 12, type = "const")
roots(VARawal)

VARawal <- VAR(diff_dset, p = 12, type = "const")
roots(VARawal)

## Cointegration Test
ctest1t <- ca.jo(dset, type = "trace", ecdet = "const", K = 11)
summary(ctest1t) ## <- coint == 3

ctest1t@V ## Identify Long-rung relationship (Cointegrating Vectors)
ctest1t@W ## Identify Adjustment Coefficient

ctest1e <- ca.jo(dset, type = "eigen", ecdet = "const", K = 11)
summary(ctest1e) ## <- coint == 3


## VECM
Model1 <- VECM(dset, lag = 12, r = 3, estim =("ML"))

summary(Model1)


coefs_all <- summary(Model1)$coefMat
coefs_all[grep("ECT", rownames(coefs_all)),]

## IRF
varmod <- vec2var(ctest1t, r = 3)
irfexrate <- irf(varmod, impulse = "exrate", response = "cpi", n.ahead = 20)
plot(irfexrate)

irfexp_us <- irf(varmod, impulse = "exp_us", response = "exrate", n.ahead = 20)
plot(irfexp_us)

irfimp <- irf(varmod, impulse = "imp", response = "cpi", n.ahead = 20)
plot(irfimp)

irfcpi <- irf(varmod, impulse = "cpi", response = "exrate", n.ahead = 20)
plot(irfcpi)

## IRF
fevd(varmod)
