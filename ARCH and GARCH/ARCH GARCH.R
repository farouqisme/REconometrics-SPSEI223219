setwd("C:/Users/muham/OneDrive/KeRjAaaAAAaaAaAAA/[SPS] Gasal 24-Tutorial Ekonometrika Untuk Penelitian/Materi/Petemuan 8")


# install.packages("openxlsx")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("lmtest")
# install.packages("FinTS")
# install.packages("urca")
# install.packages("rugarch")


library(openxlsx) # Open xlsx file
library(tseries) # Transform into TS format
library(forecast) # ARIMA estimation
library(lmtest) #Showing ARIMA result's p-val
library(FinTS) #Archtest
library(urca) #ur.df <- complete result of ADF test
library(rugarch) #ARCH GARCH estimation


data <- read.xlsx("Consumer Price Index.xlsx", sheet = "My Series")

# create inf data
for(i in 2:nrow(data)){
  data$inf[i] <- (data$cpi[i]-data$cpi[i-1])/data$cpi[i-1]
}

# declare as TS data
data <- ts(data$inf[2:nrow(data)], start = c(1968,2), frequency = 12)

# plot
plot.ts(data)

# stationary test
View(adf.test(data))
View(pp.test(data))
summary(ur.df(data, type = "drift", selectlags = "AIC")) # <- more complete ADF-test resuslt
# ARIMA estimation
acf(data) #4
pacf(data) # 4

arimares <- arima(data, order = c(4,0,4))
coeftest(arimares)

# Plotting residual
pacf(resid(arimares))
acf(resid(arimares))

# Exhibiting ARCH effect
Box.test(data,type = "Ljung-Box")
ArchTest(data, lags = 4) # <- decided based on subject-matter knowledge or ACF/PACF


# ARCH and GARCH model specification
# GARCH
sgarch <- ugarchspec(variance.model=list(model = "sGARCH",
                  garchOrder=c(1,1)), #GARCH(p,q) where p = ARCH and q = GARCH
                  mean.model=list(armaOrder=c(4,4)),
                  distribution.model="std",) #student-t dist
# EGARCH
egarch <- ugarchspec(variance.model=list(model = "eGARCH", 
                  garchOrder=c(1,1)), #GARCH(p,q) where p = ARCH and q = GARCH
                  mean.model=list(armaOrder=c(4,4)),
                  distribution.model="std",) #student-t dist

# GARCH-in-Mean
garchm <- ugarchspec(variance.model=list(model = "fGARCH", #implementing omnibus GARCH model
                                         garchOrder=c(1,1),#GARCH(p,q) where p = ARCH and q = GARCH
                                         submodel = "APARCH"), #to allow leverage and taylor effect
                     mean.model=list(armaOrder=c(0,0),
                                     include.mean=T,
                                     archm=T, #to inclue ARCH-in-mean parameter
                                     archpow=2), #1 = sd, 2 = variance
                     distribution.model="std",) #student-t dist


# Estimate the model
sgarch_fit <- ugarchfit(data=data,spec=sgarch)
egarch_fit <- ugarchfit(data=data,spec=egarch)
garchm_fit <- ugarchfit(data=data,spec=garchm)

# Results summary
sgarch_fit
egarch_fit
garchm_fit

# QQ-plot
plot(sgarch_fit, which=9)
plot(egarch_fit, which=9)
plot(garchm_fit, which=9)

# Compile IC results
model.list = list("sGARCH" = sgarch_fit, "eGARCH" = egarch_fit, "GARCHM" = garchm_fit)
info.mat = sapply(model.list, infocriteria)
# rownames(info.mat) = rownames(infocriteria(garchnorm))
info.mat

#### the best model should:
#### 1. Has the lowest information criterion
#### 2. Controlling all residuals ( GARCH model assumes the standardized residuals are i.i.d., so there should be no ARCH effects in them)
#### 3. Residual's distribution is normally distributed (can be observed using QQ-plot)

plot.ts(sigma(sgarch_fit), ylab="sigma(t)", col="blue", main = "Conditional Variance of GARCH Model", cex.main=1)
plot.ts(sigma(egarch_fit), ylab="sigma(t)", col="blue", main = "Conditional Variance of EGARCH Model", cex.main=1)
plot.ts(sigma(garchm_fit), ylab="sigma(t)", col="blue", main = "Conditional Variance of GARCH-M Model", cex.main=1)

plot(ugarchforecast(sgarch_fit, data = null, n.ahead = 5, n.roll = 0, out.sample = 0))
