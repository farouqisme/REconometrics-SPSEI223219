library(tseries)  #jarque-bera
library(lmtest)   #breusch-pagan, RESET, durbin-watson
library(car)      #VIF
library(readxl)   #read XLSX
library(ggplot2)
library(sandwich)#robust SE
library(nlme)   #GLS


inc <- read_xlsx("KIELMC.xlsx", sheet = "Sheet 1")

model <- lm(price~intst + age + agesq + nearinc, data = inc)


plot(model) ##### <-- MELIHAT KONSISTENSI COVARIANCE

bptest(model) ### <-- Uji homoskedasticity ternyata tidak 

gls1 <- nlme::gls(price~intst + age + agesq + nearinc, data = inc)

gls <- nlme::gls(price~intst + age + agesq + nearinc, weights=nlme::varExp(), data = inc)

summary(gls1)
summary(gls)
plot(gls1)

plot(gls)

AIC(gls)
AIC(gls1)
#################ROBUST SE##################
model <- lm(price~intst + age + agesq + nearinc, data = inc)
summary(model)
model2 <- coeftest(model, vcov. = vcovHC(model, type="HC1"))



coefte

