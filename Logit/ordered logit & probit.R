
library(haven)  #import .dta
library(MASS)   #regression results needed to obtain marginal effects (polr)
library(erer)   #calculating marginal effects
library(ordinal)#ordered logit/probit find the p-value
library(mfx) #marginal effects estimation

data <- read_dta("OLogit Oprobit-chap4,7&12-exercise.dta")

data <- data[!is.na(data$fechld),]

################Ordered Logit
######Regresi
logit <- clm(factor(fechld) ~ educ + age + sibs, data = data, Hess=TRUE, link = "logit")
summary(logit)


######Odd ratio
oddrat_logit <- logitor(factor(fechld) ~ educ + age + sibs, data = data)
oddrat_logit


######Mendapatkan Marginal Effects
logit <- polr(factor(fechld) ~ educ + age + sibs, data = data, Hess=TRUE, method = "logistic")
ocME(logit)

######Mengukur Log-likelihood Ratio
logLik(logit)

################Ordered Probit
######Regresi
probit <- clm(factor(fechld) ~ educ + age + sibs, data = data, Hess=TRUE, link = "probit")
summary(probit)

######Odd ratio
cbind(Estimate=round(coef(probit),4),
      OR=round(exp(coef(probit)),4))

######Mendapatkan Marginal Effects
probit <- polr(factor(fechld) ~ educ + age + sibs, data = data, Hess=TRUE, method = "probit")
ocME(probit)

######Mengukur Log-likelihood Ratio
logLik(probit)

