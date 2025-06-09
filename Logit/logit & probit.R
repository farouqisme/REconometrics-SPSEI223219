
library(haven)  #import .dta
library(mfx) #marginal effects estimation

data <- read_dta("Logit & Probit Data.dta")

data <- data[!is.na(data$gunlaw),]

data$gunlaw <- ifelse(data$gunlaw == 2,0,1)

data$sex <- ifelse(data$sex == 2,1,0)

###LOGIT
logit <- glm(gunlaw ~ educ + age + factor(sex), 
             data = data, family = binomial(link = "logit"))


summary(logit)

#ODD RATIO
oddrat_logit <- logitor(gunlaw ~ educ + age + factor(sex), data = data)
oddrat_logit

#MARGINAL EFFECT
logit_mfx <- logitmfx(gunlaw ~ educ + age + factor(sex), data = data)
logit_mfx


logLik(logit)

###PROBIT
probit <- glm(gunlaw ~ educ + age + factor(sex), data = data, family = binomial(link = "probit"))
summary(probit)

#ODD RATIO
cbind(Estimate=round(coef(probit),4),
      OR=round(exp(coef(probit)),4))


#MARGINAL EFFECT
probit_mfx <- probitmfx(gunlaw ~ educ + age + factor(sex), data = data)
probit_mfx

logLik(probit)

