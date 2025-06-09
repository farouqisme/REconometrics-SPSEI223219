library(ivreg)
library(dplyr)

mydata <- read.csv("IV Data.csv", na.strings = ".")  # Open dataset from working directory


mydata <- subset(mydata, is.na(wage) == FALSE)   # remove observations with missing wages from dataset



reg_ex0 <- lm(lwage~educ+exper+expersq,data=mydata) # OLS estimation
print(summary(reg_ex0))

reg_iv1 <- ivreg::ivreg(lwage~educ+exper+expersq|fatheduc+motheduc+exper+
                          expersq,data=mydata) # IV 2SLS where fatheduc and matheduc as intruments
reg_iv2 <- ivreg::ivreg(lwage~educ+exper+expersq|. -educ+fatheduc+motheduc,data=mydata) # Alternative
summary(reg_iv1)
summary(reg_iv2)

##Weak instrument = H0:Instrument is weak
##Wu-Hausman = H0:IV model is consistent and efficient (>= OLS)
##Sargan Test = H0:IV is not overidentified
modelsummary::modelsummary(list("OLS" = reg_ex0, "IV" = reg_iv1),
             gof_omit = 'IC|Log|Adj|p\\.value|statistic|se_type',
             stars = TRUE)

