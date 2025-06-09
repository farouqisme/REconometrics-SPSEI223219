library(plm)
library(lmtest)   # Hausman Test & Breusch Pagan Test
library(sandwich) # robust standard error
library(dplyr)
library(tidyverse)

# Load example data
data("Produc", package = "plm")


# Add log transformation variable for Hausman Regression Based+Robustified
Produc <- Produc %>% mutate(
  lgsp = log(gsp),
  lpcap = log(pcap),
  lpc = log(pc),
  lemp = log(emp)
)

# Estimate POLS model
pooled_ols_model <- plm(log(gsp) ~ log(pcap) + log(pc) + 
                          log(emp), data = Produc, model = "pooling")

summary(pooled_ols_model)
# Estimate FD model
Produc2 <- Produc %>% group_by(state) %>% arrange(year) %>%
  mutate(
    fgsp = gsp-lag(gsp),
    fpcap = pcap - lag(pcap),
    fpc = pc - lag(pc),
    femp = emp - lag(emp)
  ) %>% filter(!is.na(femp))




fd_model <- plm(fgsp ~ fpcap + fpc + femp, data = Produc2, model = "pooling")

summary(fd_model)
# Estimate FE model
fe_model <- plm(log(gsp) ~ log(pcap) + log(pc) + 
                  log(emp), data = Produc, model = "within")

summary(fe_model)
# Estimate RE model
re_model <- plm(log(gsp) ~ log(pcap) + log(pc) + 
                  log(emp), data = Produc, model = "random")

summary(re_model)


###### Pesaran's F-test test (Chow Test) to decide whether FE or POLS method
pFtest(fe_model, pooled_ols_model) # a < 0.05 == individual (fixed) effects & a > 0.05 == no individual (fixed) effects


###### Wooldrdige Test
pwtest(pooled_ols_model, effect = "individual")
pwtest(pooled_ols_model, effect = "time")
pwtest(pooled_ols_model, effect = c("individual", "time"))

###### Hausman test to decide whether FE or RE method
phtest(fe_model, re_model, method = "chisq") # a < 0.05 == FE & a > 0.05 == RE
phtest(log(gsp) ~ log(pcap) + log(pc) + log(emp), data = Produc)
phtest(fe_model,re_model, data = Produc, method = "aux", vcov = vcovHC)


###### BP LM test to identify \alpha_i
plmtest(re_model, effect = c("individual"), type = "bp") # a < 0.05 == FE & a > 0.05 == RE

###### Breusch-Pagan test for heteroskedasticity
bptest(pooled_ols_model)
bptest(fe_model)
bptest(re_model)

###### Standard Error
# Compute robust standard errors (HC0)
robust_se <- vcovHC(fe_model, type = "HC0")

# Use robust standard errors in summary
summary(fe_model, robust = TRUE, vcov = robust_se)

