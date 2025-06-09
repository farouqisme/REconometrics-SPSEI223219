
###IMPORT LIBRARY
library(stats) ###ANOVA, T-TEST
library(openxlsx)

###IMPORT DATA
data <- read.xlsx("Cleaned Data.xlsx", sheet = "Sheet 1")
View(data)


##ANOVA
#One-way ANOVA
oneway_np <- aov(p1~NamaPulau,data = data)    ##One-Way Anova
oneway_thn <- aov(p1~tahun,data = data)       ##One-Way Anova
twoway <- aov(p1~NamaPulau*tahun,data = data) ##Two-Way Anova

summary(oneway_np)
summary(oneway_thn)
summary(twoway)

##T-TEST
osamp_hls <- t.test(data$hls, mu = 10)        ##One-sample T-test
twsamp_hls <- t.test(hls ~ tahun, data = data)##Two-sample T-test

osamp_hls
twsamp_hls

##PEARSON
cor(data$KodeProvinsi, data$p1,  method = "pearson", use = "complete.obs") ##complete.obs to automatically eliminate NA in calculation
cor(data$hls, data$uhh,  method = "pearson", use = "complete.obs")


##SIMPLE REGRESSION
reg <- lm(uhh~hls, data = data)
summary(reg)

##Plot regression
data %>%
  ggplot(., aes(hls, uhh)) +
  geom_point() +
  geom_smooth(method = "lm")
