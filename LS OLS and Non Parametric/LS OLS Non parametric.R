library(tseries)  #jarque-bera
library(lmtest)   #breusch-pagan, RESET, durbin-watson
library(car)      #VIF
library(readxl)   #read XLSX
library(stargazer)#regression result
library(ggplot2)
library(scales)
library(sandwich)

inc <- read_xlsx("KIELMC.xlsx", sheet = "Sheet 1")

########################################################OLS & Classic Assumption
#Regression
model <- lm(price~intst + age + agesq + nearinc, data = inc)

summary(model)
coeftest(model, vcov. = vcovHC(model, type="HC1"))


AIC(model)
logLik(model)

summary(model)
model2 <- lm(log(price)~log(intst) + age + agesq + 
               nearinc, data = inc)
summary(model2)
AIC(model2)
logLik(model2)
#Output
stargazer(model2,type='latex')


#Predicted Value & Resid
predicted <- predict(model2)
resid <- resid(model2)


#Jarqu-bera test - Residual Normality
shapiro.test(resid)

hist(resid)
mean(resid(model2))

#breusch-pagan test - Heteroskedasticity
bptest(model2)

#durbin-watson test - serial autocorrelation
dwtest(model2)

#VIF - multicollinearity
vif(model2)

#RESET - linearity
reset(model2)

inc$year <- as.character(inc$year)

graph <- ggplot(inc, aes(price, ..density..,group=nearinc, color=nearinc, fill= nearinc)) + 
  geom_density(alpha = 0.2)


graph
graph + scale_x_continuous(labels = label_comma()) + scale_y_continuous(labels = label_comma())

# Kruskal Wallis
inc2 <- inc[(inc$year == 1981),]
kruskal.test(lprice ~ nearinc, data = inc2)

# Wilcoxon Signed Rank Test
inc2 <- inc[(inc$nearinc == 1),]
inc_81 <- inc2[(inc2$year == 1981),]

inc_78 <- inc2[(inc2$year == 1978),]
inc_78 <- head(inc_78,40)

wilcox.test(inc_81$price, inc_78$price, paired= TRUE)



