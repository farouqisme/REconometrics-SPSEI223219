# Manual MLE 
library(stats4)

# input data for x and y
Teaching_Method <- c(0,0,1,0,1,1,1,1,0,0,0,1)
Test_Score <- c(90,90,100,70,90,95,75,75,60,65,65,70)

# Define the model using the dnorm() function
LL <- function(beta_0, beta_1, sigma){
  -sum(dnorm(Test_Score,(beta_0+beta_1*Teaching_Method), sigma, log = TRUE))
}

# use mle() function to calculate regression coefficients 

result_1 <- mle(LL, start = list(beta_0 = 1, beta_1=1, sigma = 19))
summary(result_1)

# calculate sigma squared
(result_1@coef[3])^2


# MLE using readymade distribution
result_2 <- glm(Test_Score ~ Teaching_Method, family = "gaussian")
summary(result_2)
?glm
