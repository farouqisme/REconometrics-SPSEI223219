# Load the package and data
library(plm)
data("EmplUK")


# Estimate the dynamic model with Two-step System GMM
dyn_model <- pgmm(log(emp) ~ lag(log(emp),1) + 
                    lag(log(wage),1) + lag(log(capital),0:1) |
                    ## symbol after | refers to instrumental variables
                    lag(log(emp), 2:99) + lag(log(wage), 2:99) + 
                    lag(log(capital), 2:99),
                  ##Data yang digunakan
                  data = EmplUK, 
                  ##Specifies type of effect (individual, time, or both)
                  effect = "twoways", 
                  ##onestep uses initial set of instruments and estimates the parameters in a single step
                  ##twostep adds variance-covariance matrix of the moment condition is updated, which improves efficiency
                  model = "twosteps", 
                  ##d == use differenced transformation -> Differenced GMM
                  ##ld == use level & differenced transformation -> System GMM
                  transformation = "ld",
                  ##technique used to reduce the number of instruments 
                  ##by combining them into fewer aggregate instruments
                  ##particularly used in the System GMM approach
                  collapse = TRUE
)
summary(dyn_model)


#Sargan Test to check instrument validity
sargan_test <- sargan(dyn_model)
sargan_test ## a < 0.05 == instrument is not valid & a > 0.05 == instrument is valid to solve endogeneity

#Arellano-Bond test to check Autocorrelation 
ar1_test <- mtest(dyn_model, order = 1)
print(ar1_test)
ar2_test <- mtest(dyn_model, order = 2)
print(ar2_test) ## GMM model must not contains Autocorrelation in AR(2)




