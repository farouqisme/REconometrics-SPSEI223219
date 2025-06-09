library(MatchIt)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(cobalt)


ecls <- read.csv("ecls.csv")


ecls %>%
  group_by(catholic) %>%
  summarise(n_students = n(),
            mean_math = mean(c5r2mtsc_std),
            std_error = sd(c5r2mtsc_std) / sqrt(n_students))

ecls %>% mutate(test = (c5r2mtsc - mean(c5r2mtsc)) / sd(c5r2mtsc)) %>%   #this is how the math score is standardized 
  group_by(catholic) %>%
  summarise(mean_math = mean(test))
            
ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')
ecls %>%
  group_by(catholic) %>%
  select(one_of(ecls_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))

lapply(ecls_cov, function(v) {
  t.test(ecls[, v] ~ ecls[, 'catholic'])
})

with(ecls, t.test(race_white ~ catholic))  #(repeat for each covariate)

ecls <- ecls %>% mutate(w3income_1k = w3income / 1000)
m_ps <- glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
            family = binomial(), data = ecls)
summary(m_ps)

ecls_nomiss <- ecls %>%  # MatchIt does not allow missing values
  select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>%
  na.omit()

ecls2 <- ecls %>% select(., -c(race))


##################PSM
mod_match_nearest <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                     method = "nearest", distance = "glm", data = ecls_nomiss)

mod_match_full <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                     method = "full", distance = "glm", data = ecls_nomiss)

mod_match_opt <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                          method = "optimal", distance = "glm", data = ecls_nomiss)

##################Summarize the results
summary(mod_match_full)

##################Comparing dist
plot(mod_match_nearest, type = "jitter")
plot(mod_match_nearest, type = "hist")

plot(mod_match_full, type = "jitter")
plot(mod_match_full, type = "hist")






bal.plot(mod_match_nearest, var.name = "distance", which = "both")
bal.plot(mod_match_full, var.name = "distance", which = "both")

##################Pulling up result's data
data_full <- match.data(mod_match_full)
data_nearest <- match.data(mod_match_nearest)
data_opt <- match.data(mod_match_opt)



##################Regression
model1 <- lm(c5r2mtsc_std ~ catholic + race_white + w3income + p5hmage + 
               p5numpla + w3momed_hsb, data = ecls_nomiss)

model_full <- lm(c5r2mtsc_std ~ catholic + race_white + w3income + p5hmage + 
               p5numpla + w3momed_hsb, data = data_full, weights = weights)

model_nearest <- lm(c5r2mtsc_std ~ catholic + race_white + w3income + p5hmage + 
               p5numpla + w3momed_hsb, data = data_nearest, weights = weights)

model_opt <- lm(c5r2mtsc_std ~ catholic + race_white + w3income + p5hmage + 
               p5numpla + w3momed_hsb, data = data_opt, weights = weights)


summary(model1)
summary(model_full)
summary(model_nearest)
summary(model_opt)

AIC(model1)
AIC(model_full)
AIC(model_nearest) ##nearest gives lowest AIC == best model
AIC(model_opt)



modelsummary(list("Full opt" = model_full, "nearest" = model_nearest, "optimal" = model_opt,"Non-PSM model" = model1),
             gof_omit = 'Log|Adj|p\\.value|statistic|se_type',
             stars = TRUE)


##################ESTIMATING ATET & ATT
###########ESTIMATING ATET
ATET_match_nearest <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                             method = "full", distance = "glm", 
                             estimand = "ATT", data = ecls_nomiss)
data_ATET <- match.data(ATET_match_nearest)

# Estimate ATT using regression
model_ATET <- lm(c5r2mtsc_std ~ catholic + race_white + w3income + p5hmage + 
               p5numpla + w3momed_hsb, data = data_ATET, weights = weights)
summary(model_ATET)

###########ESTIMATING ATE
ATE_match_nearest <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                              method = "full", distance = "glm", 
                              estimand = "ATE", data = ecls_nomiss)
data_ATE <- match.data(ATE_match_nearest)

# Estimate ATE using regression
model_ATE <- lm(c5r2mtsc_std ~ catholic + race_white + w3income + p5hmage + 
                  p5numpla + w3momed_hsb, data = data_ATE, weights = weights)
summary(model_ATE)
