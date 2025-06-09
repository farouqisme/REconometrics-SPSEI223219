#install.packages("estimatr")

library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(rdrobust)  # For robust nonparametric regression discontinuity
library(estimatr)  # Run 2SLS models in one step with iv_robust()
library(modelsummary)  # Create side-by-side regression tables


fuzzydat <- read.csv("tutoring_program_fuzzy.csv", header = T) 


ggplot(fuzzydat, aes(x = entrance_exam, y = tutoring_text, color = entrance_exam <= 70)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(size = 1.5, alpha = 0.5) + 
  # Add vertical line
  geom_vline(xintercept = 70) + 
  # Add labels
  labs(x = "Entrance exam score", y = "Participated in tutoring program") + 
  # Turn off the color legend, since it's redundant
  guides(color = FALSE)



fuzzydat %>% 
  group_by(tutoring, entrance_exam <= 70) %>% 
  summarize(count = n()) %>% 
  group_by(tutoring) %>% 
  mutate(prop = count / sum(count))

tutoring_centered <- fuzzydat %>% 
  mutate(entrance_centered = entrance_exam - 70,
         below_cutoff = entrance_exam <= 70)

ggplot(fuzzydat, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  # Add a line based on a linear model for the people scoring 70 or less
  geom_smooth(data = filter(fuzzydat, entrance_exam <= 70), method = "lm") +
  # Add a line based on a linear model for the people scoring more than 70
  geom_smooth(data = filter(fuzzydat, entrance_exam > 70), method = "lm") +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring")


###WRONG MODEL
model_sans_instrument <- lm(exit_exam ~ entrance_centered + tutoring,
                            data = filter(tutoring_centered,
                                          entrance_centered >= -10 & 
                                            entrance_centered <= 10))


###FUZZY RDD WITH BANDWITH +-10
model_fuzzy <- iv_robust(
  exit_exam ~ entrance_centered + tutoring | entrance_centered + below_cutoff,
  data = filter(tutoring_centered, entrance_centered >= -10 & entrance_centered <= 10)
)


modelsummary(list("Wrong Model" = model_sans_instrument, "Fuzzy RD (bw = 10)" = model_fuzzy),
             gof_omit = 'IC|Log|Adj|p\\.value|statistic|se_type',
             stars = TRUE)


# summary(rdrobust(y = fuzzydat$exit_exam, x = fuzzydat$entrance_exam, 
#          c = 70, fuzzy = fuzzydat$tutoring))

