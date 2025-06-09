library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(rdrobust)  # For robust nonparametric regression discontinuity
library(estimatr)  # Run 2SLS models in one step with iv_robust()
library(modelsummary)  # Create side-by-side regression tables
library(rdd)


sharpdat <- read.csv("tutoring_program_sharp.csv", header = T) 

ggplot(sharpdat, aes(x = entrance_exam, y = tutoring, color = tutoring)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(size = 0.5, alpha = 0.5) + 
  # Add vertical line
  geom_vline(xintercept = 70) + 
  # Add labels
  labs(x = "Entrance exam score", y = "Participated in tutoring program") + 
  # Turn off the color legend, since it's redundant
  guides(color = FALSE)

sharpdat %>% 
  group_by(tutoring, entrance_exam <= 70) %>% 
  summarize(count = n())


ggplot(sharpdat, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 0.5, alpha = 0.5) + 
  # Add a line based on a linear model for the people scoring 70 or less
  geom_smooth(data = filter(sharpdat, entrance_exam <= 70), method = "lm") +
  # Add a line based on a linear model for the people scoring more than 70
  geom_smooth(data = filter(sharpdat, entrance_exam > 70), method = "lm") +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring")


tutoring_centered <- sharpdat %>% 
  mutate(entrance_centered = entrance_exam - 70)

# Test optimal Bandwith -> The objective is to minimize the mean squared error between the estimated and actual treatment effects.
IKbandwidth(tutoring_centered$entrance_exam,
            tutoring_centered$exit_exam,
                 cutpoint = 70,
                 kernel = "triangular")

model_bw_8 <- lm(exit_exam ~ entrance_centered + tutoring,
                 data = filter(tutoring_centered,
                               entrance_centered >= -8 & 
                                 entrance_centered <= 8))

model_bw_10 <- lm(exit_exam ~ entrance_centered + tutoring,
                  data = filter(tutoring_centered,
                                entrance_centered >= -10 & 
                                  entrance_centered <= 10))

model_bw_15 <- lm(exit_exam ~ entrance_centered + tutoring,
                  data = filter(tutoring_centered,
                                entrance_centered >= -15 & 
                                  entrance_centered <= 15))

modelsummary(list("Sharp RD (bw = 8)" = model_bw_8, "Sharp RD (bw = 10)" = model_bw_10, "Sharp RD (bw = 15)" = model_bw_15),
             gof_omit = 'IC|Log|Adj|p\\.value|statistic|se_type',
             stars = TRUE)


summary(rdrobust(y = sharpdat$exit_exam, x = sharpdat$entrance_exam, c = 70))

