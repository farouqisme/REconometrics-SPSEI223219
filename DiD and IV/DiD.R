library(dplyr)
library(tidyverse)
library(wooldridge)
library(modelsummary)
data("kielmc", package = "wooldridge")


########################################################pre-post & with-without
withwithout <- lm(lprice~nearinc, data = kielmc[kielmc$year==1981,])
prepost <- lm(lprice~as.factor(year), data = kielmc)
prepost_T <- lm(lprice~as.factor(year), data = kielmc[kielmc$nearinc==1,])
prepost_C <- lm(lprice~as.factor(year), data = kielmc[kielmc$nearinc==0,])


modelsummary(list("With & Without" = withwithout, "Pre-Post" = prepost),
                           gof_omit = 'IC|Log|Adj|p\\.value|statistic|se_type',
                           stars = TRUE)

modelsummary(list("Pre-Post" = prepost, "Pre-Post Treat" = prepost_T, 
                  "Pre-Post Control" = prepost_C),
             gof_omit = 'IC|Log|Adj|p\\.value|statistic|se_type',
             stars = TRUE)

########################################################diff-in-diffs (basic)
diffindiffs <- lm(lprice~nearinc*y81, data = kielmc)
diffindiffs_WC <- lm(lprice~nearinc*y81+lintst+rooms+larea+lland+age+agesq, data = kielmc)

modelsummary(list("Diff-in-Diffs" = diffindiffs, "Diff-in-Diifs with contr" = diffindiffs_WC),
             gof_omit = 'IC|Log|Adj|p\\.value|statistic|se_type',
             stars = TRUE)

########################################################diff-in-diffs (package: did)
library(did)

data(mpdta)
out1 <- att_gt(yname="lemp", #Y
               tname="year", #time
               idname="countyreal", #individual's id
               gname="first.treat", #first year exposed by the treatment
               xformla=~lpop, #X
               #the available options are nevertreated & notyettreated
               #nevertreated = group that never participate in the treaetment (doesn't change over period)
               #notyettreated = that have not yet participated in the treatment in that time period
               control_group = "nevertreated", 
               data=mpdta) 
summary(out1)



