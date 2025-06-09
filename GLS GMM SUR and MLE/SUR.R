library(foreign)
library(systemfit)

hsb2 <- read.dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")

r1 <- read~female + as.numeric(ses) + socst
r2 <- math~female + as.numeric(ses) + science

summary(lm(r1, data=hsb2))
summary(lm(r2, data=hsb2))
fitsur <- systemfit(list(readreg = r1, mathreg = r2), method = "SUR", data=hsb2)
summary(fitsur)



