
library(openxlsx)   ##import & export data format xlsx
library(dplyr)      ##Data cleaning
library(tidyverse)  ##Data cleaning & pipe operator "%>%" 
library(ggplot2)    ##Visualisasi
library(psych)      ##statiscs descriptive

###IMPORT DATA
dat1 <- read.xlsx("Data Cleaning.xlsx", sheet = "data 1")
dat2 <- read.xlsx("Data Cleaning.xlsx", sheet = "data 2")
dat3 <- read.xlsx("Data Cleaning.xlsx", sheet = "data 3")
dat4 <- read.xlsx("Data Cleaning.xlsx", sheet = "data 4")

###MERGE
dat21 <- left_join(dat1,dat2, by = c("Nama.Wilayah","tahun","KodeWilayah"))
dat22 <- left_join(dat3,dat4, by = c("Nama.Wilayah","tahun","KodeWilayah"))

###APPEND
dat_sum <- rbind(dat21,dat22)



###DATA PREPARATION
dat_sum <- dat_sum[!is.na(dat_sum$NamaPulau),] ##<- Drop NA

dat_jawsum <- dat_sum %>% 
  filter(NamaPulau == c("Pulau Jawa-Bali","Pulau Sumatera")) ##<- Keep Pulau Jawa & Pulau Sumatera

###DESCRIPTIVE STATISTICS
describe(dat_sum)

###EXPORT DATA
write.xlsx(dat_sum, "Cleaned Data.xlsx")

###AGGREGATING
dat_agr <- dat_jawsum %>% group_by(tahun, NamaPulau) %>%
  summarise(
    ting_miskin = mean(miskin, na.rm = T)
    )

###VISUALIZATION
viz_bar <- dat_agr %>% 
  ggplot(., aes(x = tahun, y = ting_miskin, fill = NamaPulau)) +
  ### stat untuk mengarahkan value di y dan position untuk 
  ### menyusun fill secara urut berdasarkan tahun
  geom_bar(stat = "identity", position = "dodge") + 
  ### Labeling pada setiap axis
  labs(title = "Perbandingan Tingkat Kemiskinan Antar Pulau",
       subtitle = "Sumber data: BPS",
       x = "Pulau", y = "Poverty Level (%)", fill = "Tahun")

### Export plot
ggsave(filename = "plot tingkat kemiskinan.png", plot = viz_bar, width = 6, height = 4)


