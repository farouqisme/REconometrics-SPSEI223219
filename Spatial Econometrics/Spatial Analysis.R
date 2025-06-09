rm(list=ls())

library(rio)
library(dplyr)
library(tidyverse)
library(sf) ## import Shp
library(ggplot2)
library(ggspatial) ## add compass arrow
library(spdep) ## spatial autocorrelation test
library(spatialreg) ## spatial econometrics
library(geosphere) ##for distance-based est W-matrix

##-------- Import data
indo_shp <- sf::st_read("idn_adm_bps_20200401_shp/idn_admbnda_adm2_bps_20200401.shp")
materi <- rio::import("materi_spat_econ2021.xlsx")

##-------- Merge dataset
dataset <- indo_shp %>% dplyr::select(., -c(1:3,5:14)) %>%
  mutate(ADM2_PCODE = as.numeric(gsub("ID","",ADM2_PCODE))
                               ) %>%
  left_join(.,materi,by=c("ADM2_PCODE")) %>% 
  drop_na()
    
    
##-------- Create a Thematic Map
dataset %>% ggplot() +
  geom_sf(data = dataset, aes(fill = rgdp_const), color = NA)

## make it more presentable
dataset %>% ggplot() +
  ## geom_sf to visualize a map
  geom_sf(data = dataset, aes(fill = log(rgdp_const)), color = NA) +
  ## customize scale's color
  scale_fill_viridis_c(option = "mako", 
                       na.value = "grey80") +
  theme_minimal() +
  ## customize a title, subtitle, x & y label, and legend
  labs(
    title = paste("Regional GDP in Java Island 2021 (Log Constant Price)"),
    subtitle = paste("Retrieved from BPS"),
    fill = "RGDP Constant Price"
  ) + xlab("Longitude") + ylab("Latitude") +
  ## customize the background, layout, etc
  theme(plot.title = element_text(size = 10,face="bold"),
        plot.subtitle = element_text(size = 9,face = "italic"),
        panel.background = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.title.position = "top",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.75)
  ) +
  ## add north arrow
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.09, "in"), pad_y = unit(0.09, "in"),
                         style = north_arrow_fancy_orienteering) 
  

##-------- Create W-matrix
sf_use_s2(FALSE)
## compute regional's centroid and find the coordinate
centroid_dat <- st_coordinates(st_centroid(dataset$geometry))

####---- Contiguity-based
## rook
rook <- poly2nb(dataset, queen = F)
listw_r <- nb2listw(rook, zero.policy = TRUE) ## zero.policy = T to permit neighbourless area

## queen
queen <- poly2nb(dataset, queen = T)
listw_q <- nb2listw(queen, zero.policy = TRUE)

####---- Distance-based


## create inverse distance based on Haversine distance
distance_matrix <- 1/(distm(centroid_dat[, c(1,2)], fun = distHaversine))*1000 
inf.detect <- which(distance_matrix == Inf, arr.ind = T) 
distance_matrix[inf.detect] <- 0

##Transforming  matrix into list
listw <- mat2listw(distance_matrix, style = "W") 


##-------- Spatial Autocorrelation: Global
####---- Moran's I

glob_moran <- moran.test(dataset$rgdp_const, listw)
glob_moran

####---- Geary's C
glob_geary <- geary.test(dataset$rgdp_const, listw)
glob_geary



##-------- Spatial Autocorrelation: Local
## Spatial Corr
oid <- order(dataset$ADM2_PCODE) ##<- create a consistent id based on the order for LISA


####---- Local Moran's I
## Estimation
loc_moran_list <- localmoran(log(dataset$rgdp_const), listw)

## Merge est. result with master data
loc_moran <- data.frame(loc_moran_list[oid,], "ADM2_PCODE" = dataset$ADM2_PCODE[oid])

## Classify insignificant result
loc_moran <- loc_moran %>% mutate(sign = if_else(Pr.z....E.Ii.. <= 0.05,Ii,NA))

## Transform result into cluster classification
cluster <- attributes(loc_moran_list)$quadr$mean
loc_moran <- left_join(dataset,loc_moran, by = c("ADM2_PCODE")) %>% cbind(.,as.data.frame(cluster)) %>% 
  mutate(cluster = as.character(cluster))
loc_moran <- loc_moran %>% mutate(cluster = if_else(Pr.z....E.Ii.. <= 0.05, cluster,"Not Significant"))

## Visualize the cluster
loc_moran %>% ggplot() +
  geom_sf(data = loc_moran, aes(fill = cluster), color = NA) +
  scale_fill_manual(
    values = c("High-High" = "#F20C0C", "High-Low" = "#F29F05", "Low-High" = "#C8F2C2", "Low-Low" = "#143C8C", "Not Significant" = "#D9D9D9"),
    breaks = c("High-High", "High-Low", "Low-High", "Low-Low", "Not Significant"),
    labels = c("High-High" = "High-High", "High-Low" = "High-Low", "Low-High" = "Low-High", "Low-Low" = "Low-Low", "Not Significant" = "Not Significant"),
    na.value = "#fff"
  )+
  theme_minimal() +
  labs(
    title = "Local Moran's I Cluster for Regional GDP 2021 (Constant Price)",
    caption = "* = P-value < 0.05",
    fill = "Cluster"
  ) + xlab("Longitude") + ylab("Latitude") +
  theme(plot.title = element_text(size = 10,face="bold"),
        plot.subtitle = element_text(size = 9,face = "italic"),
        plot.caption = element_text(size = 8, face = "italic", hjust = 1),
        panel.background = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.title.position = "top",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.75)
  ) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.09, "in"), pad_y = unit(0.09, "in"),
                         style = north_arrow_fancy_orienteering) 



####---- Local Geary's C
## Estimation
loc_geary_list <- localC(log(dataset$rgdp_const), listw)

## Merge est. result with master dataset
loc_geary <- data.frame("lgeary" = as.data.frame(loc_geary_list)[oid,], "ADM2_PCODE" = dataset$ADM2_PCODE[oid])
loc_geary <- left_join(dataset,loc_geary, by = c("ADM2_PCODE")) 

## Visualize the value
loc_geary %>% ggplot() +
  geom_sf(data = loc_geary, aes(fill = lgeary), color = NA) +
  scale_fill_viridis_c(option = "cividis",
                           na.value = "#fff") +
  theme_minimal() +
  labs(
    title = paste("Local Geary's C Statistics for Regional GDP 2021 (Constant Price)"),
    fill = "Statistics"
  ) + xlab("Longitude") + ylab("Latitude") +
  theme(plot.title = element_text(size = 10,face="bold"),
        plot.subtitle = element_text(size = 9,face = "italic"),
        panel.background = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_text(size=8), 
        axis.title.x = element_text(size = 8),
        legend.title = element_text(size = 9),
        legend.title.position = "top",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.75)
  ) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_x = unit(0.09, "in"), pad_y = unit(0.09, "in"),
                         style = north_arrow_fancy_orienteering) 


##-------- Spatial Econometrics (Model selection in accordance with Elhorst (2010))
## simple OLS
model <- log(rgdp_const) ~ log(A_rgdp_const) + log(C_rgdp_const) + ipm + poverty + labor_force_rate
ols_est <- lm(model, data = dataset)
summary(ols_est)

## test for spatial autocorrelation in residual
lm.morantest(ols_est, listw, alternative = "two.sided")

## test for spatial autocorrelation both in lagged dependent and residual
lm.RStests(ols_est, listw, test = "all")
## RSerr = error dependence
## RSlag = missing spatially lagged dependent variable
## adjRSerr = tests for error dependence in the possible presence of a missing lagged dependent variable
## adjRSlag = tests for missing lagged dependent variable in the possible presence of an error dependence
## SARMA = Portmanteau Test


## Found that the spatial dependence is detected in \lambda but not in \rho (according to adjRSlag)
## thus, we can proceed to estimate using SDM
sdm <- spatialreg::lagsarlm(model, dataset,  listw, type = "Durbin")
summary(sdm)

## estimating SEM
sem <- spatialreg::errorsarlm(model, dataset, listw)
summary(sem)

## estimating SAR
sar <- spatialreg::lagsarlm(model,dataset, listw)
summary(sar)

## calculate LR test to find the best model
spatialreg::LR.Sarlm(sdm,sem) #<- rejecting H0, thus we use SDM



modelsummary::modelsummary(list("OLS" = ols_est, "SAR" = sar, "SEM" = sem, "SDM" = sdm),
                                      stars = TRUE)

