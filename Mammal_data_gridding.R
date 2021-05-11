## Gridding for mammal sexual size dimorphism study 
#The datasets: "Mammal_spatial_BM_extended.csv" and "Mammal_spatial_HBL_extended.csv" are from the C"Mammal_spatial" study 

###These datasets have already been cleaned and include vernet, NEON and NACSM records 

##Still want to add in P. keeni and any others?? 

######################################################

library(sf)
library(raster)
library(tidyverse)
library(dplyr)

setwd("/Users/Maggie/Dropbox/Mammal_SSD/Mammal_SSD_github/")

##################
#Mammal BM data 
Mammal_BM <- read.csv("Mammal_spatial_BM_extended.csv", header = TRUE, stringsAsFactors = FALSE)
str(Mammal_BM)
plyr::count(Mammal_BM$binomial2)
#plyr::count(Mammal_BM$institutioncode)

###Subset to the species of interest 
Mammal_BM_SSD <- Mammal_BM %>% filter(binomial2 == 'Microtus_californicus' | binomial2 == 'Microtus_longicaudus' | binomial2 == 'Microtus_montanus' |
                                        binomial2 == 'Microtus_ochrogaster' | binomial2 == 'Microtus_oregoni' | binomial2 == 'Microtus_pennsylvanicus' |
                                        binomial2 == 'Microtus_pinetorum' | binomial2 == 'Microtus_townsendii' |
                                        binomial2 == 'Peromyscus_boylii' | binomial2 == 'Peromyscus_crinitus' | binomial2 == 'Peromyscus_eremicus' |
                                        binomial2 == 'Peromyscus_leucopus' | binomial2 == 'Peromyscus_maniculatus' | binomial2 == 'Peromyscus_truei') 
plyr::count(Mammal_BM_SSD$binomial2)

table(Mammal_BM_SSD$sex)
#is.na(Mammal_BM_SSD$decimallatitude) #all look fine 

##################
#Mammal HBL data 
Mammal_HBL <- read.csv("Mammal_spatial_HBL_extended.csv", header = TRUE, stringsAsFactors = FALSE)
str(Mammal_HBL)
plyr::count(Mammal_HBL$binomial2)
#plyr::count(Mammal_HBL$institutioncode)

###Subset to the species of interest 
Mammal_HBL_SSD <- Mammal_HBL %>% filter(binomial2 == 'Microtus_californicus' | binomial2 == 'Microtus_longicaudus' | binomial2 == 'Microtus_montanus' |
                                        binomial2 == 'Microtus_ochrogaster' | binomial2 == 'Microtus_oregoni' | binomial2 == 'Microtus_pennsylvanicus' |
                                        binomial2 == 'Microtus_pinetorum' | binomial2 == 'Microtus_townsendii' |
                                        binomial2 == 'Peromyscus_boylii' | binomial2 == 'Peromyscus_crinitus' | binomial2 == 'Peromyscus_eremicus' |
                                        binomial2 == 'Peromyscus_leucopus' | binomial2 == 'Peromyscus_maniculatus' | binomial2 == 'Peromyscus_truei') 
plyr::count(Mammal_HBL_SSD$binomial2)

table(Mammal_HBL_SSD$sex)
############################################################################################################

###Grids
mammal_dist <- rnaturalearth::ne_countries(country = c("United States of America", "Mexico", "Canada"),returnclass = "sf")
plot(mammal_dist)
mammal_dist <-st_transform(mammal_dist, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(mammal_dist)

grids_100000 <- st_make_grid(mammal_dist, cellsize = c(100000, 100000)) %>% st_intersection(mammal_dist) 
plot(grids_100000)
grids_100000 <- mutate(st_sf(geometry = grids_100000), id_cells = 1:n())
plot(grids_100000)

grids_150000 <- st_make_grid(mammal_dist, cellsize = c(150000, 150000)) %>% st_intersection(mammal_dist) 
plot(grids_150000)
grids_150000 <- mutate(st_sf(geometry = grids_150000), id_cells = 1:n())
plot(grids_150000)

grids_75000 <- st_make_grid(mammal_dist, cellsize = c(75000, 75000)) %>% st_intersection(mammal_dist) 
plot(grids_75000)
grids_75000 <- mutate(st_sf(geometry = grids_75000), id_cells = 1:n())
plot(grids_75000)

###########################################
#Grid mammal BM
#coords=1,2 long and lat
BM_grid <- st_transform(st_as_sf(Mammal_BM_SSD, coords=1:2, crs =4326), crs=st_crs(mammal_dist)$proj4string)
#view(BM_grid)
BM_grid_cells = st_join(BM_grid, grids_100000)
#view(BM_grid_cells)
str(BM_grid_cells)
BM_grid_cells <- BM_grid_cells %>% drop_na(id_cells) 
#BM_grid_cells$X <- NULL

#write_csv(BM_grid_cells, "Mammal_SSD_BM_grids_100000.csv")

#############
#Grid mammal HBL
#coords=1,2 long and lat
HBL_grid <- st_transform(st_as_sf(Mammal_HBL_SSD, coords=1:2, crs =4326), crs=st_crs(mammal_dist)$proj4string)
#view(HBL_grid)
HBL_grid_cells = st_join(HBL_grid, grids_100000)
#view(HBL_grid_cells)
str(HBL_grid_cells)
HBL_grid_cells <- HBL_grid_cells %>% drop_na(id_cells) 

#write_csv(HBL_grid_cells, "Mammal_SSD_HBL_grids_100000.csv")

################################################

#Grid mammal BM
#coords=1,2 long and lat
BM_grid2 <- st_transform(st_as_sf(Mammal_BM_SSD, coords=1:2, crs =4326), crs=st_crs(mammal_dist)$proj4string)
#view(BM_grid2)
BM_grid_cells2 = st_join(BM_grid2, grids_150000)
#view(BM_grid_cells2)
str(BM_grid_cells2)
BM_grid_cells2 <- BM_grid_cells2 %>% drop_na(id_cells) 

#write_csv(BM_grid_cells2, "Mammal_SSD_BM_grids_150000.csv")

#############
#Grid mammal HBL
#coords=1,2 long and lat
HBL_grid2 <- st_transform(st_as_sf(Mammal_HBL_SSD, coords=1:2, crs =4326), crs=st_crs(mammal_dist)$proj4string)
#view(HBL_grid2)
HBL_grid_cells2 = st_join(HBL_grid2, grids_150000)
#view(HBL_grid_cells2)
str(HBL_grid_cells2)
HBL_grid_cells2 <- HBL_grid_cells2 %>% drop_na(id_cells) 

#write_csv(HBL_grid_cells2, "Mammal_SSD_HBL_grids_150000.csv")

##################################

#Grid mammal BM
#coords=1,2 long and lat
BM_grid3 <- st_transform(st_as_sf(Mammal_BM_SSD, coords=1:2, crs =4326), crs=st_crs(mammal_dist)$proj4string)
#view(BM_grid3)
BM_grid_cells3 = st_join(BM_grid3, grids_75000)
#view(BM_grid_cells3)
str(BM_grid_cells3)
BM_grid_cells3 <- BM_grid_cells3 %>% drop_na(id_cells) 

#write_csv(BM_grid_cells3, "Mammal_SSD_BM_grids_75000.csv")

#############
#Grid mammal HBL
#coords=1,2 long and lat
HBL_grid3 <- st_transform(st_as_sf(Mammal_HBL_SSD, coords=1:2, crs =4326), crs=st_crs(mammal_dist)$proj4string)
#view(HBL_grid3)
HBL_grid_cells3 = st_join(HBL_grid3, grids_75000)
#view(HBL_grid_cells3)
str(HBL_grid_cells3)
HBL_grid_cells3 <- HBL_grid_cells3 %>% drop_na(id_cells) 

#write_csv(HBL_grid_cells3, "Mammal_SSD_HBL_grids_75000.csv")



