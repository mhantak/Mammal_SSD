## Mammal sexual size dimorphism study 
#The datasets: "Mammal_spatial_BM_extended.csv" and "Mammal_spatial_HBL_extended.csv" are from the "Mammal_spatial" study 
#Gridded in the "Mammal_data_gridding" script

##Still want to add in P. keeni and any others?? 

######################################################
library(tidyverse)
library(dplyr)
library(sf)

setwd("/Users/Maggie/Dropbox/Mammal_SSD/Mammal_SSD_github/")

#Mammal BM data 
Mam_BM <- read.csv("Mammal_SSD_BM_grids_100000.csv", header = TRUE, stringsAsFactors = FALSE)
str(Mam_BM)
plyr::count(Mam_BM$binomial2)
#plyr::count(Mam_BM$source)

avg_bm_species <- Mam_BM %>%
  group_by(binomial2, sex) %>%
  dplyr::summarize(Mean_BM = mean(X1st_body_mass, na.rm=TRUE)) 

#Mammal HBL data 
Mam_HBL <- read.csv("Mammal_SSD_HBL_grids_100000.csv", header = TRUE, stringsAsFactors = FALSE)
str(Mam_HBL)
plyr::count(Mam_HBL$binomial2)
#plyr::count(Mam_HBL$source)

avg_hbl_species <- Mam_HBL %>%
  group_by(binomial2, sex) %>%
  dplyr::summarize(Mean_HBL = mean(HB.length, na.rm=TRUE)) 

########################################################################################################################
###Filter by species & sex
Microtus_californicus_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_californicus')
Microtus_californicus_BM_male <- Microtus_californicus_BM %>% filter(sex == "male")
Microtus_californicus_BM_female <- Microtus_californicus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_californicus_BM_min <- Microtus_californicus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_californicus_BM_min$sex)

Male.bm.Microtus_californicus = Microtus_californicus_BM_min[Microtus_californicus_BM_min$sex == "male", ] 
Female.bm.Microtus_californicus = Microtus_californicus_BM_min[Microtus_californicus_BM_min$sex == "female", ] 

Male.bm.Microtus_californicus2 <- right_join(Male.bm.Microtus_californicus, Microtus_californicus_BM_male)
Male.bm.Microtus_californicus2 <- Male.bm.Microtus_californicus2 %>% drop_na(n) 

Female.bm.Microtus_californicus2 <- right_join(Female.bm.Microtus_californicus, Microtus_californicus_BM_female)
Female.bm.Microtus_californicus2 <- Female.bm.Microtus_californicus2 %>% drop_na(n) 

Male.bm.Microtus_californicus3 <- Male.bm.Microtus_californicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_californicus3 <- Female.bm.Microtus_californicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_californicus_data_BM <- full_join(Male.bm.Microtus_californicus3, Female.bm.Microtus_californicus3)
str(Microtus_californicus_data_BM)
Microtus_californicus_data_BM <- Microtus_californicus_data_BM %>% drop_na() 
nrow(Microtus_californicus_data_BM)

#rensch's rule
BM_rensch_Microtus_californicus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_californicus_data_BM)
summary(BM_rensch_Microtus_californicus)
confint(BM_rensch_Microtus_californicus)

ggplot(data = Microtus_californicus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(3.5,4.15), ylim = c(3.5,4.15), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_californicus_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_californicus')
Microtus_californicus_HBL_male <- Microtus_californicus_HBL %>% filter(sex == "male")
Microtus_californicus_HBL_female <- Microtus_californicus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_californicus_HBL_min <- Microtus_californicus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_californicus_HBL_min$sex)

Male.HBL.Microtus_californicus = Microtus_californicus_HBL_min[Microtus_californicus_HBL_min$sex == "male", ] 
Female.HBL.Microtus_californicus = Microtus_californicus_HBL_min[Microtus_californicus_HBL_min$sex == "female", ] 

Male.HBL.Microtus_californicus2 <- right_join(Male.HBL.Microtus_californicus, Microtus_californicus_HBL_male)
Male.HBL.Microtus_californicus2 <- Male.HBL.Microtus_californicus2 %>% drop_na(n) 

Female.HBL.Microtus_californicus2 <- right_join(Female.HBL.Microtus_californicus, Microtus_californicus_HBL_female)
Female.HBL.Microtus_californicus2 <- Female.HBL.Microtus_californicus2 %>% drop_na(n) 

Male.HBL.Microtus_californicus3 <- Male.HBL.Microtus_californicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_californicus3 <- Female.HBL.Microtus_californicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_californicus_data_HBL <- full_join(Male.HBL.Microtus_californicus3, Female.HBL.Microtus_californicus3)
str(Microtus_californicus_data_HBL)
Microtus_californicus_data_HBL <- Microtus_californicus_data_HBL %>% drop_na() 
nrow(Microtus_californicus_data_HBL)

#rensch's rule
HBL_rensch_Microtus_californicus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_californicus_data_HBL)
summary(HBL_rensch_Microtus_californicus)
confint(HBL_rensch_Microtus_californicus)

ggplot(data = Microtus_californicus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.6,5), ylim = c(4.6,5), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################
###Filter by species & sex
Microtus_longicaudus_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_longicaudus')
Microtus_longicaudus_BM_male <- Microtus_longicaudus_BM %>% filter(sex == "male")
Microtus_longicaudus_BM_female <- Microtus_longicaudus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_longicaudus_BM_min <- Microtus_longicaudus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_longicaudus_BM_min$sex)

Male.bm.Microtus_longicaudus = Microtus_longicaudus_BM_min[Microtus_longicaudus_BM_min$sex == "male", ] 
Female.bm.Microtus_longicaudus = Microtus_longicaudus_BM_min[Microtus_longicaudus_BM_min$sex == "female", ] 

Male.bm.Microtus_longicaudus2 <- right_join(Male.bm.Microtus_longicaudus, Microtus_longicaudus_BM_male)
Male.bm.Microtus_longicaudus2 <- Male.bm.Microtus_longicaudus2 %>% drop_na(n) 

Female.bm.Microtus_longicaudus2 <- right_join(Female.bm.Microtus_longicaudus, Microtus_longicaudus_BM_female)
Female.bm.Microtus_longicaudus2 <- Female.bm.Microtus_longicaudus2 %>% drop_na(n) 

Male.bm.Microtus_longicaudus3 <- Male.bm.Microtus_longicaudus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_longicaudus3 <- Female.bm.Microtus_longicaudus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_longicaudus_data_BM <- full_join(Male.bm.Microtus_longicaudus3, Female.bm.Microtus_longicaudus3)
str(Microtus_longicaudus_data_BM)
Microtus_longicaudus_data_BM <- Microtus_longicaudus_data_BM %>% drop_na() 
nrow(Microtus_longicaudus_data_BM)

#rensch's rule
BM_rensch_Microtus_longicaudus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_longicaudus_data_BM)
summary(BM_rensch_Microtus_longicaudus)
confint(BM_rensch_Microtus_longicaudus)

ggplot(data = Microtus_longicaudus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(3,4), ylim = c(3,4), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_longicaudus_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_longicaudus')
Microtus_longicaudus_HBL_male <- Microtus_longicaudus_HBL %>% filter(sex == "male")
Microtus_longicaudus_HBL_female <- Microtus_longicaudus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_longicaudus_HBL_min <- Microtus_longicaudus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_longicaudus_HBL_min$sex)

Male.HBL.Microtus_longicaudus = Microtus_longicaudus_HBL_min[Microtus_longicaudus_HBL_min$sex == "male", ] 
Female.HBL.Microtus_longicaudus = Microtus_longicaudus_HBL_min[Microtus_longicaudus_HBL_min$sex == "female", ] 

Male.HBL.Microtus_longicaudus2 <- right_join(Male.HBL.Microtus_longicaudus, Microtus_longicaudus_HBL_male)
Male.HBL.Microtus_longicaudus2 <- Male.HBL.Microtus_longicaudus2 %>% drop_na(n) 

Female.HBL.Microtus_longicaudus2 <- right_join(Female.HBL.Microtus_longicaudus, Microtus_longicaudus_HBL_female)
Female.HBL.Microtus_longicaudus2 <- Female.HBL.Microtus_longicaudus2 %>% drop_na(n) 

Male.HBL.Microtus_longicaudus3 <- Male.HBL.Microtus_longicaudus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_longicaudus3 <- Female.HBL.Microtus_longicaudus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_longicaudus_data_HBL <- full_join(Male.HBL.Microtus_longicaudus3, Female.HBL.Microtus_longicaudus3)
str(Microtus_longicaudus_data_HBL)
Microtus_longicaudus_data_HBL <- Microtus_longicaudus_data_HBL %>% drop_na() 
nrow(Microtus_longicaudus_data_HBL)

#rensch's rule
HBL_rensch_Microtus_longicaudus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_longicaudus_data_HBL)
summary(HBL_rensch_Microtus_longicaudus)
confint(HBL_rensch_Microtus_longicaudus)

ggplot(data = Microtus_longicaudus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.5,4.9), ylim = c(4.5,4.9), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

########################################################################################################################

###Filter by species & sex
Microtus_montanus_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_montanus')
Microtus_montanus_BM_male <- Microtus_montanus_BM %>% filter(sex == "male")
Microtus_montanus_BM_female <- Microtus_montanus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_montanus_BM_min <- Microtus_montanus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_montanus_BM_min$sex)

Male.bm.Microtus_montanus = Microtus_montanus_BM_min[Microtus_montanus_BM_min$sex == "male", ] 
Female.bm.Microtus_montanus = Microtus_montanus_BM_min[Microtus_montanus_BM_min$sex == "female", ] 

Male.bm.Microtus_montanus2 <- right_join(Male.bm.Microtus_montanus, Microtus_montanus_BM_male)
Male.bm.Microtus_montanus2 <- Male.bm.Microtus_montanus2 %>% drop_na(n) 

Female.bm.Microtus_montanus2 <- right_join(Female.bm.Microtus_montanus, Microtus_montanus_BM_female)
Female.bm.Microtus_montanus2 <- Female.bm.Microtus_montanus2 %>% drop_na(n) 

Male.bm.Microtus_montanus3 <- Male.bm.Microtus_montanus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_montanus3 <- Female.bm.Microtus_montanus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_montanus_data_BM <- full_join(Male.bm.Microtus_montanus3, Female.bm.Microtus_montanus3)
str(Microtus_montanus_data_BM)
Microtus_montanus_data_BM <- Microtus_montanus_data_BM %>% drop_na() 
nrow(Microtus_montanus_data_BM)

#rensch's rule
BM_rensch_Microtus_montanus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_montanus_data_BM)
summary(BM_rensch_Microtus_montanus)
confint(BM_rensch_Microtus_montanus)

ggplot(data = Microtus_montanus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(3.5,4.3), ylim = c(3.5,4.3), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_montanus_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_montanus')
Microtus_montanus_HBL_male <- Microtus_montanus_HBL %>% filter(sex == "male")
Microtus_montanus_HBL_female <- Microtus_montanus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_montanus_HBL_min <- Microtus_montanus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_montanus_HBL_min$sex)

Male.HBL.Microtus_montanus = Microtus_montanus_HBL_min[Microtus_montanus_HBL_min$sex == "male", ] 
Female.HBL.Microtus_montanus = Microtus_montanus_HBL_min[Microtus_montanus_HBL_min$sex == "female", ] 

Male.HBL.Microtus_montanus2 <- right_join(Male.HBL.Microtus_montanus, Microtus_montanus_HBL_male)
Male.HBL.Microtus_montanus2 <- Male.HBL.Microtus_montanus2 %>% drop_na(n) 

Female.HBL.Microtus_montanus2 <- right_join(Female.HBL.Microtus_montanus, Microtus_montanus_HBL_female)
Female.HBL.Microtus_montanus2 <- Female.HBL.Microtus_montanus2 %>% drop_na(n) 

Male.HBL.Microtus_montanus3 <- Male.HBL.Microtus_montanus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_montanus3 <- Female.HBL.Microtus_montanus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_montanus_data_HBL <- full_join(Male.HBL.Microtus_montanus3, Female.HBL.Microtus_montanus3)
str(Microtus_montanus_data_HBL)
Microtus_montanus_data_HBL <- Microtus_montanus_data_HBL %>% drop_na() 
nrow(Microtus_montanus_data_HBL)

#rensch's rule
HBL_rensch_Microtus_montanus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_montanus_data_HBL)
summary(HBL_rensch_Microtus_montanus)
confint(HBL_rensch_Microtus_montanus)

ggplot(data = Microtus_montanus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.4,4.95), ylim = c(4.4,4.95), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Microtus_ochrogaster_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_ochrogaster')
Microtus_ochrogaster_BM_male <- Microtus_ochrogaster_BM %>% filter(sex == "male")
Microtus_ochrogaster_BM_female <- Microtus_ochrogaster_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_ochrogaster_BM_min <- Microtus_ochrogaster_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_ochrogaster_BM_min$sex)

Male.bm.Microtus_ochrogaster = Microtus_ochrogaster_BM_min[Microtus_ochrogaster_BM_min$sex == "male", ] 
Female.bm.Microtus_ochrogaster = Microtus_ochrogaster_BM_min[Microtus_ochrogaster_BM_min$sex == "female", ] 

Male.bm.Microtus_ochrogaster2 <- right_join(Male.bm.Microtus_ochrogaster, Microtus_ochrogaster_BM_male)
Male.bm.Microtus_ochrogaster2 <- Male.bm.Microtus_ochrogaster2 %>% drop_na(n) 

Female.bm.Microtus_ochrogaster2 <- right_join(Female.bm.Microtus_ochrogaster, Microtus_ochrogaster_BM_female)
Female.bm.Microtus_ochrogaster2 <- Female.bm.Microtus_ochrogaster2 %>% drop_na(n) 

Male.bm.Microtus_ochrogaster3 <- Male.bm.Microtus_ochrogaster2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_ochrogaster3 <- Female.bm.Microtus_ochrogaster2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_ochrogaster_data_BM <- full_join(Male.bm.Microtus_ochrogaster3, Female.bm.Microtus_ochrogaster3)
str(Microtus_ochrogaster_data_BM)
Microtus_ochrogaster_data_BM <- Microtus_ochrogaster_data_BM %>% drop_na() 
nrow(Microtus_ochrogaster_data_BM)

#rensch's rule
BM_rensch_Microtus_ochrogaster <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_ochrogaster_data_BM)
summary(BM_rensch_Microtus_ochrogaster)
confint(BM_rensch_Microtus_ochrogaster)

ggplot(data = Microtus_ochrogaster_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(3.3,4.15), ylim = c(3.3,4.15), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_ochrogaster_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_ochrogaster')
Microtus_ochrogaster_HBL_male <- Microtus_ochrogaster_HBL %>% filter(sex == "male")
Microtus_ochrogaster_HBL_female <- Microtus_ochrogaster_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_ochrogaster_HBL_min <- Microtus_ochrogaster_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_ochrogaster_HBL_min$sex)

Male.HBL.Microtus_ochrogaster = Microtus_ochrogaster_HBL_min[Microtus_ochrogaster_HBL_min$sex == "male", ] 
Female.HBL.Microtus_ochrogaster = Microtus_ochrogaster_HBL_min[Microtus_ochrogaster_HBL_min$sex == "female", ] 

Male.HBL.Microtus_ochrogaster2 <- right_join(Male.HBL.Microtus_ochrogaster, Microtus_ochrogaster_HBL_male)
Male.HBL.Microtus_ochrogaster2 <- Male.HBL.Microtus_ochrogaster2 %>% drop_na(n) 

Female.HBL.Microtus_ochrogaster2 <- right_join(Female.HBL.Microtus_ochrogaster, Microtus_ochrogaster_HBL_female)
Female.HBL.Microtus_ochrogaster2 <- Female.HBL.Microtus_ochrogaster2 %>% drop_na(n) 

Male.HBL.Microtus_ochrogaster3 <- Male.HBL.Microtus_ochrogaster2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_ochrogaster3 <- Female.HBL.Microtus_ochrogaster2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_ochrogaster_data_HBL <- full_join(Male.HBL.Microtus_ochrogaster3, Female.HBL.Microtus_ochrogaster3)
str(Microtus_ochrogaster_data_HBL)
Microtus_ochrogaster_data_HBL <- Microtus_ochrogaster_data_HBL %>% drop_na() 
nrow(Microtus_ochrogaster_data_HBL)

#rensch's rule
HBL_rensch_Microtus_ochrogaster <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_ochrogaster_data_HBL)
summary(HBL_rensch_Microtus_ochrogaster)
confint(HBL_rensch_Microtus_ochrogaster)

ggplot(data = Microtus_ochrogaster_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.5,4.9), ylim = c(4.5,4.9), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Microtus_oregoni_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_oregoni')
Microtus_oregoni_BM_male <- Microtus_oregoni_BM %>% filter(sex == "male")
Microtus_oregoni_BM_female <- Microtus_oregoni_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_oregoni_BM_min <- Microtus_oregoni_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_oregoni_BM_min$sex)

Male.bm.Microtus_oregoni = Microtus_oregoni_BM_min[Microtus_oregoni_BM_min$sex == "male", ] 
Female.bm.Microtus_oregoni = Microtus_oregoni_BM_min[Microtus_oregoni_BM_min$sex == "female", ] 

Male.bm.Microtus_oregoni2 <- right_join(Male.bm.Microtus_oregoni, Microtus_oregoni_BM_male)
Male.bm.Microtus_oregoni2 <- Male.bm.Microtus_oregoni2 %>% drop_na(n) 

Female.bm.Microtus_oregoni2 <- right_join(Female.bm.Microtus_oregoni, Microtus_oregoni_BM_female)
Female.bm.Microtus_oregoni2 <- Female.bm.Microtus_oregoni2 %>% drop_na(n) 

Male.bm.Microtus_oregoni3 <- Male.bm.Microtus_oregoni2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_oregoni3 <- Female.bm.Microtus_oregoni2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_oregoni_data_BM <- full_join(Male.bm.Microtus_oregoni3, Female.bm.Microtus_oregoni3)
str(Microtus_oregoni_data_BM)
Microtus_oregoni_data_BM <- Microtus_oregoni_data_BM %>% drop_na() 
nrow(Microtus_oregoni_data_BM)

#rensch's rule
BM_rensch_Microtus_oregoni <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_oregoni_data_BM)
summary(BM_rensch_Microtus_oregoni)
confint(BM_rensch_Microtus_oregoni)

ggplot(data = Microtus_oregoni_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.78,3.4), ylim = c(2.78,3.4), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_oregoni_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_oregoni')
Microtus_oregoni_HBL_male <- Microtus_oregoni_HBL %>% filter(sex == "male")
Microtus_oregoni_HBL_female <- Microtus_oregoni_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_oregoni_HBL_min <- Microtus_oregoni_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_oregoni_HBL_min$sex)

Male.HBL.Microtus_oregoni = Microtus_oregoni_HBL_min[Microtus_oregoni_HBL_min$sex == "male", ] 
Female.HBL.Microtus_oregoni = Microtus_oregoni_HBL_min[Microtus_oregoni_HBL_min$sex == "female", ] 

Male.HBL.Microtus_oregoni2 <- right_join(Male.HBL.Microtus_oregoni, Microtus_oregoni_HBL_male)
Male.HBL.Microtus_oregoni2 <- Male.HBL.Microtus_oregoni2 %>% drop_na(n) 

Female.HBL.Microtus_oregoni2 <- right_join(Female.HBL.Microtus_oregoni, Microtus_oregoni_HBL_female)
Female.HBL.Microtus_oregoni2 <- Female.HBL.Microtus_oregoni2 %>% drop_na(n) 

Male.HBL.Microtus_oregoni3 <- Male.HBL.Microtus_oregoni2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_oregoni3 <- Female.HBL.Microtus_oregoni2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_oregoni_data_HBL <- full_join(Male.HBL.Microtus_oregoni3, Female.HBL.Microtus_oregoni3)
str(Microtus_oregoni_data_HBL)
Microtus_oregoni_data_HBL <- Microtus_oregoni_data_HBL %>% drop_na() 
nrow(Microtus_oregoni_data_HBL)

#rensch's rule
HBL_rensch_Microtus_oregoni <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_oregoni_data_HBL)
summary(HBL_rensch_Microtus_oregoni)
confint(HBL_rensch_Microtus_oregoni)

ggplot(data = Microtus_oregoni_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.4,4.69), ylim = c(4.4,4.69), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Microtus_pennsylvanicus_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_pennsylvanicus')
Microtus_pennsylvanicus_BM_male <- Microtus_pennsylvanicus_BM %>% filter(sex == "male")
Microtus_pennsylvanicus_BM_female <- Microtus_pennsylvanicus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_pennsylvanicus_BM_min <- Microtus_pennsylvanicus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_pennsylvanicus_BM_min$sex)

Male.bm.Microtus_pennsylvanicus = Microtus_pennsylvanicus_BM_min[Microtus_pennsylvanicus_BM_min$sex == "male", ] 
Female.bm.Microtus_pennsylvanicus = Microtus_pennsylvanicus_BM_min[Microtus_pennsylvanicus_BM_min$sex == "female", ] 

Male.bm.Microtus_pennsylvanicus2 <- right_join(Male.bm.Microtus_pennsylvanicus, Microtus_pennsylvanicus_BM_male)
Male.bm.Microtus_pennsylvanicus2 <- Male.bm.Microtus_pennsylvanicus2 %>% drop_na(n) 

Female.bm.Microtus_pennsylvanicus2 <- right_join(Female.bm.Microtus_pennsylvanicus, Microtus_pennsylvanicus_BM_female)
Female.bm.Microtus_pennsylvanicus2 <- Female.bm.Microtus_pennsylvanicus2 %>% drop_na(n) 

Male.bm.Microtus_pennsylvanicus3 <- Male.bm.Microtus_pennsylvanicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_pennsylvanicus3 <- Female.bm.Microtus_pennsylvanicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_pennsylvanicus_data_BM <- full_join(Male.bm.Microtus_pennsylvanicus3, Female.bm.Microtus_pennsylvanicus3)
str(Microtus_pennsylvanicus_data_BM)
Microtus_pennsylvanicus_data_BM <- Microtus_pennsylvanicus_data_BM %>% drop_na() 
nrow(Microtus_pennsylvanicus_data_BM)

#rensch's rule
BM_rensch_Microtus_pennsylvanicus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_pennsylvanicus_data_BM)
summary(BM_rensch_Microtus_pennsylvanicus)
confint(BM_rensch_Microtus_pennsylvanicus)

ggplot(data = Microtus_pennsylvanicus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(3.3,4.1), ylim = c(3.3,4.1), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_pennsylvanicus_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_pennsylvanicus')
Microtus_pennsylvanicus_HBL_male <- Microtus_pennsylvanicus_HBL %>% filter(sex == "male")
Microtus_pennsylvanicus_HBL_female <- Microtus_pennsylvanicus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_pennsylvanicus_HBL_min <- Microtus_pennsylvanicus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_pennsylvanicus_HBL_min$sex)

Male.HBL.Microtus_pennsylvanicus = Microtus_pennsylvanicus_HBL_min[Microtus_pennsylvanicus_HBL_min$sex == "male", ] 
Female.HBL.Microtus_pennsylvanicus = Microtus_pennsylvanicus_HBL_min[Microtus_pennsylvanicus_HBL_min$sex == "female", ] 

Male.HBL.Microtus_pennsylvanicus2 <- right_join(Male.HBL.Microtus_pennsylvanicus, Microtus_pennsylvanicus_HBL_male)
Male.HBL.Microtus_pennsylvanicus2 <- Male.HBL.Microtus_pennsylvanicus2 %>% drop_na(n) 

Female.HBL.Microtus_pennsylvanicus2 <- right_join(Female.HBL.Microtus_pennsylvanicus, Microtus_pennsylvanicus_HBL_female)
Female.HBL.Microtus_pennsylvanicus2 <- Female.HBL.Microtus_pennsylvanicus2 %>% drop_na(n) 

Male.HBL.Microtus_pennsylvanicus3 <- Male.HBL.Microtus_pennsylvanicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_pennsylvanicus3 <- Female.HBL.Microtus_pennsylvanicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_pennsylvanicus_data_HBL <- full_join(Male.HBL.Microtus_pennsylvanicus3, Female.HBL.Microtus_pennsylvanicus3)
str(Microtus_pennsylvanicus_data_HBL)
Microtus_pennsylvanicus_data_HBL <- Microtus_pennsylvanicus_data_HBL %>% drop_na() 
nrow(Microtus_pennsylvanicus_data_HBL)

#rensch's rule
HBL_rensch_Microtus_pennsylvanicus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_pennsylvanicus_data_HBL)
summary(HBL_rensch_Microtus_pennsylvanicus)
confint(HBL_rensch_Microtus_pennsylvanicus)

ggplot(data = Microtus_pennsylvanicus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.5,4.95), ylim = c(4.5,4.95), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Microtus_pinetorum_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_pinetorum')
Microtus_pinetorum_BM_male <- Microtus_pinetorum_BM %>% filter(sex == "male")
Microtus_pinetorum_BM_female <- Microtus_pinetorum_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_pinetorum_BM_min <- Microtus_pinetorum_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_pinetorum_BM_min$sex)

Male.bm.Microtus_pinetorum = Microtus_pinetorum_BM_min[Microtus_pinetorum_BM_min$sex == "male", ] 
Female.bm.Microtus_pinetorum = Microtus_pinetorum_BM_min[Microtus_pinetorum_BM_min$sex == "female", ] 

Male.bm.Microtus_pinetorum2 <- right_join(Male.bm.Microtus_pinetorum, Microtus_pinetorum_BM_male)
Male.bm.Microtus_pinetorum2 <- Male.bm.Microtus_pinetorum2 %>% drop_na(n) 

Female.bm.Microtus_pinetorum2 <- right_join(Female.bm.Microtus_pinetorum, Microtus_pinetorum_BM_female)
Female.bm.Microtus_pinetorum2 <- Female.bm.Microtus_pinetorum2 %>% drop_na(n) 

Male.bm.Microtus_pinetorum3 <- Male.bm.Microtus_pinetorum2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_pinetorum3 <- Female.bm.Microtus_pinetorum2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_pinetorum_data_BM <- full_join(Male.bm.Microtus_pinetorum3, Female.bm.Microtus_pinetorum3)
str(Microtus_pinetorum_data_BM)
Microtus_pinetorum_data_BM <- Microtus_pinetorum_data_BM %>% drop_na() 
nrow(Microtus_pinetorum_data_BM)

#rensch's rule
BM_rensch_Microtus_pinetorum <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_pinetorum_data_BM)
summary(BM_rensch_Microtus_pinetorum)
confint(BM_rensch_Microtus_pinetorum)

ggplot(data = Microtus_pinetorum_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.75,3.75), ylim = c(2.75,3.75), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_pinetorum_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_pinetorum')
Microtus_pinetorum_HBL_male <- Microtus_pinetorum_HBL %>% filter(sex == "male")
Microtus_pinetorum_HBL_female <- Microtus_pinetorum_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_pinetorum_HBL_min <- Microtus_pinetorum_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_pinetorum_HBL_min$sex)

Male.HBL.Microtus_pinetorum = Microtus_pinetorum_HBL_min[Microtus_pinetorum_HBL_min$sex == "male", ] 
Female.HBL.Microtus_pinetorum = Microtus_pinetorum_HBL_min[Microtus_pinetorum_HBL_min$sex == "female", ] 

Male.HBL.Microtus_pinetorum2 <- right_join(Male.HBL.Microtus_pinetorum, Microtus_pinetorum_HBL_male)
Male.HBL.Microtus_pinetorum2 <- Male.HBL.Microtus_pinetorum2 %>% drop_na(n) 

Female.HBL.Microtus_pinetorum2 <- right_join(Female.HBL.Microtus_pinetorum, Microtus_pinetorum_HBL_female)
Female.HBL.Microtus_pinetorum2 <- Female.HBL.Microtus_pinetorum2 %>% drop_na(n) 

Male.HBL.Microtus_pinetorum3 <- Male.HBL.Microtus_pinetorum2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_pinetorum3 <- Female.HBL.Microtus_pinetorum2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_pinetorum_data_HBL <- full_join(Male.HBL.Microtus_pinetorum3, Female.HBL.Microtus_pinetorum3)
str(Microtus_pinetorum_data_HBL)
Microtus_pinetorum_data_HBL <- Microtus_pinetorum_data_HBL %>% drop_na() 
nrow(Microtus_pinetorum_data_HBL)

#rensch's rule
HBL_rensch_Microtus_pinetorum <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_pinetorum_data_HBL)
summary(HBL_rensch_Microtus_pinetorum)
confint(HBL_rensch_Microtus_pinetorum)

ggplot(data = Microtus_pinetorum_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.45,4.75), ylim = c(4.45,4.75), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Microtus_townsendii_BM <- Mam_BM %>% filter(binomial2 == 'Microtus_townsendii')
Microtus_townsendii_BM_male <- Microtus_townsendii_BM %>% filter(sex == "male")
Microtus_townsendii_BM_female <- Microtus_townsendii_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_townsendii_BM_min <- Microtus_townsendii_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_townsendii_BM_min$sex)

Male.bm.Microtus_townsendii = Microtus_townsendii_BM_min[Microtus_townsendii_BM_min$sex == "male", ] 
Female.bm.Microtus_townsendii = Microtus_townsendii_BM_min[Microtus_townsendii_BM_min$sex == "female", ] 

Male.bm.Microtus_townsendii2 <- right_join(Male.bm.Microtus_townsendii, Microtus_townsendii_BM_male)
Male.bm.Microtus_townsendii2 <- Male.bm.Microtus_townsendii2 %>% drop_na(n) 

Female.bm.Microtus_townsendii2 <- right_join(Female.bm.Microtus_townsendii, Microtus_townsendii_BM_female)
Female.bm.Microtus_townsendii2 <- Female.bm.Microtus_townsendii2 %>% drop_na(n) 

Male.bm.Microtus_townsendii3 <- Male.bm.Microtus_townsendii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Microtus_townsendii3 <- Female.bm.Microtus_townsendii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Microtus_townsendii_data_BM <- full_join(Male.bm.Microtus_townsendii3, Female.bm.Microtus_townsendii3)
str(Microtus_townsendii_data_BM)
Microtus_townsendii_data_BM <- Microtus_townsendii_data_BM %>% drop_na() 
nrow(Microtus_townsendii_data_BM)

#rensch's rule
BM_rensch_Microtus_townsendii <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Microtus_townsendii_data_BM)
summary(BM_rensch_Microtus_townsendii)
confint(BM_rensch_Microtus_townsendii)

ggplot(data = Microtus_townsendii_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(3,5.3), ylim = c(3,5.3), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Microtus_townsendii_HBL <- Mam_HBL %>% filter(binomial2 == 'Microtus_townsendii')
Microtus_townsendii_HBL_male <- Microtus_townsendii_HBL %>% filter(sex == "male")
Microtus_townsendii_HBL_female <- Microtus_townsendii_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Microtus_townsendii_HBL_min <- Microtus_townsendii_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Microtus_townsendii_HBL_min$sex)

Male.HBL.Microtus_townsendii = Microtus_townsendii_HBL_min[Microtus_townsendii_HBL_min$sex == "male", ] 
Female.HBL.Microtus_townsendii = Microtus_townsendii_HBL_min[Microtus_townsendii_HBL_min$sex == "female", ] 

Male.HBL.Microtus_townsendii2 <- right_join(Male.HBL.Microtus_townsendii, Microtus_townsendii_HBL_male)
Male.HBL.Microtus_townsendii2 <- Male.HBL.Microtus_townsendii2 %>% drop_na(n) 

Female.HBL.Microtus_townsendii2 <- right_join(Female.HBL.Microtus_townsendii, Microtus_townsendii_HBL_female)
Female.HBL.Microtus_townsendii2 <- Female.HBL.Microtus_townsendii2 %>% drop_na(n) 

Male.HBL.Microtus_townsendii3 <- Male.HBL.Microtus_townsendii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Microtus_townsendii3 <- Female.HBL.Microtus_townsendii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Microtus_townsendii_data_HBL <- full_join(Male.HBL.Microtus_townsendii3, Female.HBL.Microtus_townsendii3)
str(Microtus_townsendii_data_HBL)
Microtus_townsendii_data_HBL <- Microtus_townsendii_data_HBL %>% drop_na() 
nrow(Microtus_townsendii_data_HBL)

#rensch's rule
HBL_rensch_Microtus_townsendii <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Microtus_townsendii_data_HBL)
summary(HBL_rensch_Microtus_townsendii)
confint(HBL_rensch_Microtus_townsendii)

ggplot(data = Microtus_townsendii_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.7,5.1), ylim = c(4.7,5.1), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Peromyscus_boylii_BM <- Mam_BM %>% filter(binomial2 == 'Peromyscus_boylii')
Peromyscus_boylii_BM_male <- Peromyscus_boylii_BM %>% filter(sex == "male")
Peromyscus_boylii_BM_female <- Peromyscus_boylii_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_boylii_BM_min <- Peromyscus_boylii_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_boylii_BM_min$sex)

Male.bm.Peromyscus_boylii = Peromyscus_boylii_BM_min[Peromyscus_boylii_BM_min$sex == "male", ] 
Female.bm.Peromyscus_boylii = Peromyscus_boylii_BM_min[Peromyscus_boylii_BM_min$sex == "female", ] 

Male.bm.Peromyscus_boylii2 <- right_join(Male.bm.Peromyscus_boylii, Peromyscus_boylii_BM_male)
Male.bm.Peromyscus_boylii2 <- Male.bm.Peromyscus_boylii2 %>% drop_na(n) 

Female.bm.Peromyscus_boylii2 <- right_join(Female.bm.Peromyscus_boylii, Peromyscus_boylii_BM_female)
Female.bm.Peromyscus_boylii2 <- Female.bm.Peromyscus_boylii2 %>% drop_na(n) 

Male.bm.Peromyscus_boylii3 <- Male.bm.Peromyscus_boylii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Peromyscus_boylii3 <- Female.bm.Peromyscus_boylii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Peromyscus_boylii_data_BM <- full_join(Male.bm.Peromyscus_boylii3, Female.bm.Peromyscus_boylii3)
str(Peromyscus_boylii_data_BM)
Peromyscus_boylii_data_BM <- Peromyscus_boylii_data_BM %>% drop_na() 
nrow(Peromyscus_boylii_data_BM)

#rensch's rule
BM_rensch_Peromyscus_boylii <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Peromyscus_boylii_data_BM)
summary(BM_rensch_Peromyscus_boylii)
confint(BM_rensch_Peromyscus_boylii)

ggplot(data = Peromyscus_boylii_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.8,3.5), ylim = c(2.8,3.5), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Peromyscus_boylii_HBL <- Mam_HBL %>% filter(binomial2 == 'Peromyscus_boylii')
Peromyscus_boylii_HBL_male <- Peromyscus_boylii_HBL %>% filter(sex == "male")
Peromyscus_boylii_HBL_female <- Peromyscus_boylii_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_boylii_HBL_min <- Peromyscus_boylii_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_boylii_HBL_min$sex)

Male.HBL.Peromyscus_boylii = Peromyscus_boylii_HBL_min[Peromyscus_boylii_HBL_min$sex == "male", ] 
Female.HBL.Peromyscus_boylii = Peromyscus_boylii_HBL_min[Peromyscus_boylii_HBL_min$sex == "female", ] 

Male.HBL.Peromyscus_boylii2 <- right_join(Male.HBL.Peromyscus_boylii, Peromyscus_boylii_HBL_male)
Male.HBL.Peromyscus_boylii2 <- Male.HBL.Peromyscus_boylii2 %>% drop_na(n) 

Female.HBL.Peromyscus_boylii2 <- right_join(Female.HBL.Peromyscus_boylii, Peromyscus_boylii_HBL_female)
Female.HBL.Peromyscus_boylii2 <- Female.HBL.Peromyscus_boylii2 %>% drop_na(n) 

Male.HBL.Peromyscus_boylii3 <- Male.HBL.Peromyscus_boylii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Peromyscus_boylii3 <- Female.HBL.Peromyscus_boylii2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Peromyscus_boylii_data_HBL <- full_join(Male.HBL.Peromyscus_boylii3, Female.HBL.Peromyscus_boylii3)
str(Peromyscus_boylii_data_HBL)
Peromyscus_boylii_data_HBL <- Peromyscus_boylii_data_HBL %>% drop_na() 
nrow(Peromyscus_boylii_data_HBL)

#rensch's rule
HBL_rensch_Peromyscus_boylii <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Peromyscus_boylii_data_HBL)
summary(HBL_rensch_Peromyscus_boylii)
confint(HBL_rensch_Peromyscus_boylii)

ggplot(data = Peromyscus_boylii_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.3,4.85), ylim = c(4.3,4.85), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Peromyscus_crinitus_BM <- Mam_BM %>% filter(binomial2 == 'Peromyscus_crinitus')
Peromyscus_crinitus_BM_male <- Peromyscus_crinitus_BM %>% filter(sex == "male")
Peromyscus_crinitus_BM_female <- Peromyscus_crinitus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_crinitus_BM_min <- Peromyscus_crinitus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_crinitus_BM_min$sex)

Male.bm.Peromyscus_crinitus = Peromyscus_crinitus_BM_min[Peromyscus_crinitus_BM_min$sex == "male", ] 
Female.bm.Peromyscus_crinitus = Peromyscus_crinitus_BM_min[Peromyscus_crinitus_BM_min$sex == "female", ] 

Male.bm.Peromyscus_crinitus2 <- right_join(Male.bm.Peromyscus_crinitus, Peromyscus_crinitus_BM_male)
Male.bm.Peromyscus_crinitus2 <- Male.bm.Peromyscus_crinitus2 %>% drop_na(n) 

Female.bm.Peromyscus_crinitus2 <- right_join(Female.bm.Peromyscus_crinitus, Peromyscus_crinitus_BM_female)
Female.bm.Peromyscus_crinitus2 <- Female.bm.Peromyscus_crinitus2 %>% drop_na(n) 

Male.bm.Peromyscus_crinitus3 <- Male.bm.Peromyscus_crinitus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Peromyscus_crinitus3 <- Female.bm.Peromyscus_crinitus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Peromyscus_crinitus_data_BM <- full_join(Male.bm.Peromyscus_crinitus3, Female.bm.Peromyscus_crinitus3)
str(Peromyscus_crinitus_data_BM)
Peromyscus_crinitus_data_BM <- Peromyscus_crinitus_data_BM %>% drop_na() 
nrow(Peromyscus_crinitus_data_BM)

#rensch's rule
BM_rensch_Peromyscus_crinitus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Peromyscus_crinitus_data_BM)
summary(BM_rensch_Peromyscus_crinitus)
confint(BM_rensch_Peromyscus_crinitus)

ggplot(data = Peromyscus_crinitus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.5,3), ylim = c(2.5,3), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Peromyscus_crinitus_HBL <- Mam_HBL %>% filter(binomial2 == 'Peromyscus_crinitus')
Peromyscus_crinitus_HBL_male <- Peromyscus_crinitus_HBL %>% filter(sex == "male")
Peromyscus_crinitus_HBL_female <- Peromyscus_crinitus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_crinitus_HBL_min <- Peromyscus_crinitus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_crinitus_HBL_min$sex)

Male.HBL.Peromyscus_crinitus = Peromyscus_crinitus_HBL_min[Peromyscus_crinitus_HBL_min$sex == "male", ] 
Female.HBL.Peromyscus_crinitus = Peromyscus_crinitus_HBL_min[Peromyscus_crinitus_HBL_min$sex == "female", ] 

Male.HBL.Peromyscus_crinitus2 <- right_join(Male.HBL.Peromyscus_crinitus, Peromyscus_crinitus_HBL_male)
Male.HBL.Peromyscus_crinitus2 <- Male.HBL.Peromyscus_crinitus2 %>% drop_na(n) 

Female.HBL.Peromyscus_crinitus2 <- right_join(Female.HBL.Peromyscus_crinitus, Peromyscus_crinitus_HBL_female)
Female.HBL.Peromyscus_crinitus2 <- Female.HBL.Peromyscus_crinitus2 %>% drop_na(n) 

Male.HBL.Peromyscus_crinitus3 <- Male.HBL.Peromyscus_crinitus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Peromyscus_crinitus3 <- Female.HBL.Peromyscus_crinitus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Peromyscus_crinitus_data_HBL <- full_join(Male.HBL.Peromyscus_crinitus3, Female.HBL.Peromyscus_crinitus3)
str(Peromyscus_crinitus_data_HBL)
Peromyscus_crinitus_data_HBL <- Peromyscus_crinitus_data_HBL %>% drop_na() 
nrow(Peromyscus_crinitus_data_HBL)

#rensch's rule
HBL_rensch_Peromyscus_crinitus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Peromyscus_crinitus_data_HBL)
summary(HBL_rensch_Peromyscus_crinitus)
confint(HBL_rensch_Peromyscus_crinitus)

ggplot(data = Peromyscus_crinitus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.2,4.56), ylim = c(4.2,4.56), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Peromyscus_eremicus_BM <- Mam_BM %>% filter(binomial2 == 'Peromyscus_eremicus')
Peromyscus_eremicus_BM_male <- Peromyscus_eremicus_BM %>% filter(sex == "male")
Peromyscus_eremicus_BM_female <- Peromyscus_eremicus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_eremicus_BM_min <- Peromyscus_eremicus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_eremicus_BM_min$sex)

Male.bm.Peromyscus_eremicus = Peromyscus_eremicus_BM_min[Peromyscus_eremicus_BM_min$sex == "male", ] 
Female.bm.Peromyscus_eremicus = Peromyscus_eremicus_BM_min[Peromyscus_eremicus_BM_min$sex == "female", ] 

Male.bm.Peromyscus_eremicus2 <- right_join(Male.bm.Peromyscus_eremicus, Peromyscus_eremicus_BM_male)
Male.bm.Peromyscus_eremicus2 <- Male.bm.Peromyscus_eremicus2 %>% drop_na(n) 

Female.bm.Peromyscus_eremicus2 <- right_join(Female.bm.Peromyscus_eremicus, Peromyscus_eremicus_BM_female)
Female.bm.Peromyscus_eremicus2 <- Female.bm.Peromyscus_eremicus2 %>% drop_na(n) 

Male.bm.Peromyscus_eremicus3 <- Male.bm.Peromyscus_eremicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Peromyscus_eremicus3 <- Female.bm.Peromyscus_eremicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Peromyscus_eremicus_data_BM <- full_join(Male.bm.Peromyscus_eremicus3, Female.bm.Peromyscus_eremicus3)
str(Peromyscus_eremicus_data_BM)
Peromyscus_eremicus_data_BM <- Peromyscus_eremicus_data_BM %>% drop_na() 
nrow(Peromyscus_eremicus_data_BM)

#rensch's rule
BM_rensch_Peromyscus_eremicus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Peromyscus_eremicus_data_BM)
summary(BM_rensch_Peromyscus_eremicus)
confint(BM_rensch_Peromyscus_eremicus)

ggplot(data = Peromyscus_eremicus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.7,3.4), ylim = c(2.7,3.4), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Peromyscus_eremicus_HBL <- Mam_HBL %>% filter(binomial2 == 'Peromyscus_eremicus')
Peromyscus_eremicus_HBL_male <- Peromyscus_eremicus_HBL %>% filter(sex == "male")
Peromyscus_eremicus_HBL_female <- Peromyscus_eremicus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_eremicus_HBL_min <- Peromyscus_eremicus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_eremicus_HBL_min$sex)

Male.HBL.Peromyscus_eremicus = Peromyscus_eremicus_HBL_min[Peromyscus_eremicus_HBL_min$sex == "male", ] 
Female.HBL.Peromyscus_eremicus = Peromyscus_eremicus_HBL_min[Peromyscus_eremicus_HBL_min$sex == "female", ] 

Male.HBL.Peromyscus_eremicus2 <- right_join(Male.HBL.Peromyscus_eremicus, Peromyscus_eremicus_HBL_male)
Male.HBL.Peromyscus_eremicus2 <- Male.HBL.Peromyscus_eremicus2 %>% drop_na(n) 

Female.HBL.Peromyscus_eremicus2 <- right_join(Female.HBL.Peromyscus_eremicus, Peromyscus_eremicus_HBL_female)
Female.HBL.Peromyscus_eremicus2 <- Female.HBL.Peromyscus_eremicus2 %>% drop_na(n) 

Male.HBL.Peromyscus_eremicus3 <- Male.HBL.Peromyscus_eremicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Peromyscus_eremicus3 <- Female.HBL.Peromyscus_eremicus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Peromyscus_eremicus_data_HBL <- full_join(Male.HBL.Peromyscus_eremicus3, Female.HBL.Peromyscus_eremicus3)
str(Peromyscus_eremicus_data_HBL)
Peromyscus_eremicus_data_HBL <- Peromyscus_eremicus_data_HBL %>% drop_na() 
nrow(Peromyscus_eremicus_data_HBL)

#rensch's rule
HBL_rensch_Peromyscus_eremicus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Peromyscus_eremicus_data_HBL)
summary(HBL_rensch_Peromyscus_eremicus)
confint(HBL_rensch_Peromyscus_eremicus)

ggplot(data = Peromyscus_eremicus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.3,4.6), ylim = c(4.3,4.6), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")


########################################################################################################################

###Filter by species & sex
Peromyscus_leucopus_BM <- Mam_BM %>% filter(binomial2 == 'Peromyscus_leucopus')
Peromyscus_leucopus_BM_male <- Peromyscus_leucopus_BM %>% filter(sex == "male")
Peromyscus_leucopus_BM_female <- Peromyscus_leucopus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_leucopus_BM_min <- Peromyscus_leucopus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_leucopus_BM_min$sex)

Male.bm.Peromyscus_leucopus = Peromyscus_leucopus_BM_min[Peromyscus_leucopus_BM_min$sex == "male", ] 
Female.bm.Peromyscus_leucopus = Peromyscus_leucopus_BM_min[Peromyscus_leucopus_BM_min$sex == "female", ] 

Male.bm.Peromyscus_leucopus2 <- right_join(Male.bm.Peromyscus_leucopus, Peromyscus_leucopus_BM_male)
Male.bm.Peromyscus_leucopus2 <- Male.bm.Peromyscus_leucopus2 %>% drop_na(n) 

Female.bm.Peromyscus_leucopus2 <- right_join(Female.bm.Peromyscus_leucopus, Peromyscus_leucopus_BM_female)
Female.bm.Peromyscus_leucopus2 <- Female.bm.Peromyscus_leucopus2 %>% drop_na(n) 

Male.bm.Peromyscus_leucopus3 <- Male.bm.Peromyscus_leucopus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Peromyscus_leucopus3 <- Female.bm.Peromyscus_leucopus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Peromyscus_leucopus_data_BM <- full_join(Male.bm.Peromyscus_leucopus3, Female.bm.Peromyscus_leucopus3)
str(Peromyscus_leucopus_data_BM)
Peromyscus_leucopus_data_BM <- Peromyscus_leucopus_data_BM %>% drop_na() 
nrow(Peromyscus_leucopus_data_BM)

#rensch's rule
BM_rensch_Peromyscus_leucopus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Peromyscus_leucopus_data_BM)
summary(BM_rensch_Peromyscus_leucopus)
confint(BM_rensch_Peromyscus_leucopus)

ggplot(data = Peromyscus_leucopus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.5,3.65), ylim = c(2.5,3.65), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Peromyscus_leucopus_HBL <- Mam_HBL %>% filter(binomial2 == 'Peromyscus_leucopus')
Peromyscus_leucopus_HBL_male <- Peromyscus_leucopus_HBL %>% filter(sex == "male")
Peromyscus_leucopus_HBL_female <- Peromyscus_leucopus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_leucopus_HBL_min <- Peromyscus_leucopus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_leucopus_HBL_min$sex)

Male.HBL.Peromyscus_leucopus = Peromyscus_leucopus_HBL_min[Peromyscus_leucopus_HBL_min$sex == "male", ] 
Female.HBL.Peromyscus_leucopus = Peromyscus_leucopus_HBL_min[Peromyscus_leucopus_HBL_min$sex == "female", ] 

Male.HBL.Peromyscus_leucopus2 <- right_join(Male.HBL.Peromyscus_leucopus, Peromyscus_leucopus_HBL_male)
Male.HBL.Peromyscus_leucopus2 <- Male.HBL.Peromyscus_leucopus2 %>% drop_na(n) 

Female.HBL.Peromyscus_leucopus2 <- right_join(Female.HBL.Peromyscus_leucopus, Peromyscus_leucopus_HBL_female)
Female.HBL.Peromyscus_leucopus2 <- Female.HBL.Peromyscus_leucopus2 %>% drop_na(n) 

Male.HBL.Peromyscus_leucopus3 <- Male.HBL.Peromyscus_leucopus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Peromyscus_leucopus3 <- Female.HBL.Peromyscus_leucopus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Peromyscus_leucopus_data_HBL <- full_join(Male.HBL.Peromyscus_leucopus3, Female.HBL.Peromyscus_leucopus3)
str(Peromyscus_leucopus_data_HBL)
Peromyscus_leucopus_data_HBL <- Peromyscus_leucopus_data_HBL %>% drop_na() 
nrow(Peromyscus_leucopus_data_HBL)

#rensch's rule
HBL_rensch_Peromyscus_leucopus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Peromyscus_leucopus_data_HBL)
summary(HBL_rensch_Peromyscus_leucopus)
confint(HBL_rensch_Peromyscus_leucopus)

ggplot(data = Peromyscus_leucopus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.25,4.7), ylim = c(4.25,4.7), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

########################################################################################################################

###Filter by species & sex
Peromyscus_maniculatus_BM <- Mam_BM %>% filter(binomial2 == 'Peromyscus_maniculatus')
Peromyscus_maniculatus_BM_male <- Peromyscus_maniculatus_BM %>% filter(sex == "male")
Peromyscus_maniculatus_BM_female <- Peromyscus_maniculatus_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_maniculatus_BM_min <- Peromyscus_maniculatus_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_maniculatus_BM_min$sex)

Male.bm.Peromyscus_maniculatus = Peromyscus_maniculatus_BM_min[Peromyscus_maniculatus_BM_min$sex == "male", ] 
Female.bm.Peromyscus_maniculatus = Peromyscus_maniculatus_BM_min[Peromyscus_maniculatus_BM_min$sex == "female", ] 

Male.bm.Peromyscus_maniculatus2 <- right_join(Male.bm.Peromyscus_maniculatus, Peromyscus_maniculatus_BM_male)
Male.bm.Peromyscus_maniculatus2 <- Male.bm.Peromyscus_maniculatus2 %>% drop_na(n) 

Female.bm.Peromyscus_maniculatus2 <- right_join(Female.bm.Peromyscus_maniculatus, Peromyscus_maniculatus_BM_female)
Female.bm.Peromyscus_maniculatus2 <- Female.bm.Peromyscus_maniculatus2 %>% drop_na(n) 

Male.bm.Peromyscus_maniculatus3 <- Male.bm.Peromyscus_maniculatus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Peromyscus_maniculatus3 <- Female.bm.Peromyscus_maniculatus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Peromyscus_maniculatus_data_BM <- full_join(Male.bm.Peromyscus_maniculatus3, Female.bm.Peromyscus_maniculatus3)
str(Peromyscus_maniculatus_data_BM)
Peromyscus_maniculatus_data_BM <- Peromyscus_maniculatus_data_BM %>% drop_na() 
nrow(Peromyscus_maniculatus_data_BM)

#rensch's rule
BM_rensch_Peromyscus_maniculatus <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Peromyscus_maniculatus_data_BM)
summary(BM_rensch_Peromyscus_maniculatus)
confint(BM_rensch_Peromyscus_maniculatus)

ggplot(data = Peromyscus_maniculatus_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.2,3.6), ylim = c(2.2,3.6), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Peromyscus_maniculatus_HBL <- Mam_HBL %>% filter(binomial2 == 'Peromyscus_maniculatus')
Peromyscus_maniculatus_HBL_male <- Peromyscus_maniculatus_HBL %>% filter(sex == "male")
Peromyscus_maniculatus_HBL_female <- Peromyscus_maniculatus_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_maniculatus_HBL_min <- Peromyscus_maniculatus_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_maniculatus_HBL_min$sex)

Male.HBL.Peromyscus_maniculatus = Peromyscus_maniculatus_HBL_min[Peromyscus_maniculatus_HBL_min$sex == "male", ] 
Female.HBL.Peromyscus_maniculatus = Peromyscus_maniculatus_HBL_min[Peromyscus_maniculatus_HBL_min$sex == "female", ] 

Male.HBL.Peromyscus_maniculatus2 <- right_join(Male.HBL.Peromyscus_maniculatus, Peromyscus_maniculatus_HBL_male)
Male.HBL.Peromyscus_maniculatus2 <- Male.HBL.Peromyscus_maniculatus2 %>% drop_na(n) 

Female.HBL.Peromyscus_maniculatus2 <- right_join(Female.HBL.Peromyscus_maniculatus, Peromyscus_maniculatus_HBL_female)
Female.HBL.Peromyscus_maniculatus2 <- Female.HBL.Peromyscus_maniculatus2 %>% drop_na(n) 

Male.HBL.Peromyscus_maniculatus3 <- Male.HBL.Peromyscus_maniculatus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Peromyscus_maniculatus3 <- Female.HBL.Peromyscus_maniculatus2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Peromyscus_maniculatus_data_HBL <- full_join(Male.HBL.Peromyscus_maniculatus3, Female.HBL.Peromyscus_maniculatus3)
str(Peromyscus_maniculatus_data_HBL)
Peromyscus_maniculatus_data_HBL <- Peromyscus_maniculatus_data_HBL %>% drop_na() 
nrow(Peromyscus_maniculatus_data_HBL)

#rensch's rule
HBL_rensch_Peromyscus_maniculatus <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Peromyscus_maniculatus_data_HBL)
summary(HBL_rensch_Peromyscus_maniculatus)
confint(HBL_rensch_Peromyscus_maniculatus)

ggplot(data = Peromyscus_maniculatus_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.2,4.85), ylim = c(4.2,4.85), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

########################################################################################################################
Peromyscus_truei_BM <- Mam_BM %>% filter(binomial2 == 'Peromyscus_truei')
Peromyscus_truei_BM_male <- Peromyscus_truei_BM %>% filter(sex == "male")
Peromyscus_truei_BM_female <- Peromyscus_truei_BM %>% filter(sex == "female")

###Filter by species & sex
Peromyscus_truei_BM <- Mam_BM %>% filter(binomial2 == 'Peromyscus_truei')
Peromyscus_truei_BM_male <- Peromyscus_truei_BM %>% filter(sex == "male")
Peromyscus_truei_BM_female <- Peromyscus_truei_BM %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_truei_BM_min <- Peromyscus_truei_BM %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_truei_BM_min$sex)

Male.bm.Peromyscus_truei = Peromyscus_truei_BM_min[Peromyscus_truei_BM_min$sex == "male", ] 
Female.bm.Peromyscus_truei = Peromyscus_truei_BM_min[Peromyscus_truei_BM_min$sex == "female", ] 

Male.bm.Peromyscus_truei2 <- right_join(Male.bm.Peromyscus_truei, Peromyscus_truei_BM_male)
Male.bm.Peromyscus_truei2 <- Male.bm.Peromyscus_truei2 %>% drop_na(n) 

Female.bm.Peromyscus_truei2 <- right_join(Female.bm.Peromyscus_truei, Peromyscus_truei_BM_female)
Female.bm.Peromyscus_truei2 <- Female.bm.Peromyscus_truei2 %>% drop_na(n) 

Male.bm.Peromyscus_truei3 <- Male.bm.Peromyscus_truei2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Female.bm.Peromyscus_truei3 <- Female.bm.Peromyscus_truei2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_BM = mean(X1st_body_mass, na.rm=TRUE)) 

Peromyscus_truei_data_BM <- full_join(Male.bm.Peromyscus_truei3, Female.bm.Peromyscus_truei3)
str(Peromyscus_truei_data_BM)
Peromyscus_truei_data_BM <- Peromyscus_truei_data_BM %>% drop_na() 
nrow(Peromyscus_truei_data_BM)

#rensch's rule
BM_rensch_Peromyscus_truei <- lm(log(Mean_male_BM) ~ log(Mean_female_BM), data = Peromyscus_truei_data_BM)
summary(BM_rensch_Peromyscus_truei)
confint(BM_rensch_Peromyscus_truei)

ggplot(data = Peromyscus_truei_data_BM, aes(x = log(Mean_female_BM), y = log(Mean_male_BM))) + 
  coord_fixed(ratio = 1, xlim = c(2.7,3.6), ylim = c(2.7,3.6), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")

############################################################
#HBL
###Filter by species & sex
Peromyscus_truei_HBL <- Mam_HBL %>% filter(binomial2 == 'Peromyscus_truei')
Peromyscus_truei_HBL_male <- Peromyscus_truei_HBL %>% filter(sex == "male")
Peromyscus_truei_HBL_female <- Peromyscus_truei_HBL %>% filter(sex == "female")

#######
#Split by species, sex, and id_cells 
#10 records per cell for males and females 

Peromyscus_truei_HBL_min <- Peromyscus_truei_HBL %>%
  group_by(id_cells, sex) %>%
  tally() %>%
  filter(n >= 10) %>% 
  ungroup()
#####should be able to match ID_cell with avg predictors 

table(Peromyscus_truei_HBL_min$sex)

Male.HBL.Peromyscus_truei = Peromyscus_truei_HBL_min[Peromyscus_truei_HBL_min$sex == "male", ] 
Female.HBL.Peromyscus_truei = Peromyscus_truei_HBL_min[Peromyscus_truei_HBL_min$sex == "female", ] 

Male.HBL.Peromyscus_truei2 <- right_join(Male.HBL.Peromyscus_truei, Peromyscus_truei_HBL_male)
Male.HBL.Peromyscus_truei2 <- Male.HBL.Peromyscus_truei2 %>% drop_na(n) 

Female.HBL.Peromyscus_truei2 <- right_join(Female.HBL.Peromyscus_truei, Peromyscus_truei_HBL_female)
Female.HBL.Peromyscus_truei2 <- Female.HBL.Peromyscus_truei2 %>% drop_na(n) 

Male.HBL.Peromyscus_truei3 <- Male.HBL.Peromyscus_truei2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_male_HBL = mean(HB.length, na.rm=TRUE)) 

Female.HBL.Peromyscus_truei3 <- Female.HBL.Peromyscus_truei2 %>%
  group_by(id_cells) %>%
  dplyr::summarize(Mean_female_HBL = mean(HB.length, na.rm=TRUE)) 

Peromyscus_truei_data_HBL <- full_join(Male.HBL.Peromyscus_truei3, Female.HBL.Peromyscus_truei3)
str(Peromyscus_truei_data_HBL)
Peromyscus_truei_data_HBL <- Peromyscus_truei_data_HBL %>% drop_na() 
nrow(Peromyscus_truei_data_HBL)

#rensch's rule
HBL_rensch_Peromyscus_truei <- lm(log(Mean_male_HBL) ~ log(Mean_female_HBL), data = Peromyscus_truei_data_HBL)
summary(HBL_rensch_Peromyscus_truei)
confint(HBL_rensch_Peromyscus_truei)

ggplot(data = Peromyscus_truei_data_HBL, aes(x = log(Mean_female_HBL), y = log(Mean_male_HBL))) + 
  coord_fixed(ratio = 1, xlim = c(4.3,4.7), ylim = c(4.3,4.7), expand = TRUE) +
  geom_point(aes(color=id_cells)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope=1,intercept = 0, linetype="dashed")



#########################################################################################




