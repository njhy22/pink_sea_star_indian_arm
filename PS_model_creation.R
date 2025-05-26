#Title: Model 1 Pink Sea Star
#Author: Nicole Yang
#Date: May 22, 2025

library(usethis)
use(git)

# Load packages
library(tidyverse)
library(MetBrewer)
library(dplyr) 
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(DHARMa)

# Load CSV files 
transect_abundance <- read.csv("present_raw_data/PS_2025_Transect/PS_Transect_Abundance.csv")
transect_abundance
transect_measurement <- read.csv("present_raw_data/PS_2025_Transect/PS_Transect_Measurements.csv")
transect_measurement
transect_location_info <- read.csv("present_raw_data/PS_2025_Transect/PS_Transect_Location_Info.csv") 
transect_location_info

rov_abundance <- read.csv("present_raw_data/PS_2025_ROV/PS_ROV_Abundance.csv")
rov_abundance
rov_measurement <- read.csv("present_raw_data/PS_2025_ROV/PS_ROV_Measurments.csv") 
rov_measurement
rov_location_info <- read.csv("present_raw_data/PS_2025_ROV/PS_ROV_Location_Info.csv")
rov_location_info

historic_measurement <- read.csv("historic_raw_data/PS_Historic_Data.csv")

##### CLEANING DATA SHEETS ###### 
rov_measurement
# isolate location with the diameter measurements for both historic and present 



unique(rov_measurement$dive_id)
# group together dive ID's to site names 
rov_measurement$dive_id <- gsub("^(TI_J6_R1|TI_F5_R2)$", "Twin Island", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(JI_J6_R1|JI_M12_R2)$", "Jug Island", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(BI_F5_R1|BI_M12_R2)$", "Boulder Island", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(BP_J27_R1|BP_M26_R2)$", "Best Point", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(LR_J27_R1|LR_M17_R2)$", "Lone Rock Point", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(BB_M17_R1|BB_A5_R2)$", "Bedwell Bay", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(MI_M19_R1|MI_A5_R2)$", "Moody Inlet", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(GR_M19_R1|GR_A9_R2)$", "Grey Rocks Island", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(CP_M31_R1|CP_A9_R2)$", "Cate's Park", rov_measurement$dive_id, ignore.case = TRUE)
rov_measurement$dive_id <- gsub("^(BC_M26_R1|BC_M31_R2)$", "Belcarra Bay", rov_measurement$dive_id, ignore.case = TRUE)

#double check site names are the same for both csv files 
unique(rov_measurement$dive_id)
unique(historic_measurement$Location)

#try to plot, not what we want though 
ggplot(data_2, aes(x = depth, y = diameter)) +
  geom_point() +
  ggtitle("Present Diameter Size") +
  xlab("Depth") +
  ylab("Diameter")

#isolate data set for 1 site
data_2<- rov_measurement %>% 
  filter(dive_id == "Cate's Park")

#isolate historic data set for 1 site
data_CP<- historic_measurement %>% 
  filter(Location == "Cate's Park")

#attempt to isolate just diameter column so i can plot with the other csv file, but i don't think its working
diameter_present_df <- rov_measurement[["diameter"]]




##### MODEL 1: HISTORICAL VS PRESENT #######




##### MODEL 2: ROV VS TRANSECT ##########





##### MODEL 3: LIFE HISTORY: PRESENT #######