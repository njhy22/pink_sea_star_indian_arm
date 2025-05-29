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
rov_measurement$diameter <- gsub("^(N/A)$", "0", rov_measurement$diameter, ignore.case = TRUE) #turned N/a into 0 values so it can be numeric 

#double check site names are the same for both csv files 
unique(rov_measurement$location)
unique(historic_measurement$location)

#try to plot, not what we want though 
## sample plot for present sites and diameter, raw data 
ggplot(rov_measurement, aes(x = location, y = diameter)) +
  geom_point() +
  ggtitle("location and diameter") +
  xlab("location") +
  ylab("Diameter")



##sample plot for historic sites and diameter, raw data
ggplot(historic_measurement, aes(x = location, y = diameter)) +
  geom_point() +
  ggtitle("location and diameter") +
  xlab("location") +
  ylab("Diameter")

#classification of each variable 
class(rov_measurement$diameter)
rov_measurement$diameter <- as.numeric(as.character(rov_measurement$diameter))

#overall averages for each sheet 
mean(rov_measurement$diameter, na.rm = TRUE) #average for overall diameter for present
mean(historic_measurement$diameter, na.rm = TRUE) #average for overall diameter for historic

#for each site average?? 
data_2<- rov_measurement %>% 
  filter(location == "Cate's Park")
mean(data_2$diameter)

#isolate by 1 site, keep location and diameter (2 columns) in this dataframe
location_df <- rov_measurement %>%
  filter(location == "Bedwell Bay") %>%
  group_by(location, diameter) %>%
  summarise(total = n()) 

mean(location_df$diameter, na.rm = TRUE)

#plot for 1 site?? with diameter frequencies?? 
ggplot(location_df, aes(x = diameter, y = total)) +
  geom_point() +
  ggtitle("location and diameter") +
  xlab("diameter") +
  ylab("frequency")


#merging the 2 csv files 
rov_present <- rov_measurement[, c("location", "diameter")]
rov_historic <- historic_measurement[, c("location", "diameter")]



combined_rov <- cbind(rov_present, rov_historic)

##### MODEL 1: HISTORICAL VS PRESENT #######

#isolate data set for 1 site
data_2<- rov_measurement %>% 
  filter(dive_id == "Cate's Park")

#isolate historic data set for 1 site
data_CP<- historic_measurement %>% 
  filter(Location == "Cate's Park")




##### MODEL 2: ROV VS TRANSECT ##########





##### MODEL 3: LIFE HISTORY: PRESENT #######