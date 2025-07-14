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

######## Load CSV Files ######

# LOAD transect data sheets 
#transect abundance sheet
transect_abundance <- read.csv("present_raw_data/PS_2025_Transect/PS_Transect_Abundance.csv")
transect_abundance

#transect measurements (diameters sea stars)
transect_measurement <- read.csv("present_raw_data/PS_2025_Transect/PS_Transect_Measurements.csv")
transect_measurement

#transect site data
transect_location_info <- read.csv("present_raw_data/PS_2025_Transect/PS_Transect_Location_Info.csv") 
transect_location_info


# LOAD present ROV data sheets 

# abundances
rov_abundance <- read.csv("present_raw_data/PS_2025_ROV/PS_ROV_Abundance.csv")

# diameters
rov_measurement <- read.csv("present_raw_data/PS_2025_ROV/PS_ROV_Measurments.csv") 

# metadata
rov_location_info <- read.csv("present_raw_data/PS_2025_ROV/PS_ROV_Location_Info.csv")
rov_location_info


# LOAD Historic data sheet
historic_measurement <- read.csv("historic_raw_data/PS_Historic_Data.csv")
historic_measurement


#FILTER OUT Jan 20 ROV DIVES for rov measurmment, location info, and abundance sheets
rov_measurement_jan20_clean <- rov_measurement %>%
  filter(!dive_id %in% c("JI_J20_R2", "BI_J20_R1"))

rov_abundance_jan20_clean <- rov_abundance %>%
  filter(!dive_id %in% c("JI_J20_R2", "BI_J20_R1"))

rov_location_info_jan20_clean <- rov_location_info %>%
  filter(!dive_id %in% c("JI_J20_R2", "BI_J20_R1"))


##### CLEAN ROV abundance DATA FRAMES (And save csv for models) #####
#join metadata to present ROV data sheets 
# left joining
rov_abundance_withmeta <- rov_abundance_jan20_clean %>%
  left_join(rov_location_info_jan20_clean, by="dive_id") %>%
  dplyr::select(-c(location.y, notes, X)) %>%
  rename(location = location.x)

# creating date time column   
#created survey time column 
#creating abundance/minute column 
rov_abundance_date <- rov_abundance_withmeta %>%
  mutate(month_num = case_when(month == "Jan" ~ "01",
                               month == "Jan " ~ "01",
                               month == "Feb" ~ "02",
                               month == "Mar" ~ "03",
                               month == "Apr" ~ "04")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  filter(species == "Pisaster brevispinus") %>%
  add_column(time_period = "present") %>%
  add_column(time = as.numeric(30)) %>%
  mutate(number_min = number/time)

#add dates to historic sheet, add time period 
historic_measurement_date <- historic_measurement %>%
  mutate(month_num = case_when(month == "March" ~ "03",
                               month == "April" ~ "04",
                               month == "July" ~ "07",
                               month == "November" ~ "11")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  add_column(time_period = "past")

historic_abundance_for_plot <- historic_measurement_date %>%
  group_by(month, year, date, time, location, diver) %>%
  count(location, diver) %>%
  rename("number" = "n") %>%
  add_column(time_period = "past") %>% 
  add_column(species = "Pisaster brevispinus") %>%
  ungroup() %>%
  as.data.frame() 

## make abundance # of stars/minute of survey 
historic_abund_per_min <- historic_abundance_for_plot %>%
  mutate(number_min = number/time)

# QUICK PLOT abundance
ggplot(data = historic_abund_per_min, aes(x= date, y = number_min)) +
  geom_point()

# combine data frames for ABUNDANCE
clean_abund_present_raw <- rov_abundance_date %>%
  select(c(location, date, year, time_period, number, number_min, time, species)) %>%
  filter(species == "Pisaster brevispinus") 

#HISTORIC DATA, no need to filter because only pink sea stars 
clean_abund_historic_raw <- historic_abund_per_min %>%
  select(c(location, date, year, time_period, number, time, number_min, species))

#MERGE ABUNDANCE COUNTS from 2 datashets into 1 (past and present data)
combined_abund_raw_plot <- full_join(clean_abund_historic_raw, clean_abund_present_raw)
print(combined_abund_raw_plot)

combined_abund_complete <- combined_abund_raw_plot %>%
  mutate(years_past = (2025 - as.numeric(year)),
         wasting = case_when(year == "2010"|
                               year == "2011"|
                               year == "2012"|
                               year == "2013" ~ "pre",
                             year == "2014"|
                               year == "2015" ~ "post",
                             year == "2025" ~ "present"))

##
#SAVE .CSV FOR ABUNDANCE 
write.csv(combined_abund_complete, "./manipulated_data/past_present_pop_abundances.csv", row.names=F)



##### CLEAN ROV DIAMETER DATA FRAMES (And save csv for models) ##### 
# for DIAMETER in PRESENT ROVS TO metadata sheet
# left joining
rov_measurement_withmeta <- rov_measurement_jan20_clean %>%
  left_join(rov_location_info_jan20_clean, by="dive_id") %>%
  dplyr::select(-c(location.y)) %>%
  rename(location = location.x)

# creating date time column ROV PRESENT **(NEED to clean column names)**
rov_measurement_date <- rov_measurement_withmeta %>%
  mutate(month_num = case_when(month == "Jan" ~ "01",
                               month == "Jan " ~ "01",
                               month == "Feb" ~ "02",
                               month == "Mar" ~ "03",
                               month == "Apr" ~ "04")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  add_column(time_period = "present") 

#make columns into correct unit 
rov_present <- rov_measurement_date %>%
  mutate(diameter = as.numeric(diameter),
         depth_found = as.numeric(depth_found),
         location = as.factor(location),
         feeding = as.factor(feeding),
         feeding_type = as.factor(feeding_type),
         year = as.character(year),
         substrate_level = as.factor(substrate_level)) 


##HISTORIC measurements diameters 
historic_measurement_date <- historic_measurement %>%
  mutate(month_num = case_when(month == "March" ~ "03",
                               month == "April" ~ "04",
                               month == "July" ~ "07",
                               month == "November" ~ "11")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  add_column(time_period = "past")

rov_historic <- historic_measurement_date %>%
  mutate(diameter = as.numeric(diameter),
         location = as.factor(location),
         depth_low = as.numeric(depth_low),
         depth_upper = as.numeric(depth_upper),
         year = as.character(year),
         feeding = as.factor(feeding)) 

str(rov_historic)
# COMBINE data frames for diameter
#PRESENT diamter measurements
clean_diameter_present_raw <- rov_present%>%
  select(c(location, date, year, time_period, diameter)) %>%
  na.omit(rov_measurement_date$diameter) 

#HISTORIC DATA, no need to filter because only pink sea stars 
clean_diameter_historic_raw <- rov_historic %>%
  select(c(location, date, year, time_period, diameter))

#MERGE DIAMETER COUNTS from 2 datashets into 1 (past and present data)
combined_diameter_raw_plot <- full_join(clean_diameter_historic_raw, clean_diameter_present_raw)
print(combined_diameter_raw_plot)

combined_diameter_full <- combined_diameter_raw_plot %>%
  mutate(years_past = (2025 - as.numeric(year)),
         wasting = case_when(year == "2010"|
                               year == "2011"|
                               year == "2012"|
                               year == "2013" ~ "pre",
                             year == "2014"|
                               year == "2015" ~ "post",
                             year == "2025" ~ "present"))

# SAVE CSV for diameters 
write.csv(combined_diameter_full, "./manipulated_data/past_present_indiv_diameter.csv", row.names = F)



##
##
##### CLEAN Transect data frame #####
#### for TRANSECT CLEAN ####
str(transect_location_info)
transect_location_data <- transect_location_info %>%
  select(c(location, visibility, substrate_level, dive_id)) %>%
  mutate(visibility = as.numeric(visibility),
         substrate_level = as.factor(substrate_level),
         location = as.factor(location)) %>%
  add_column(method = "transect")

str(transect_location_data)

##clean transect abundance sheet, isolate for pinks only 
transect_numbers_mod2 <- transect_abundance %>%
  mutate(location = as.factor(location)) %>%
  filter(species == "Pisaster brevispinus ")

# left joining transect sheets: location+abundance
transect_abundance_withmeta <- transect_numbers_mod2 %>%
  left_join(transect_location_data, by="dive_id") %>%
  dplyr::select(-c(location.y)) %>%
  rename(location = location.x)

transect_abundance_date <- transect_abundance_withmeta %>%
  mutate(number = as.numeric(number),
         dive_id = as.factor(dive_id)) %>%
  select(-species, -common_name)

##### Q2 Plots: Abundance by method ----

## Plot 1: Linear plot of abundance ROV vs. Transect
#for ROV CLEAN
# categories: location, visibility, substrate_level, method(add column)
rov_abundance_model2 <- rov_abundance_date %>%
  mutate(species = as.factor(species),
         number = as.numeric(number),
         location = as.factor(location),
         dive_id = as.factor(dive_id))

### make number say rov_number 
rov_model_2_abun_location <- rov_abundance_model2 %>%
  select(c(location, visibility, substrate_level, dive_id, date, number, species)) %>%
  mutate(visibility = as.numeric(visibility),
         substrate_level = as.factor(substrate_level),
         location = as.factor(location)) %>%
  add_column(method = "roving") %>%
  select(-species, -date)

str(rov_model_2_abun_location)

 


## Plot ?: Ranked differences in abundance b/w methods







##### FROM BEFORE; all below is good for above section. Join metadata to present ROV data sheets #####

# left joining
rov_abundance_withmeta <- rov_abundance_jan20_clean %>%
  left_join(rov_location_info_jan20_clean, by="dive_id") %>%
  dplyr::select(-c(location.y, notes, X)) %>%
  rename(location = location.x)

# creating date time column 
#created survey time column 
#creating abundance/minute column 
rov_abundance_date <- rov_abundance_withmeta %>%
  mutate(month_num = case_when(month == "Jan" ~ "01",
                               month == "Jan " ~ "01",
                               month == "Feb" ~ "02",
                               month == "Mar" ~ "03",
                               month == "Apr" ~ "04")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  filter(species == "Pisaster brevispinus") %>%
  add_column(time_period = "present") %>%
  add_column(time = as.numeric(30)) %>%
  mutate(number_min = number/time)
  

ggplot (data = rov_abundance_date, aes(x = date, y = number_min)) +
  geom_point()

# for DIAMETER in PRESENT ROVS TO metadata sheet
# left joining
rov_measurement_withmeta <- rov_measurement_jan20_clean %>%
  left_join(rov_location_info_jan20_clean, by="dive_id") %>%
  dplyr::select(-c(location.y)) %>%
  rename(location = location.x)

# creating date time column **(NEED to clean column names)**
rov_measurement_date <- rov_measurement_withmeta %>%
  mutate(month_num = case_when(month == "Jan" ~ "01",
                               month == "Jan " ~ "01",
                               month == "Feb" ~ "02",
                               month == "Mar" ~ "03",
                               month == "Apr" ~ "04")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  add_column(time_period = "present") 

ggplot (data = rov_measurement_date, aes(x = date, y = diameter)) +
  geom_point()

# creating date time column for HISTORIC DATA
unique(historic_measurement$month)

historic_measurement_date <- historic_measurement %>%
  mutate(month_num = case_when(month == "March" ~ "03",
                               month == "April" ~ "04",
                               month == "July" ~ "07",
                               month == "November" ~ "11")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  add_column(time_period = "past")

## QUICK PLOT diameter
ggplot (data = historic_measurement_date, aes(x = date, y = diameter)) +
  geom_point()

# HISTORIC for ABUNDANCE **(must fix all of this with adjusted dates)
historic_abundance_for_plot <- historic_measurement_date %>%
  group_by(month, year, date, time, location, diver) %>%
  count(location, diver) %>%
  rename("number" = "n") %>%
  add_column(time_period = "past") %>% 
  add_column(species = "Pisaster brevispinus") %>%
  ungroup() %>%
  as.data.frame() 

## make abundance # of stars/minute of survey 
historic_abund_per_min <- historic_abundance_for_plot %>%
  mutate(number_min = number/time)

# QUICK PLOT abundance
ggplot(data = historic_abund_per_min, aes(x= date, y = number_min)) +
  geom_point()

##### PLOT BOTH HISTORIC AND PRESENT RAW DATA DIAMETER AND ABUNDANCE #####
# combine data frames for ABUNDANCE
clean_abund_present_raw <- rov_abundance_date %>%
  select(c(location, date, time_period, number, number_min, time, species)) %>%
  filter(species == "Pisaster brevispinus") 

#HISTORIC DATA, no need to filter because only pink sea stars 
clean_abund_historic_raw <- historic_abund_per_min %>%
  select(c(location, date, time_period, number, time, number_min, species))

#MERGE ABUNDANCE COUNTS from 2 datashets into 1 (past and present data)
combined_abund_raw_plot <- full_join(clean_abund_historic_raw, clean_abund_present_raw)
print(combined_abund_raw_plot)


###### plot raw data for abundance **add more defined dates for graph ######
ggplot() +
  geom_point(data = combined_abund_raw_plot, position = position_dodge(width = 1), #how close u want dots together or apart
             aes(x = date, y = number_min, group = time_period, fill = time_period, color = time_period), shape = 21) + 
  labs(title = "Abundance at each site in 2 time periods", x = "Date", y = "Number of Stars/Minute") +
  theme_bw() +
  scale_fill_manual(values = c("#C82D47", "#5BA054")) + 
  scale_colour_manual(values = c("#C82D47", "#5BA054")) + #fill: only fill but shape must be fillable, color: only give the border
  theme_classic() 

### FOR DIAMETER ####
# combine data frames for diameter
clean_diameter_present_raw <- rov_measurement_date %>%
  select(c(location, date, time_period, diameter)) %>%
  na.omit(rov_measurement_date$diameter)

di_cl_pre <- clean_diameter_present_raw %>%
  mutate(diameter = as.numeric(diameter))


#HISTORIC DATA, no need to filter because only pink sea stars 
clean_diameter_historic_raw <- historic_measurement_date %>%
  select(c(location, date, time_period, diameter))

#MERGE ABUNDANCE COUNTS from 2 datashets into 1 (past and present data)
combined_diameter_raw_plot <- full_join(clean_diameter_historic_raw, di_cl_pre)
print(combined_diameter_raw_plot)

#### plot raw data diameters ####
ggplot() +
  geom_point(data = combined_diameter_raw_plot, position = position_dodge(width = 1), #how close u want dots together or apart
             aes(x = date, y = diameter, group = time_period, fill = time_period, color = time_period), shape = 21) + 
  geom_segment(data = combined_diameter_raw_plot, position = position_dodge(width = 1),
               aes(y = 0, yend = diameter, x = date, group = time_period, colour = time_period), linewidth = 1) +
  labs(title = "Diameters at each site in 2 time periods", x = "Date", y = "Diameter (cm)") +
  theme_bw() +
  scale_fill_manual(values = c("#C82D47", "#5BA054")) + 
  scale_colour_manual(values = c("#C82D47", "#5BA054")) + #fill: only fill but shape must be fillable, color: only give the border
  theme_classic() 

#####classification of present data individual sea stars #####
#ROV
###
rov_present <- rov_measurement_jan20_clean %>%
  mutate(diameter = as.numeric(diameter),
         depth_found = as.numeric(depth_found),
         location = as.factor(location),
         feeding = as.factor(feeding),
         feeding_type = as.factor(feeding_type),
         substrate = as.factor(substrate)) %>%
  add_column(time_period = "present") %>%
  add_column(year = "2025")

str(rov_present)

###
## PRESENT data individual sea star 
# TRANSECT #
###
str(transect_measurement)
transect_measurement_clean <- transect_measurement %>%
  mutate(diameter = as.numeric(diameter),
         location = as.factor(location),
         depth_found = as.numeric(depth_found),
         location = as.factor(location),
         feeding = as.factor(feeding),
         feeding_type = as.factor(feeding_type),
         substrate = as.factor(substrate)) %>%
  add_column(time_period = "present")

str(transect_measurement_clean)

### filter out the random fourth category for feeding type, a space 
transect_measurement_clean_feeding_type <- transect_measurement_clean %>%
  filter(!feeding_type %in% c(""))

unique(transect_measurement_clean_feeding_type$feeding_type)

###
#####classification of of historic data individual sea stars #####
###
str(historic_measurement)
rov_historic <- historic_measurement %>%
  mutate(diameter = as.numeric(diameter),
         location = as.factor(location),
         depth_low = as.numeric(depth_low),
         depth_upper = as.numeric(depth_upper),
         year = as.factor(year),
         feeding = as.factor(feeding)) %>%
  add_column(time_period = "past")

str(rov_historic)

###
######HISTORIC Data population abundance cleaning #######
###

# each diver is 1 survey, group them by how many observation each diver saw per location
historic_abundance <- rov_historic %>%
  group_by(month, year, location, diver) %>%
  count(location, diver) %>%
  rename("number" = "n") %>%
  add_column(time_period = "past") %>% 
  add_column(species = "Pisaster brevispinus") %>%
  ungroup() %>%
  as.data.frame()


###
#####PRESENT data population abundances cleaning #####
###
str(rov_abundance_jan20_clean)
present_abundance <- rov_abundance_jan20_clean %>%
  mutate(species = as.factor(species),
         number = as.numeric(number),
         location = as.factor(location),
         dive_id = as.factor(dive_id)) %>%
  add_column(time_period = "present") %>%
  add_column(year = "2025")

str(present_abundance)

rov_abundance_PS_only <- present_abundance %>%
  select(c(location, species, number)) %>% 
  filter(species == "Pisaster brevispinus") 

str(rov_abundance_PS_only)

### transect abundance cleaning
str(transect_abundance)

transect_abundance_PS_only <- transect_abundance %>%
  select(c(location, species, number)) %>% 
  filter(species == "Pisaster brevispinus") 


##### creating data frames for the models; need to create 4 of them ####
####Model_1A: data frame 1:historic and present site pop. abundances ####

### PRESENT data, to filter JUST PINK SEA STAR species
unique(present_abundance$species)
clean_abund_present <- present_abundance %>%
  select(c(dive_id, location, month, day, year, time_period, number, species)) %>%
  filter(species == "Pisaster brevispinus") 

#HISTORIC DATA, no need to filter because only pink sea stars 
clean_abund_historic <- historic_abundance %>%
  select(c(diver, location, year, time_period, number, species))%>%
  rename(dive_id = diver)

#MERGE ABUNDANCE COUNTS from 2 datashets into 1 (past and present data)
combined_abund <- full_join(clean_abund_historic, clean_abund_present)
print(combined_abund)

combined_abund_full <- combined_abund %>%
  mutate(years_past = (2025 - as.numeric(year)),
         wasting = case_when(year == "2010"|
                               year == "2011"|
                               year == "2012"|
                               year == "2013" ~ "pre",
                             year == "2014"|
                               year == "2015" ~ "post",
                             year == "2025" ~ "present"))


#save csv of abundance counts of both historic and present in one 

write.csv(combined_abund_full, "./manipulated_data/past_present_pop_abundances.csv", row.names=F)

#### model_1B: data frame 2: historic and present; individual diameters ####
str(rov_present)

#present data clean: select categories 
clean_diameter_present <- rov_present %>%
  select(c(location, time_period, year, diameter)) 

# past data clean: select categories
str(rov_historic)
clean_diameter_past <- rov_historic %>%
  select(c(location, time_period, year, diameter)) 

# merge 2 csv of past and present data
combined_diameter <- full_join(clean_diameter_past, clean_diameter_present)
print(combined_diameter)

#adding years past and wasting timeline columns #
combined_diameter_full <- combined_diameter %>%
  mutate(years_past = (2025 - as.numeric(year)),
         wasting = case_when(year == "2010"|
                               year == "2011"|
                               year == "2012"|
                               year == "2013" ~ "pre",
                             year == "2014"|
                               year == "2015" ~ "post",
                             year == "2025" ~ "present"))
                               

write.csv(combined_diameter_full, "./manipulated_data/past_present_indiv_diameter.csv", row.names = F)

#### model_2A: data frame 3: methods: present transect vs ROV site pop. abundances ####

#for ROV CLEAN
# categories: location, visibility, substrate_level, method(add column)
rov_abundance_model2 <- rov_abundance_date %>%
  mutate(species = as.factor(species),
         number = as.numeric(number),
         location = as.factor(location),
         dive_id = as.factor(dive_id))

rov_model_2_abun_location <- rov_abundance_model2 %>%
  select(c(location, visibility, substrate_level, dive_id, date, number, species)) %>%
  mutate(visibility = as.numeric(visibility),
         substrate_level = as.factor(substrate_level),
         location = as.factor(location)) %>%
  add_column(method = "roving") %>%
  select(-species, -date)
  
str(rov_model_2_abun_location)

#### for TRANSECT CLEAN ####
str(transect_location_info)
transect_location_data <- transect_location_info %>%
  select(c(location, visibility, substrate_level, dive_id)) %>%
  mutate(visibility = as.numeric(visibility),
         substrate_level = as.factor(substrate_level),
         location = as.factor(location)) %>%
  add_column(method = "transect")

str(transect_location_data)

##clean transect abundance sheet, isolate for pinks only 
transect_numbers_mod2 <- transect_abundance %>%
  mutate(location = as.factor(location)) %>%
  filter(species == "Pisaster brevispinus ") 

##### Join metadata to present TRANSECT data sheets #####

# left joining transect sheets: location+abundance
transect_abundance_withmeta <- transect_numbers_mod2 %>%
  left_join(transect_location_data, by="dive_id") %>%
  dplyr::select(-c(location.y)) %>%
  rename(location = location.x)

transect_abundance_date <- transect_abundance_withmeta %>%
  mutate(number = as.numeric(number),
         dive_id = as.factor(dive_id)) %>%
  select(-species, -common_name)
  

### MERGE BOTH METHODS AND INFORMATION INTO 1 CSV (make sure all columns same )
str(transect_abundance_date) ## boulder numbers are coming back wonky
str(rov_model_2_abun_location)


#### model_3A: data frame 4: present data, rov + transect: ecology model, individual diameters and feeding ####
## for model 3
 
# ROV data clean: select 
##location, diameter, substrate, feeding, feeding_type, depth_found,

str(rov_present)
clean_rov_ecology<- rov_present %>%
  select(c(location, diameter, substrate, depth_found, feeding, feeding_type)) %>%
  add_column(method = "roving") ## do i need to add the method for this?  if we are only looking at indiv.


# TRANSECT data clean: select categories
# categories: location, diameter, substrate, feeding, feeding_type, depth_found, method(add column); 

str(transect_measurement_clean_feeding_type)
clean_transect_ecology <- transect_measurement_clean_feeding_type %>%
  select(c(location, diameter, substrate, feeding, feeding_type, depth_found,)) %>%
  add_column(method = "transect")

###saving new .csv files for models 
write.csv(nameofdataframe, "./projectfolder/manipulateddatafolder/newnamefordatafram.csv", row.names = F)


#####FOR ROV DIAMETERS; HISTORIC AND PRESENT: just basic means and plot?####

###select specific columns wanting to keep
cleanrovpresent <- rov_present %>% 
  select(c(location, time_period, diameter))
cleanrovhistoric <- rov_historic %>%
  select(c(location, time_period, diameter))

###combine the 2 data sheets into 1 sheet
combined_rov <- full_join(cleanrovpresent, cleanrovhistoric)
print(combined_rov)
unique(combined_rov$time_period)

# Diameter means for each site at 2 different time periods 
group_rov <- combined_rov %>%
  dplyr::select(c(time_period, location, diameter)) %>%
  group_by(time_period, location) %>% #group values by region, then by site name
  summarise(diameter_mean = mean(diameter, na.rm=T), #tell r what to do with the diameter 
            diameter_sd = sd(diameter, na.rm=T)) 

######### PLOTTING GRAPHS ########
##sample plot for TIME PERIODS and DIAMETER, how do i get them diff coloured? 
ggplot() +
  geom_point(data = group_rov, position = position_dodge(width = 0.5), #how close u want dots together or apart
             aes(x = location, y = diameter_mean, group = time_period, fill = time_period, color = time_period), shape = 21) + 
  geom_segment(data = group_rov, position = position_dodge(width = 0.5),
               aes(y = 0, yend = diameter_mean, x = location, group = time_period, colour = time_period), linewidth = 1) +
   scale_fill_manual(values = c("blue", "#3cc08f")) + 
  scale_colour_manual(values = c("blue", "#3cc08f")) + #fill: only fill but shape must be fillable, color: only give the border
  theme_classic() +
  theme ( #make pretty
    
  )
  xlab("site") +
  ylab("diameter mean")

  #trial bar graph for diameters with past adn present 
ggplot(group_rov, aes(x = location, y = diameter_mean, fill = time_period)) +
  geom_bar(stat = "identity") +
  ggtitle("location and diameter") +
  xlab("site") +
  ylab("diameter mean")

#####try whisker plot (strip plot??) raw data?  ######
# for diameters?? 
ggplot() +
geom_jitter(, aes(shape = diameter, color = time_period), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 1.2) +
  stat_summary(
    aes(color = time_period),
    geom = "pointrange",  size = 0.4,
    position = position_dodge(0.8)) +
  scale_color_manual(values =  c("#00AFBB", "#E7B800"))

#still attempting whisker plot
ggplot(combined_rov, aes(x = location, y = diameter, fill = time_period)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.6) +
  stat_summary(
    aes(color = time_period),
    geom = "pointrange",  size = 0.4,
    position = position_dodge(0.8))+
  scale_color_manual(values =  c("#00AFBB", "#E7B800"))
  labs(title = "raw data diameter", y = "diameter (cm)") +
  theme_minimal()

## PLOTING ABUNDANCE STRAIGHT IN; HISTORIC AND PRESENT (this is all counts of all stars, need to filter)
ggplot(combined_abund, aes(x = location, y = number, fill = location)) +
  geom_bar(stat = "identity") +
  ggtitle("location and counts") +
  xlab("site") +
  ylab("abundance")


######lollipop graph #####
ggplot() +
  geom_point(data = combined_abund, position = position_dodge(width = 1), #how close u want dots together or apart
             aes(x = location, y = number, group = time_period, fill = time_period, color = time_period), shape = 21) + 
  geom_segment(data = combined_abund, position = position_dodge(width = 1),
               aes(y = 0, yend = number, x = location, group = time_period, colour = time_period), linewidth = 1) +
  scale_fill_manual(values = c("#C82D47", "#5BA054")) + 
  scale_colour_manual(values = c("#C82D47", "#5BA054")) + #fill: only fill but shape must be fillable, color: only give the border
  theme_classic() +
  theme (
  )
xlab("site") +
  ylab("number")


#Just bar graph past data abundances 
ggplot(clean_abund_historic, aes(x = location, y = number, fill = location)) +
  geom_bar(stat = "identity") +
  ggtitle("historic counts") +
  xlab("site") +
  ylab("abundance")

#just bar graphing present data abundances
ggplot(clean_abund_present, aes(x = location, y = number, fill = location)) +
  geom_bar(stat = "identity") +
  ggtitle("present counts") +
  xlab("site") +
  ylab("abundance")


######isolate by 1 site, keep location and diameter (2 columns) in this dataframe ######
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




