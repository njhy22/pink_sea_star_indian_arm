#Title: Model 1 Pink Sea Star
#Author: Nicole Yang
#Date: May 22, 2025

library(usethis)

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
  filter(species == "Pisaster brevispinus")

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

rov_average_abun <- rov_model_2_abun_location %>%
  group_by(location) %>%
 summarise(mean_value = mean(number)) 

## dataframe JUST for raw data points/ surveys 
rov_plot_abun <- rov_model_2_abun_location %>%
  select(c(location, number, method))
 

#### for TRANSECT clean (grouping and getting mean) ####
#need to add both T1 together, then T2, then average the 2 
## so each site will have 2 rows, then into1 row for average
# but need the 2 rows (totals for T1 and T2) for the actual model 

  
  df <- transect_numbers_mod2 %>%
    mutate(TransectGroup = str_extract(dive_id, "T[12]"))
  
df_summary <- df %>%
  group_by(location, TransectGroup) %>%
  summarise(
    transect_raw_num = mean(number, na.rm = TRUE),
    .groups = "drop") %>%
  add_column(method = "transect")

  
transect_average_abun <- transect_abundance_date %>%
  group_by(location) %>%
  summarise(mean_value = mean(number)) 

#combine ROV and TRANSECT abundances together into 1 dataframe **NOT CORRECT 
transect_rov_means_abun <- transect_average_abun %>%
  left_join(rov_average_abun, by="location") %>%
  rename(transect_mean = mean_value.x) %>%
  rename(rov_mean = mean_value.y) 
  mutate(abund_delta = rov_mean - transect_mean)

  df_full <- df_summary %>%
    left_join(rov_plot_abun, by="location")

  
  ## Plot: Ranked differences in abundance b/w methods, or just pliotting the values
  # has the code for the line in the middle 
ggplot() +
  labs(x = "Abundance", y = "Site") +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 10, linetype = "solid", color = "black") +
  geom_point(data = df_summary, 
    aes(x = Transect, y = location, color = "Transect")) +
  geom_point(data = rov_abundance_date, 
    aes(x = number, y = location, color = "ROV")) +
  scale_colour_manual(values = c("Transect" = "#C4D4D3", "ROV" = "#341C1C"))

## PLOT one method on x axis, 1 method on y axis *the transcts are not added together 
ggplot() +
  labs(x = "ROV", y = "TRANSECT") +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data = transect_abundance_date, 
             aes(x = number, y = location, color = "Transect")) +
  geom_point(data = rov_abundance_date, 
             aes(x = number, y = location, color = "ROV")) +
  scale_colour_manual(values = c("Transect" = "green", "ROV" = "#341C1C"))

# rov on one axis and transect on other???
df_long <- df_full %>%
  pivot_longer(
    cols = c(transect_raw_num, number),
    names_to = "Method",
    values_to = "Value"
  )

ggplot(df_full, aes(x = number, y = transect_raw_num, color = method.x )) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Transect vs. ROV Survey",
       x = "ROV Survey",
       y = "Transect Survey") +
  theme_classic() +
  scale_colour_manual(values = c("transect_raw_num" = "green", "number" = "#341C1C"))



##### for raw data plots ####
### raw data points 
 ggplot() +
  labs(x = "Wasting Categories", y = "Number of stars/minute") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank())+
  geom_point(data= combined_diameter_full, 
             aes(x = wasting, y = diameter, colour = wasting),
             position=position_jitter(width=0.2), alpha=1) +
  scale_colour_manual(values = c("#D5A021",
                                 "#5BA054"))
