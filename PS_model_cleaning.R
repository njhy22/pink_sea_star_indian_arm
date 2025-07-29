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
library(ggrepel)

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

write.csv(transect_rov_means_abun, "./manipulated_data/transect_rov_abun.csv", row.names=F)

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
#### for TRANSECT CLEAN for MOD2 ####
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

rov_average_abun_22 <- rov_model_2_abun_location %>%
  group_by(location) %>%
 summarise(mean_value = mean(number)) 

## dataframe JUST for raw data points/ surveys 
rov_plot_abun <- rov_model_2_abun_location %>%
  select(c(location, number, method))
 

#### for TRANSECT clean (grouping and getting mean) ####
#need to add both T1 together, then T2, then average the 2 
## so each site will have 2 rows, then into 1 row for average
# but need the 2 rows (totals for T1 and T2) for the actual model 

  df <- transect_numbers_mod2 %>%
    mutate(TransectGroup = str_extract(dive_id, "T[12]"))
  
## adding together each side of the transect (T1 and T2) 
df_summary <- df %>%
  group_by(location, TransectGroup) %>%
  summarise(
    transect_raw_num = sum(number, na.rm = TRUE),
    .groups = "drop") %>%
  add_column(method = "transect")

## site average using both transects 
transect_averages <- df_summary %>%
  group_by(location) %>%
  summarise( transect_average = mean(transect_raw_num)) 

#combine ROV and TRANSECT abundances together into 1 dataframe **NOT CORRECT 
transect_rov_means_abun <- transect_averages %>%
  left_join(rov_average_abun, by="location") %>%
  rename(rov_average = "mean_value") %>%
  mutate(abund_delta = rov_average - transect_average)%>%
  mutate(overall_avg = (rov_average + transect_average) /2)

  
  ## Plot: Ranked differences in abundance b/w methods, or just pliotting the values
  # has the code for the line in the middle 
## KEEP THIS: MAIN PLOT for Q2 
## astyehtics, add the dot with the lines (lollipop) 
## make dots bigger
## change make light grey the line, or dotted isntead of black if needed
## only add colors if add to information
## add visuals and arrows and text of what the dots on either side mean 
## transect estimated more, on left 
## rov estimated more: on right
# middle: exact same numbers found like each found 20 stars, but this one is just moody where its 0 
ggplot() +
  labs(x = "Difference in abundance", y = "") +
  theme_classic(base_size =16) +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits=c(-46, 46)) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_point(data = transect_rov_means_abun, 
    aes(x = abund_delta, y = fct_reorder(location, -abs(transect_average))), 
    shape = 16, size =2.5) +
geom_segment(data = transect_rov_means_abun, position = position_dodge(width = 1),
    aes(x = 0, xend = abund_delta, y = location), linewidth = 1) 


# i can say that there are otherfactors probably in play, like vis and depth and thats being done later
# or we dont say anything and if someone asks u know 


## GRAPH with ROV and Transect ONE ON EACH AXIS 
## do i want to put sites?? like color each dot by site? no real point tbh just pretty 
#Each site average is from 2 roving and 2 belt transect surveys 
ggplot() +
  labs(x = "Average roving abundance", y = "Average transect abundance") +
  theme_classic(base_size = 20) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(color = "black"),
   axis.text = element_text(color = "black"),
   axis.text.x = element_text(size = 20),  
   axis.text.y = element_text(size = 20)) +
scale_x_continuous(limits=c(0, 120), breaks = seq(0, 120, 20)) +
scale_y_continuous(limits=c(0, 120), breaks = seq(0, 120, 20)) +
coord_fixed() +
  geom_point(data = transect_rov_means_abun, 
             aes(x = rov_average, y = transect_average),
             shape=16, size=3.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
geom_text_repel(data = transect_rov_means_abun, 
          aes(x = rov_average, y = transect_average, label = location), 
          size = 5,
          box.padding = 0.5,         # Padding around text labels
          point.padding = 1.1,       # Space between points and labels
          max.overlaps = Inf,        # Show all labels
          force = 4,                 # Amount of repulsion (increase for more spreading)
          force_pull = 0.5)         # How strongly labels are pulled toward points

## run this with random effect (glmm for random effect)
trial <- lm(data = transect_rov_means_abun, transect_average ~ rov_average)
summary(trial)

## explains variation with R^2
## correlated 



## i could find the proporttions of how close it is?? i have the deltas at least 
##### plot raw data for Q2: rov adn transect abundances #####

method_compare <- merge(df_summary,rov_plot_abun, by="location")

dff1 <- method_compare %>%
 transmute(location, method = method.x, abundance = transect_raw_num)
dff2 <- method_compare %>%
  transmute(location, method = method.y, abundance = number)
method_compare_combine <- bind_rows(dff1, dff2)
  
  
  ggplot() +
    labs(x = "Location", y = "Abundance") +
    theme_classic(base_size = 20) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 18),   
          axis.text.y = element_text(size = 20)) +
    geom_point(data = method_compare_combine, 
               aes(x = location, y = abundance, color = method),
               shape=16, size=3.5) 
  
##### for raw data plots ####
### raw data points 

# QUICK PLOT abundance with dates 
ggplot(data = historic_abund_per_min, aes(x= date, y = number_min)) +
  geom_point()

ggplot(data = transect, aes(x= location, y = number_min, color = time_period)) +
  geom_point(shape=16, size=3.5) +
  labs(x = "Locations", y = "Number of stars/minute") +
  theme_classic(base_size = 18) +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  
        axis.text.y = element_text(size = 18)) +
  theme(panel.grid.minor = element_blank())+
  scale_colour_manual(values = c("#5BA054", 
                                 "#C82D47"))

# BASIC plot plotting just past or present surveys, per site, each dot being #star/min 

combined_abund_complete
ggplot(data = combined_abund_complete, aes(x= location, y = number_min, color = time_period)) +
  geom_point(shape=16, size=3.5) +
  labs(x = "Locations", y = "Number of stars/minute") +
  theme_classic(base_size = 18) +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 18),  
        axis.text.y = element_text(size = 18)) +
  theme(panel.grid.minor = element_blank())+
  scale_colour_manual(values = c("#5BA054", 
                                 "#C82D47"))

#####lollipop graph only for ROV??? 
## if i do "combined abund raw plot" or "combine abund complete" its the same look cause mean 
### BUT the raw plot has jug island past and present, while the complete doesnt becasue no time 
ggplot() +
  geom_point(data = combined_abund_raw_plot, position = position_dodge(width = 1), #how close u want dots together or apart
             aes(x = location, y = number, group = time_period, fill = time_period, color = time_period), 
             shape=16, size=3.5) + 
  geom_segment(data = combined_abund_raw_plot, position = position_dodge(width = 1),
               aes(y = 0, yend = number, x = location, group = time_period, colour = time_period), linewidth = 1.5) +
  scale_fill_manual(values = c("#C82D47", "#5BA054")) + 
  scale_colour_manual(values = c("#C82D47", "#5BA054")) + #fill: only fill but shape must be fillable, color: only give the border
  theme_classic(base_size = 15) +
  theme(axis.title = element_text(color = "black"),
                axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  
        axis.text.y = element_text(size = 15)) +
xlab("Location") +
  ylab("Number of Stars")


## JUST MOODY INLET (cause dramatic)
moody <- combined_abund_raw_plot %>%
  filter(location == "Moody Inlet")

ggplot(data = moody, aes(x= time_period, y = number_min)) +
  geom_point(shape=16, size=4) +
  theme_classic(base_size = 16) +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text( size = 15),  
        axis.text.y = element_text(size = 15)) +
  geom_abline(slope = 0, intercept = 0, linetype = "dashed", color = "black") +
  xlab("Time period") +
  ylab("Number of stars/minute")

## sites where not much changed

boulder <- combined_abund_raw_plot %>%
  filter(location == "Boulder Island")

ggplot(data = boulder, aes(x= time_period, y = number)) +
  geom_point(shape=16, size=4) +
  scale_y_continuous(limits=c(0, 46)) +
  theme_classic(base_size = 16) +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text( size = 15),  
        axis.text.y = element_text(size = 15)) +
  xlab("Time period") +
  ylab("Number of stars")


## diameter
combined_diameter_full
### find mean of diameter in each site for past and present 
## then plot each site as average diameter per site per time period
ggplot(data = combined_diameter_raw_plot, aes(x= location, y = diameter, color = time_period),
       position=position_jitter(width=0.2), alpha=1) +
  geom_point() +
  labs(x = "Locations", y = "Number of stars/minute") +
  theme_classic() +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank())+
  scale_colour_manual(values = c("#5BA054", 
                                 "#C82D47"))

##### percent changes between present and past #####

historic_abundance_pc <- historic_measurement_date %>%
  group_by(month, year, date, time, location, diver) %>%
  count(location, diver) %>%
  rename("number" = "n") %>%
  add_column(time_period = "past") %>% 
  add_column(species = "Pisaster brevispinus") %>%
  ungroup() %>%
  as.data.frame() %>%
  select(c("location", "number", "time_period")) %>%
  group_by(location) %>%
  summarise(number = sum(number, na.rm = TRUE), .groups = "drop")

rov_abundance_pc <- rov_abundance_withmeta %>%
  mutate(month_num = case_when(month == "Jan" ~ "01",
                               month == "Jan " ~ "01",
                               month == "Feb" ~ "02",
                               month == "Mar" ~ "03",
                               month == "Apr" ~ "04")) %>%
  unite(col = datefull, c("year", "month_num", "day"), sep = "-", remove = FALSE) %>%
  mutate(date = as.Date(datefull)) %>%
  filter(species == "Pisaster brevispinus") %>%
  add_column(time_period = "present") %>%
  select(c("location", "number", "time_period")) %>%
  group_by(location) %>%
  summarise(number = sum(number, na.rm = TRUE), .groups = "drop")

## merging (make sure each location is only 1 dot)
abundance_merge <- merge(historic_abundance_pc, rov_abundance_pc, by = "location") %>%
  rename(past_num = number.x) %>%
  rename(present_num = number.y) %>%
  mutate(percent_change = ((present_num - past_num) / past_num) * 100)

ggplot() +
  labs(x = "Percent change", y = "Site") +
  theme_classic(base_size =16) +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  geom_point(data = abundance_merge, 
             aes(x = percent_change, y = fct_reorder(location, -abs(percent_change))), 
             shape = 16, size =2.5) +
  geom_segment(data = abundance_merge, position = position_dodge(width = 1),
               aes(x = 0, xend = percent_change, y = location), linewidth = 1) 


  