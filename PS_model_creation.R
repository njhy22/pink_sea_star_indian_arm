#Title: Modelling
#Author: Nicole
#Date: June 12, 2025

##### Load packages #####
library(usethis)
use(git)
library(tidyverse)
library(MetBrewer)
library(dplyr) 
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(DHARMa)
library(glmmTMB)
library(ggeffects)


##### ABUND_MOD1A:  time period (historic and present) *fix graph asthetic ####### 
##load .csv
mod1A_data <- read_csv("./manipulated_data/past_present_pop_abundances.csv")

## remove NA
mod1A_data_clean <- na.omit(mod1A_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = as.factor(wasting)) %>%
  as.data.frame()

str(mod1A_data_clean)

#checkong histogram for distribution of data
ggplot(mod1A_data, aes(x = number)) +
  geom_histogram(bins = 10)  

## selecting poisson distribution for count base response variable (because of histogram results)

##DISTRIBUTION CHOSEN: Poisson

##create model 1AA
mod1A <- glmmTMB(number ~ time_period + (1|location), 
                 family = poisson(link='log'), #distribution of the data
                 data = mod1A_data_clean)
                 
##checking model fit with DHARMA
plot(simulateResiduals(mod1A))

##assess model output 
summary(mod1A)

##plot the model, if in glmm (or non linear space) need to backtransform the data before plotting it 
#ploting not the values of the data, of the linked model space, but how it is used in the model 
#looking at the "linear" version of what u are presenting 

##predicting 
predict_mod1A_data <- ggpredict(mod1A, terms = "time_period") %>%
  rename(time_period = x)#if many predictors, more terms 
      
##now plot these predictions 
## historic, present, abundance at each site 
##visualizing not per site but per time frame

#to PLOT raw data and predicted values at same time 
ggplot() +
  labs(x = "Time Period", y = "Abundance") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1A_data_clean, 
             aes(x = time_period, y = number, colour = time_period),
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1A_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period),
                  shape=16, size=0.7) +
   geom_jitter(width = 0.4, alpha = 0.5) +
  scale_colour_manual(values = c("#C82D47",
                               "#5BA054"))

# lighter point- alpha values
# clearer points- jitter them
# make predicted values bigger 

##### ABUND_MOD1B: with WASTING(PRE AND PRESENTT) #####
##load .csv
mod1B_data <- read_csv("./manipulated_data/past_present_pop_abundances.csv")

## remove NA
mod1B_data_clean <- na.omit(mod1B_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")),
         year = as.numeric(year)) %>%
  as.data.frame()

str(mod1B_data_clean)

#checkong histogram for distribution of data
ggplot(mod1B_data, aes(x = number)) +
  geom_histogram(bins = 10)  

## choosing distribution: still count data, poison still?? 

##DISTRIBUTION CHOSEN: poisson???

##create model 1B
mod1B <- glmmTMB(number ~ wasting + (1|location), 
                 family = poisson(link='log'), #distribution of the data
                 data = mod1B_data_clean)

##checking model fit with DHARMA
plot(simulateResiduals(mod1B))

##assess model output 
summary(mod1B)

##plot the model, if in glmm (or non linear space) need to backtransform the data before plotting it 
#ploting not the values of the data, of the linked model space, but how it is used in the model 
#looking at the "linear" version of what u are presenting 

##predicting 
predict_mod1B_data <- ggpredict(mod1B, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x) 


##now plot these predictions 
## historic, present, abundance at each site 
##visualizing not per site but per time frame

#to PLOT raw data and predicted values at same time  **FIX ORDER of categories (should be pre post present)
ggplot() +
  labs(title = "abundance with wasting timeframe", x = "wasting categories", y = "Abundance") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1B_data_clean, 
             aes(x = wasting, y = number, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1B_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting),
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#D5A021",
                                 "#2C5530",
                                 "#3F88C5"))

##### ABUND_MOD1C:  WITH WASTING(POST AND PRESENT)
##### ABUND_MOD1D: DATE  ######
###### DIAM_MOD1A:  time period *fix graph asthetics ######

#load csv data 
model1C_data <- read.csv("./manipulated_data/past_present_indiv_diameter.csv")

mod1C_data_clean <- na.omit(model1C_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")), #makes ordercertain way
         diameter = as.numeric(diameter)) %>%
  as.data.frame()

str(mod1C_data_clean)

#checkong histogram for distribution of data
ggplot(model1C_data, aes(x = diameter)) +
  geom_histogram(bins = 10)  


## selecting teedie distribution for count base response variable (because of histogram results)

##DISTRIBUTION CHOSEN: tweedie distribution 

##create model 1C
mod1C <- glmmTMB(diameter ~ time_period + (1|location), 
                 family = tweedie(link = "log"), #distribution of the data
                 data = mod1C_data_clean)

##checking model fit with DHARMA , its fine even with red
plot(simulateResiduals(mod1C))

## this one looks alright, maybe try others 

##assess model output 
summary(mod1C)

##predicting 
predict_mod1C_data <- ggpredict(mod1C, terms = "time_period") %>% #if many predictors, more terms 
         rename(time_period = x)

#to plot raw data and predicted values at same time 

ggplot() +
  labs(title = "size of star as time period", x = "Time Period", y = "Diameter (cm)") +
  theme_classic() +
   theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1C_data_clean, 
             aes(x = time_period, y = diameter, colour = time_period),
             position=position_jitter(width=0.2), alpha=0.2) +
  geom_pointrange(data = predict_mod1C_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#C82D47",
                                 "#5BA054"))


###### DIAM_MOD1B: WASITN PRE AND PRESENT ######## 
###### DIAM_MOD1C:WASTING POST AND PRESENT########
###### DIAM_MOD1D:DATE #######
###### MODEL 1D: past present, diameter, wasting, 3 levels ##### 
#load csv data 
model1D_data <- read.csv("./manipulated_data/past_present_indiv_diameter.csv")

mod1D_data_clean <- na.omit(model1D_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")), #makes ordercertain way
         diameter = as.numeric(diameter)) %>%
  as.data.frame()

str(mod1D_data_clean)

#checkong histogram for distribution of data
ggplot(model1D_data, aes(x = diameter)) +
  geom_histogram(bins = 10)  


## selecting teedie distribution for count base response variable (because of histogram results)
##DISTRIBUTION CHOSEN: tweedie distribution 

##create model 1D
mod1D <- glmmTMB(diameter ~ wasting + (1|location), 
                 family = tweedie(link = "log"), #distribution of the data
                 data = mod1D_data_clean)

##checking model fit with DHARMA (IT LOOKS BAD)

plot(simulateResiduals(mod1D))
## this one looks alright, maybe try others 

##assess model output 
summary(mod1D)
mod
##predicting 
predict_mod1D_data <- ggpredict(mod1D, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x) 

#to plot raw data and predicted values at same time 

ggplot() +
  labs(title = "wasting time periods and diameter", x = "wasting categories", y = "Diameter (cm)") +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1D_data_clean, 
             aes(x = wasting, y = diameter, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.2) +
  geom_pointrange(data = predict_mod1D_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#D5A021",
                                 "#2C5530",
                                 "#3F88C5"))



#### ATTEMPT TO SEPERATE B/W PRE AND PRESENT #####
df_subset <- subset(mod1D_data_clean, wasting != "pre")
df_subset$wasting <- droplevels(df_subset$wasting)
df_subset2 <- subset(df_subset, location %in% c("Boulder Island", "Grey Rocks Island"))

mod1DD <- glmmTMB(diameter ~ wasting + (1|location), 
                 family = tweedie(link = "log"), #distribution of the data
                 data = df_subset2)

plot(simulateResiduals(mod1DD))
## this one looks alright, maybe try others 

##assess model output 
summary(mod1DD)

# prediction 
predict_mod1DD_data <- ggpredict(mod1DD, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x) 

ggplot() +
  labs(title = "post wasting+present and diameter", x = "wasting categories", y = "Diameter (cm)") +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= df_subset2, 
             aes(x = wasting, y = diameter, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.2) +
  geom_pointrange(data = predict_mod1DD_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#D5A021",
                                 "#2C5530"))

###### for pre and present WASTING SEPERATING diameter #####

df_subset11 <- subset(mod1D_data_clean, wasting != "post")
df_subset11$wasting <- droplevels(df_subset11$wasting)
df_subset3 <- subset(df_subset11, !location %in% c("Boulder Island", "Grey Rocks Island"))
df_subset3$location <- droplevels(df_subset3$location)

mod1DDD <- glmmTMB(diameter ~ wasting + (1|location), 
                  family = tweedie(link = "log"), #distribution of the data
                  data = df_subset3)

plot(simulateResiduals(mod1DDD))
## this one looks alright, maybe try others 

##assess model output 
summary(mod1DDD)

predict_mod1DDD_data <- ggpredict(mod1DDD, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x) 

ggplot() +
  labs(title = "pre wasting+present and diameter", x = "wasting categories", y = "Diameter (cm)") +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= df_subset3, 
             aes(x = wasting, y = diameter, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.2) +
  geom_pointrange(data = predict_mod1DDD_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#D5A021",
                                 "#2C5530"))

##### MODEL 2: ROV VS TRANSECT ##########
mod2A_data <- read.csv("./manipulated_data/present_methods_compare.csv")
##### MODEL 3: LIFE HISTORY: PRESENT #######


