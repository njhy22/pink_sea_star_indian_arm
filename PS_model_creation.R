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


##### MODEL 1A: HISTORICAL VS PRESENT abundance, time period and years past ####### 
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
mod1A <- glmmTMB(number ~ time_period + years_past + (1|location), 
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
predict_mod1A_data <- ggpredict(mod1A, terms = c("time_period", "years_past")) %>% #if many predictors, more terms 
         rename(time_period = x)

##now plot these predictions 
## historic, present, abundance at each site 
##visualizing not per site but per time frame

#to PLOT raw data and predicted values at same time 
ggplot() +
  labs(x = "Years Past since 2025", y = "Number of Stars / Survey") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1A_data_clean, 
             aes(x = time_period, y = number, colour = time_period),
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1A_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period),
                  shape=16, size=0.7) +
  # geom_jitter(width = 0.4, alpha = 0.5) +
  scale_colour_manual(values = c("#C82D47",
                               "#5BA054"))

# lighter point- alpha values
# clearer points- jitter them
# make predicted values bigger 

##### MODEL 1B: past+present, abundance, with wasting and years past #####

###### MODEL 1C: past+present Diameter ######

#load csv data 
model1B_data <- read.csv("./manipulated_data/past_present_indiv_diameter.csv")

mod1B_data_clean <- na.omit(model1B_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")), #makes ordercertain way
         diameter = as.numeric(diameter)) %>%
  as.data.frame()

str(mod1B_data_clean)

#checkong histogram for distribution of data
ggplot(model1B_data, aes(x = diameter)) +
  geom_histogram(bins = 10)  

## selecting teedie distribution for count base response variable (because of histogram results)

##fitting glmm with a tweedie distribution 


##create model 1B
mod1B <- glmmTMB(diameter ~ wasting + years_past + (1|location), 
                 family = tweedie(link = "log"), #distribution of the data
                 data = mod1B_data_clean)

##checking model fit with DHARMA
plot(simulateResiduals(mod1B))
## this one looks alright, maybe try others 

##assess model output 
summary(mod1B)

##plot the model, if in glmm (or non linear space) need to backtransform the data before plotting it 
#ploting not the values of the data, of the linked model space, but how it is used in the model 
#looking at the "linear" version of what u are presenting 

##predicting 
predict_mod1B_data <- ggpredict(mod1B, terms = "time_period") %>% #if many predictors, more terms 
  rename(time_period = x)

##now plot these predictions 
## historic, present, diameters at each site 
##visualizing not per site but per time frame 

#to plot raw data and predicted values at same time 

ggplot() +
  labs(x = "Time Period", y = "Diameter (cm)") +
  theme_classic() +
   theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1B_data_clean, 
             aes(x = time_period, y = diameter, colour = time_period),
             position=position_jitter(width=0.2), alpha=0.2) +
  geom_pointrange(data = predict_mod1B_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#C82D47",
                                 "#5BA054"))


## create column for pre, post, present?? ##

## create column for years passed since present ?? ###

##### MODEL 2: ROV VS TRANSECT ##########
mod2A_data <- read.csv("./manipulated_data/present_methods_compare.csv")
##### MODEL 3: LIFE HISTORY: PRESENT #######


