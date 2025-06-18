#Title: Modelling
#Author: Nicole
#Date: June 12, 2025

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
library(glmmTMB)
library(ggeffects)


##### MODEL 1: HISTORICAL VS PRESENT ####### 
##load .csv
mod1A_data <- read_csv("./manipulated_data/past_present_pop_abundances.csv")

## remove NA
mod1A_data_clean <- na.omit(mod1A_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period)) %>%
  as.data.frame()

str(mod1A_data_clean)

#checkong histogram for distribution of data
ggplot(mod1A_data, aes(x = number)) +
  geom_histogram(bins = 10)  

## selecting poisson distribution for count base response variable (because of histogram results)


##fitting glmm with a poisson distribution 


##create model 1A
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
predict_mod1A_data <- ggpredict(mod1A, terms = "time_period") %>% #if many predictors, more terms 
         rename(time_period = x)

##now plot these predictions 
## historic, present, abundance at each site 
##visualizing not per site but per time frame 

#to plot raw data and predicted values at same time 
ggplot(mod1A_data_clean, aes(x = time_period, y = number)) +
  geom_point() + ##this and line above plots raw data
  geom_pointrange(data = predict_mod1A_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high)) +
  theme_classic()

# lighter point- alpha values
# clearer points- jitter them
# make predicted values bigger 



##### MODEL 2: ROV VS TRANSECT ##########

##### MODEL 3: LIFE HISTORY: PRESENT #######


