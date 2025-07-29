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
library(cowplot)
library(lme4)
library(Matrix)
library(emmeans)

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
ggplot(mod1A_data_clean, aes(x = number_min)) +
  geom_histogram(bins = 10)  

## selecting poisson distribution for count base response variable (because of histogram results)

##DISTRIBUTION CHOSEN: Poisson

##create model 1AA
mod1A <- glmmTMB(number_min ~ time_period + (1|location), 
                 family = tweedie(link ='log'), #distribution of the data
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
graph1A <- ggplot() +
  labs(x = "Time Period", y = "Number of stars/minute") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1A_data_clean, 
             aes(x = time_period, y = number_min, colour = time_period),
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1A_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period),
                  shape=16, size=0.7) +
   geom_jitter(width = 0.4, alpha = 0.5) +
  scale_colour_manual(values = c("#C82D47",
                                 "#5BA054"))
graph1A

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

## filter wasting periods: pre and present 
mod1B_filter_wasting <- mod1B_data_clean %>%
  filter(wasting != "post") %>%
  filter(!location %in% c("Boulder Island","Grey Rocks Island")) %>%
  droplevels()
  
#checkong histogram for distribution of data
ggplot(mod1B_filter_wasting, aes(x = number_min)) +
  geom_histogram(bins = 10)  


##DISTRIBUTION CHOSEN: tweedie?? 

##create model 1B
mod1B <- glmmTMB(number_min ~ wasting + (1|location), 
                 family = tweedie(link='log'), #distribution of the data
                 data = mod1B_filter_wasting)

##checking model fit with DHARMA
plot(simulateResiduals(mod1B))

##assess model output 
summary(mod1B)

##predicting 
predict_mod1B_data <- ggpredict(mod1B, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x) 


##now plot these predictions 
## historic, present, abundance at each site 
##visualizing not per site but per time frame

#to PLOT raw data and predicted values at same time  **FIX ORDER of categories (should be pre post present)
graph1B <- ggplot() +
  labs(x = "Wasting Categories", y = "Number of stars/minute") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1B_filter_wasting, 
             aes(x = wasting, y = number_min, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1B_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting),
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#3F88C5",
                                 "#5BA054"))

graph1B

##### ABUND_MOD1C:  WITH WASTING(POST AND PRESENT) WHACK #####
##load .csv
mod1C_data <- read_csv("./manipulated_data/past_present_pop_abundances.csv")

## remove NA
mod1C_data_clean <- na.omit(mod1C_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")),
         year = as.numeric(year)) %>%
  as.data.frame()

str(mod1C_data_clean)

## filter wasting periods: post and present 
mod1C_filter_wasting <- mod1C_data_clean %>%
  filter(wasting != "pre") %>%
  filter(location %in% c("Boulder Island","Grey Rocks Island")) %>%
  droplevels()

#checkong histogram for distribution of data
ggplot(mod1C_filter_wasting, aes(x = number_min)) +
  geom_histogram(bins = 10)  

## choosing distribution: 

##DISTRIBUTION CHOSEN: tweedie had error, gamma kinda works:?? 

##create model 1C
mod1C <- glmmTMB(number_min ~ wasting + (1|location), 
                 offset = log(),
                 family = poisson, #distribution of the data
                 data = mod1C_filter_wasting)

##checking model fit with DHARMA
plot(simulateResiduals(mod1C))

##assess model output 
summary(mod1C)

##plot the model, if in glmm (or non linear space) need to backtransform the data before plotting it 
#ploting not the values of the data, of the linked model space, but how it is used in the model 
#looking at the "linear" version of what u are presenting 

##predicting 
predict_mod1C_data <- ggpredict(mod1C, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x) 


##now plot these predictions 
## historic, present, abundance at each site 
##visualizing not per site but per time frame

#to PLOT raw data and predicted values at same time  **FIX ORDER of categories (should be pre post present)
graph1C <- ggplot() +
  labs(x = "Wasting Categories", y = "Number of stars/minute") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank())+
  geom_point(data= mod1C_filter_wasting, 
             aes(x = wasting, y = number_min, colour = wasting),
             position=position_jitter(width=0.2), alpha=1) +
  scale_colour_manual(values = c("#D5A021",
                                 "#5BA054"))
graph1C

##### ABUND_MOD1D: DATE  ######
##load .csv
mod1D_data <- read_csv("./manipulated_data/past_present_pop_abundances.csv")

## remove NA
mod1D_data_clean <- na.omit(mod1D_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")),
         year = as.numeric(year)) %>%
  as.data.frame()


## do i keep the wasting factor??? 
str(mod1D_data_clean)


#checkong histogram for distribution of data
ggplot(mod1D_data_clean, aes(x = number_min)) +
  geom_histogram(bins = 10)  

## choosing distribution: still count data, poison still?? 

##DISTRIBUTION CHOSEN: poisson???

##create model 1B
mod1D <- glmmTMB(number_min ~ date + (1|location), 
                 family = tweedie(link='log'), #distribution of the data
                 data = mod1D_data_clean)

##checking model fit with DHARMA
plot(simulateResiduals(mod1D))

##assess model outputD
summary(mod1D)

##plot the model, if in glmm (or non linear space) need to backtransform the data before plotting it 
#ploting not the values of the data, of the linked model space, but how it is used in the model 
#looking at the "linear" version of what u are presenting 

##predicting 
predict_mod1D_data <- ggpredict(mod1D, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x) 


##now plot these predictions 
## historic, present, abundance at each site 
##visualizing not per site but per time frame

#to PLOT raw data and predicted values at same time  **FIX ORDER of categories (should be pre post present)
ggplot() +
  labs(title = "abundance with date timeframe", x = "date", y = "number of stars/minute") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1D_filter_wasting, 
             aes(x = wasting, y = number_min, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1C_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting),
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#3F88C5",
                                 "#2C5530"))

###### DIAM_MOD1A:  time period *fix graph asthetics ######
#load csv data 
model_D1A_data <- read.csv("./manipulated_data/past_present_indiv_diameter.csv")

model_D1A_data_clean <- na.omit(model_D1A_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")), #makes ordercertain way
         diameter = as.numeric(diameter)) %>%
  as.data.frame()

str(model_D1A_data_clean)

#checkong histogram for distribution of data
ggplot(model_D1A_data_clean, aes(x = diameter)) +
  geom_histogram(bins = 10)  


## selecting teedie distribution for count base response variable (because of histogram results)

##DISTRIBUTION CHOSEN: tweedie distribution 

##create model D1A
modD1A <- glmmTMB(diameter ~ time_period + (1|location), 
                 family = tweedie(link = "log"), #distribution of the data
                 data = model_D1A_data_clean)

##checking model fit with DHARMA , its fine even with red
plot(simulateResiduals(modD1A))

## this one looks alright, maybe try others 

##assess model output 
summary(modD1A)

##predicting 
predict_modD1A_data <- ggpredict(modD1A, terms = "time_period") %>% #if many predictors, more terms 
         rename(time_period = x)

#to plot raw data and predicted values at same time 

graphD1A <- ggplot() +
  labs(x = "Time Period", y = "Diameter (cm)") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
   theme(panel.grid.minor = element_blank()) +
  geom_point(data = model_D1A_data_clean, 
             aes(x = time_period, y = diameter, colour = time_period),
             position=position_jitter(width=0.2), alpha=0.15) +
  geom_pointrange(data = predict_modD1A_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#C82D47",
                                 "#5BA054"))

graphD1A


###### DIAM_MOD1B: WASITN PRE AND PRESENT ######## 
#load csv data 
model_D1B_data <- read.csv("./manipulated_data/past_present_indiv_diameter.csv")

model_D1B_data_clean <- na.omit(model_D1B_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")), #makes ordercertain way
         diameter = as.numeric(diameter)) %>%
  as.data.frame()

str(model_D1B_data_clean)

model_D1B_filter_wasting <- model_D1B_data_clean %>%
  filter(wasting != "post") %>%
  filter(!location %in% c("Boulder Island","Grey Rocks Island")) %>%
  droplevels()


#checkong histogram for distribution of data
ggplot(model_D1B_filter_wasting, aes(x = diameter)) +
  geom_histogram(bins = 10)  



##DISTRIBUTION CHOSEN: tweedie distribution 

##create model D1B
modD1B <- glmmTMB(diameter ~ wasting + (1|location), 
                  family = tweedie(link="log"), #distribution of the data
                  data = model_D1B_filter_wasting)

##checking model fit with DHARMA , its fine even with red
plot(simulateResiduals(modD1B))

## this one looks alright, maybe try others 

##assess model output 
summary(modD1B)

##predicting 
predict_modD1B_data <- ggpredict(modD1B, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x)

#to plot raw data and predicted values at same time 

graphD1B <- ggplot() +
  labs( x = "Wasting Categories", y = "Diameter (cm)") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= model_D1B_filter_wasting, 
             aes(x = wasting, y = diameter, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.15) +
  geom_pointrange(data = predict_modD1B_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#3F88C5",
                                 "#5BA054"))
graphD1B

###### DIAM_MOD1C:WASTING POST AND PRESENT (take out for now in analysis)########
model_D1C_data <- read.csv("./manipulated_data/past_present_indiv_diameter.csv")

model_D1C_data_clean <- na.omit(model_D1C_data) %>%
  mutate(location = as.factor(location),
         time_period = as.factor(time_period),
         wasting = factor(wasting, levels=c("pre", "post", "present")), #makes ordercertain way
         diameter = as.numeric(diameter)) %>%
  as.data.frame()

str(model_D1C_data_clean)

model_D1C_filter_wasting <- model_D1C_data_clean %>%
  filter(wasting != "pre") %>%
  filter(location == c("Boulder Island","Grey Rocks Island")) %>%
  droplevels()

#checkong histogram for distribution of data
ggplot(model_D1C_filter_wasting, aes(x = diameter)) +
  geom_histogram(bins = 10)  



##DISTRIBUTION CHOSEN: tweedie distribution 

##create model D1C
modD1C <- glmmTMB(diameter ~ wasting + (1|location), 
                  family = tweedie(link="log"), #distribution of the data
                  data = model_D1C_filter_wasting)

##checking model fit with DHARMA , its fine even with red
plot(simulateResiduals(modD1C))

## this one looks alright, maybe try others 

##assess model output 
summary(modD1C)

##predicting 
predict_modD1C_data <- ggpredict(modD1C, terms = "wasting") %>% #if many predictors, more terms 
  rename(wasting = x)

#to plot raw data and predicted values at same time 

graphD1C <- ggplot() +
  labs( x = "Wasting Categories", y = "Diameter (cm)") +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= model_D1C_filter_wasting, 
             aes(x = wasting, y = diameter, colour = wasting),
             position=position_jitter(width=0.2), alpha=0.2) +
  geom_pointrange(data = predict_modD1C_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#D5A021",
                                 "#5BA054"))
###### DIAM_MOD1D:DATE (NOT DOING FOR NOW) #######
##### create multi panel plot  #####
top_row <- plot_grid(graph1A, graph1B,ncol = 2)
bot_row <- plot_grid(graphD1A, graphD1B, ncol = 2)

final_plot <- plot_grid(graph1A, graph1B,
                        graphD1A, graphD1B,
                        labels = c("A", "B", "C", "D"),
                        ncol = 2,
                        label_size = 14)
print(final_plot)

##### MODEL 2: ROV VS TRANSECT ##########
mod3_data <- read.csv("./manipulated_data/transect_rov_abun.csv")

model_3_data_clean <- na.omit(mod3_data) %>%
  mutate(location = as.factor(location))
         
str(model_3_data_clean)

#checkong histogram for distribution of data
ggplot(model_3_data, aes(x = transect_average)) +
  geom_histogram(bins = 10)  


## selecting teedie distribution for count base response variable (because of histogram results)

##DISTRIBUTION CHOSEN: tweedie distribution 

##create model D1A
unique(model_3_data_clean$location)

mod3 <- lm(transect_average ~ rov_average, 
                  data = model_3_data_clean)


mod.emt <- emtrends(mod3, ~1,
                    var = "rov_average")

mod.emt

test(mod.emt, null=1)

##checking model fit with DHARMA , its fine even with red
plot(simulateResiduals(mod3))

## this one looks alright, maybe try others 

##assess model output 
summary(mod3)

##predicting 
predict_mod3_data <- ggpredict(mod3, terms = "time_period") %>% #if many predictors, more terms 
  rename(time_period = x)

#to plot raw data and predicted values at same time 
## plot it and then add error 
## if the 1:1 liune it to error 
graphD1A <- ggplot() +
  labs(x = "Time Period", y = "Diameter (cm)") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black")) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data = model_3_data_clean, 
             aes(x = time_period, y = diameter, colour = time_period),
             position=position_jitter(width=0.2), alpha=0.15) +
  geom_pointrange(data = predict_mod3_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period), 
                  shape=16, size=0.7) +
  scale_colour_manual(values = c("#C82D47",
                                 "#5BA054"))

##### MODEL 3: LIFE HISTORY: PRESENT #######

##### for presentation #####
## plot A
ggplot() +
  labs(x = "Time period", y = "Number of stars/minute") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20)) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1A_data_clean, 
             aes(x = time_period, y = number_min, colour = time_period),
             shape=16, size= 5,
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1A_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period),
                  shape=16, size=1.5) +
  geom_jitter(width = 0.4, alpha = 0.5) +
  scale_colour_manual(values = c("#C82D47",
                                 "#5BA054")) 

## plot C
ggplot() +
  labs(x = "Time period", y = "Diameter (cm)") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20)) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data = model_D1A_data_clean, 
             aes(x = time_period, y = diameter, colour = time_period),
             shape=16, size= 3,
             position=position_jitter(width=0.2), alpha=0.22) +
  geom_pointrange(data = predict_modD1A_data, #adding predictors to graph with std
                  aes(x = time_period, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=time_period), 
                  shape=16, size=1.3) +
  scale_colour_manual(values = c("#C82D47",
                                 "#5BA054"))

#graph B
 ggplot() +
  labs(x = "Wasting categories", y = "Number of stars/minute") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20)) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= mod1B_filter_wasting, 
             aes(x = wasting, y = number_min, colour = wasting),
             shape=16, size= 4,
             position=position_jitter(width=0.2), alpha=0.4) +
  geom_pointrange(data = predict_mod1B_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting),
                  shape=16, size=1.5) +
  scale_colour_manual(values = c("#3F88C5",
                                 "#5BA054"))
## graph D 
 ggplot() +
  labs( x = "Wasting categories", y = "Diameter (cm)") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 20),  
        axis.text.y = element_text(size = 20)) +
  theme(panel.grid.minor = element_blank()) +
  geom_point(data= model_D1B_filter_wasting, 
             aes(x = wasting, y = diameter, colour = wasting),
             shape=16, size= 3,
             position=position_jitter(width=0.2), alpha=0.25) +
  geom_pointrange(data = predict_modD1B_data, #adding predictors to graph with std
                  aes(x = wasting, y = predicted, ymin = conf.low, ymax = conf.high,
                      color=wasting), 
                  shape=16, size=1.3) +
  scale_colour_manual(values = c("#3F88C5",
                                 "#5BA054"))
graphD1B
