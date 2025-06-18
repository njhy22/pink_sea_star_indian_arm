#Title: R things that are useful for coding 
#June 3, 2025

d <- your_data

## Ex/ columns of 'd'
# site_code
# site_type
# abundance

#N/A remove = True take out N/a's from the rows 
na.rm=T

# key dplyr functions 
mutate() #how to edit column (changing class to factor, numeric, etc), changing N/A to 0's, would be mutate, or adding a new column
select() #sometimes finicky, have to write dplyr with it ex. dplyr::select()
filter()
case_when() #help to make new columns that is conditional on other columns 
group_by() #how u do means, average, grouping, std, start by largest group first (broadest) then more levels in group 
  #group by categories, then the things that go into the grouping get averaged and put in 
summarise() #need to be with group_by. specifies what is being done to the thing(columns) getting grouped
add_column(Time = "present") #add to each dataframe seperately, time is column name, present is the level, make sure column name same in both CSV files

## Ex/ Creating 'site name' column
d_clean <- d %>%
  mutate(site_code = as.factor(site_code),
         site_type = as.factor(site_type)) %>%
  mutate(site_name = case_when(site_code == "ATQ_23_J" ~ "Jug Island",
                               site_code == "ATQ_23_CS" ~ "Cates Park Shallow",
                               site_code == "ATQ_23_CD" ~ "Cates Park Deep"))

###### what symbols mean what####### 
# :: means where the function comes from 
# Logical statements   
# is equal to '==' ex. 3 sp of stars, only want 2,say sp equals 2 sp
# is not equal to '!=' ex. 3 sp of stars, want to exclude 1 sp, sp is everything but that 1
# OR '|'
# AND '&'
# ',' means same function, so continue using same function

#. %in% 
# combined with filter, condition for a group of things
#ex. filtering out levels from a column of 'species'

## Ex/ columns of 'e'
# site_code
# site_type
#species (factor levels: bats, cows, cats,frogs,deer)
# abundance

#example
#filtering out species u want, create this list, tells you want you want from it
mammals <- c("bats", "cows", "cats", "deer")
e <- your_other_data
e_frogs <- e %>%
  filter(species == "frogs") #if only wanted frogs, like only 1 species 

e_mammals <- e %>%
  filter(mammals %in% species) #filtering for only things you want, not filtering entire dataframe, just in the column 

#removing columns you dont need (ex. remove site_code)
e_clean <- e %>% 
  select(c(site_type, species, abundance)) #keep columns you want, any columns you dont say, go away
  select (-c(site_code)) #removing columns you dont want, c adds the list, minus before the c, or you add the minus each time u write it
  
#N/A vs 0
  #N/A: we couldnt get there, never could get there, impossible to get, didnt even try, couldnt measure
  #0:you did it but they had 0 stars or you attempted and didnt find anything

#when you average then, you can filter out the N/A 
  mean(diameter, na.rm=T)
  
#ex. create average to diameter to groups in dataframe (ex. by each site) 
#goal, create diameter per site plot 
  
s <- seastardata
#region (factor: Regions A, Region B)
#site_code (factor)
#site_name (factor: S1, S2, S3, S4, S5)
#abundance (numeric)
#diamater (numeric)
#substrate (factor)

s_group <- s %>%
  dplyr::select(-c(abundance, substrate, site_code)) %>% #removes columns listed here
  group_by(region, site_name) %>% #group values by region, then by site name
  summarise(diameter_mean = mean(diameter, na.rm=T), #tell r what to do with the diameter 
            diameter_sd = sd(diameter, na.rm=T)) #do this first to then get ur standard error and confidence intervals
#output columns (dataframe): region, site_name, diameter_mean, diameter_sd

###### Lollipop plot ######


## 
theme()
#lets you modify any visual aspects u want to change like titles and layout of titles and yeah
#change title sizes and fonts and shapes and size 

#geom_jitter() or geom_point will emphasize raw values
#geom_jitter adds raw data points 

#####in glmm 
# link = 
##know what this is aka look into it 





##### TO DO LIST / ORDER ###########

#clean each CSV file seperately (make sure naming is the same and everything, column named same, levels same, NA and 0's are accurate)  
#in each CSV file: create column "Time" for present and historic
#merge 
#grouping by time, then site, make sure its site within the time as u group 
#group and means and take out columsn and add sd and 
#then have condesensed version with means and then i plot it 

  