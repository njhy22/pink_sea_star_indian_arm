#Project: pink sea star
# maps
#Mar 13
#Author: nicole yang

#Loading packaged 
library(tidyverse)
library(sf)
library(ggspatial)
## --------------- Example map code for site locations
## --------------- Author: Claire Attridge
## --------------- Origin date: Feb 2025


# Loading packages
library(tidyverse)
library(MetBrewer)
library(sf)
library(ggspatial)

#

#### Initial work on the base map ----


### Step 1) Setting map projections


# Setting my geom and proj systems
proj <- st_crs(3005) # ESPG code for BC Albers projection
latlon <- st_crs(4326) # for baseline WGS84 geometry



### Step 2) Loading the basemap shapefile and projecting coordinate system

# Loading the Hakai BC shapefile
# Setting the existing crs as WGS84
# Reprojecting the geometry into BC/Albers (NAD83)
land <- sf::st_read("./Hakai_polygon_BC/COAST_TEST2.shp") %>%
  st_as_sf(crs=latlon) %>% 
  st_transform(proj)


## Cropping down the full BC map to Indian Arm

# Setting bounds to a wide extent of Indian Arm
ymax <- 49.50
ymin <- 49.22
xmax <- -123.06
xmin <- -122.80

# Making corners for the extent
corners <- st_multipoint(matrix(c(xmax,xmin,ymax,ymin),ncol=2)) %>% 
  st_sfc(crs=latlon) %>%
  st_sf() %>%
  st_transform(proj) 
plot(corners)

# Cropping Hakai map to the extent
land_indianarm <- land %>%
  st_crop(st_bbox(corners))
plot(land_indianarm)

## saving the cropped map as a geopackage
write_sf(land_indianarm, "./manipulated_map/indianarm_map.gpkg", overwrite=T)


#


#### Loading the site locations ----

## RLS sites
PS_survey_sites <- read_csv("./PS_Site_Locations.csv") %>%
  as.data.frame() %>%
  mutate(Site = as.factor(Site)) %>%
  dplyr::select(-Project)

str(PS_survey_sites)
#

#### Plotting maps of site distribution ----

# Loading the cropped land map
land_indianarm <- sf::st_read("./manipulated_map/indianarm_map.shp")


## Making the frame into an sf object
# Setting the existing crs as WGS84 (bc. the original geometry is recorded as lat/lon)
# Reprojecting the geometry into BC/Albers (NAD83)
sites_sf <- st_as_sf(PS_survey_sites, coords = c("Longitude", "Latitude"),
                     crs=latlon) %>%
  st_transform(proj)

plot(sites_sf)

## Plotting the Indian Arm sites
## Saving as a .tiff file

tiff(file="./figures/IA_map.tiff", height = 7, width = 4.5, units = "in", res=400) #saving a file type

ggplot(land_indianarm)+
  geom_sf(fill = "grey65", color = NA) + #land quality 
  theme_minimal(base_size = 16) + 
  geom_sf(data = sites_sf, shape=19, size=1.5, color="green") + # site dot 
  geom_sf_text(mapping=aes(), data = sites_sf, size=1.5, check_overlap=T, nudge_y = 100, #nudge to move text around in x or y axis
               label = sites_sf$Site, stat="sf_coordinates", inherit.aes=T) + #site text
  scale_x_continuous(expand = c(0,0)) + #removes white border around box of map
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_line(colour = "transparent"), #removes grid line
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(color="black", size=10),
        panel.border = element_rect(colour="black", fill=NA)) +
  annotation_scale(location = "br", style="ticks", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(0.4, "in"), width = unit(0.4, "in"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) 



dev.off()

#map of BC

proj <- st_crs(3005) # ESPG code for BC Albers projection
latlon <- st_crs(4326) # for baseline WGS84 geometry



### Step 2) Loading the basemap shapefile and projecting coordinate system

# Loading the Hakai BC shapefile
# Setting the existing crs as WGS84
# Reprojecting the geometry into BC/Albers (NAD83)
land <- sf::st_read("./Hakai_polygon_BC/COAST_TEST2.shp") %>%
  st_as_sf(crs=latlon) %>% 
  st_transform(proj)


## Cropping down the full BC map to Indian Arm

# Setting bounds to a wide extent of Indian Arm
ymax <- 60
ymin <- 49
xmax <- -112
xmin <- -135

# Making corners for the extent
corners_bc <- st_multipoint(matrix(c(xmax,xmin,ymax,ymin),ncol=2)) %>% 
  st_sfc(crs=latlon) %>%
  st_sf() %>%
  st_transform(proj) 
plot(corners_bc)

# Cropping Hakai map to the extent
land_bc <- land %>%
  st_crop(st_bbox(corners))
plot(land_bc)

## saving the cropped map as a geopackage
write_sf(land_bc, "./manipulated_map/land_bc.gpkg", overwrite=T)



