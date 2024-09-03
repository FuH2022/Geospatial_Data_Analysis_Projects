#project 04 
library(sf)
library(sp)
library(tmap)
library(spatstat)
library(raster)
library(tidyverse)
library(dplyr)

#2spatial point pattern exploration

#2.1 read the data and display
chicago <- st_read("./Chicago Data/supermarkets/chicago boundary/ChicagoBoundary.shp")

hospital <-st_read("./Chicago Data/hospitals/Hospitals.shp")
supermarket <- st_read("./Chicago Data/supermarkets/chicago_sup.shp")
supermarket <- supermarket[!st_is_empty(supermarket), ]
railstation <- st_read ("./CTA_RailStations/CTA_RailStations.shp")
#ParknRide <- st_read("./CTA_ParknRide/CTA_ParknRide.shp")parknride
#above are shape files

#bottom are csv files
park <- read.csv("./Chicago Data/Chicago_Parks.csv", header = TRUE)

#adjust the proper base map
parks <- st_as_sf(x = park, coords = c("lng", "lat"), crs = 4236)

#other data
grocery_stores <- read.csv("./Chicago Data/Grocery_Stores_-_2013.csv", header = TRUE)
grocery <- st_as_sf(x = grocery_stores, coords = c("LONGITUDE", "LATITUDE"), crs = 4236)

#Food Inspection
food_inspection <- read.csv("./Chicago Data/Food_Inspections.csv", header = TRUE)

#chose the restaurant and the passed only 
food_inspection <- food_inspection %>%filter(Facility.Type == "Restaurant", Results == "Pass")

#clear missing data
food_inspection <- food_inspection[complete.cases(food_inspection$Longitude, food_inspection$Latitude), ]
resturant <- st_as_sf(x = food_inspection, coords = c("Longitude", "Latitude"), crs = 4236)

#clinics
Chicago_clinics <- read.csv("./Chicago Data/Chicago_Department_of_Public_Health_Clinic_Locations.csv", header = TRUE)
clinics <- st_as_sf(x = Chicago_clinics, coords = c("Longitude", "Latitude"), crs = 4236)

#police station
police_station <- read.csv("./Chicago Data/Police_Stations.csv", header = TRUE)
police <- st_as_sf(x = police_station, coords = c("LONGITUDE", "LATITUDE"), crs = 4236)




#Transform to NAD83 /UTM 16N
chicago <- st_transform(chicago, crs = "EPSG:26916")
hospital <- st_transform(hospital, crs = "EPSG:26916")
supermarket <- st_transform(supermarket, "EPSG:26916")
parks <- st_transform(parks, crs = "EPSG:26916")
grocery <- st_transform(grocery, crs = "EPSG:26916")
resturant <- st_transform(resturant, crs = "EPSG:26916")
clinics <- st_transform(clinics, crs = "EPSG:26916")
police <- st_transform(police, crs = "EPSG:26916")
railstation <- st_transform(railstation, crs = "EPSG:26916")
#ParknRide <- st_transform(ParknRide, crs = "EPSG:26916")



         
# Creating the map
tmap_mode("plot")

#parks
tm_shape(chicago) +
tm_polygons() +
  tm_shape(parks)+
  tm_dots(size = 0.1, col = "green") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
  tm_layout(panel.labels = "Chicago Parks",
            legend.outside  = TRUE)  
 
#hospital
tm_shape(chicago) +
  tm_polygons() +
  tm_shape(hospital)+
  tm_dots(size = 0.1, col = "red") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
  tm_layout(panel.labels = "Chicago Hospitals",
            legend.outside  = TRUE) 

#supermarket
tm_shape(chicago) +
  tm_polygons() +
  tm_shape(supermarket)+
  tm_dots(size = 0.1, col = "blue") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
  tm_layout(panel.labels = "Chicago Supermarkets",
            legend.outside  = TRUE) 

#grocery
tm_shape(chicago) +
  tm_polygons() +
  tm_shape(grocery)+
  tm_dots(size = 0.1, col = "lightblue") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
  tm_layout(panel.labels = "Chicago Supermarkets",
            legend.outside  = TRUE) 

#resturant
tm_shape(chicago) +
   tm_polygons() +
   tm_shape(resturant)+
   tm_dots(size = 0.05, col = "yellow") +
   tm_compass(position = c("right", "top")) +
   tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
   tm_layout(panel.labels = "Chicago Resturant",
             legend.outside  = TRUE)  

#clinics
tm_shape(chicago) +
  tm_polygons() +
  tm_shape(clinics)+
  tm_dots(size = 0.1, col = "pink") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
  tm_layout(panel.labels = "Chicago Clinics",
            legend.outside  = TRUE)  


#police
tm_shape(chicago) +
  tm_polygons() +
  tm_shape(police)+
  tm_dots(size = 0.1, col = "purple") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
  tm_layout(panel.labels = "Chicago Police Station",
            legend.outside  = TRUE)  

#railstation
tm_shape(chicago) +
  tm_polygons() +
  tm_shape(railstation)+
  tm_dots(size = 0.05, col = "salmon") +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
  tm_layout(panel.labels = "Chicago CTA Rail Stations",
            legend.outside  = TRUE)  

#parknride
# tm_shape(chicago) +
#   tm_polygons() +
#   tm_shape(ParknRide)+
#   tm_dots(size = 0.05, col = "lightblue") +
#   tm_scale_bar(position = c("left", "bottom"), text.size = 1) +
#   tm_layout(title = "Chicago Park n Ride",
#             title.position  = TRUE)  





#density map visualization
#convert chicago sf to sp and make a raster
chicago_sp <- sf::as_Spatial(chicago)
chicago_r <- raster(chicago_sp)
#set the cell size 
res(chicago_r) <- 3000

#this is just the test from the tutorial
chicago_raster <- rasterize(chicago, chicago_r)
chicago_quads <- as(chicago_raster, "SpatialPolygons")
plot(chicago_raster, main = "Chicago Park Grids")
plot(chicago_quads, add=TRUE)
parks_sp <- sf::as_Spatial(parks)
points(parks_sp, col='red', cex=.5)

#park density
park_density <- rasterize(coordinates(parks_sp),
                          chicago_r, fun='count', background=0)
park_density <- mask(park_density, chicago_sp)
plot(park_density, main = "Chicago Park Density")
plot(chicago_quads, add=TRUE)


#convert all other POI to sp
hospital_sp <- sf::as_Spatial(hospital)
grocery_sp <- sf::as_Spatial(grocery)
clinics_sp <- sf::as_Spatial(clinics)
resturant_sp <- sf::as_Spatial(resturant)
police_sp <- sf::as_Spatial(police)
railstation_sp <- sf::as_Spatial(railstation)

#hospital density
hospital_density <- rasterize(coordinates(hospital_sp),
                          chicago_r, fun='count', background=0)
hospital_density <- mask(hospital_density, chicago_sp)
plot(hospital_density, main = "Chicago Hospital Density")
plot(chicago_quads, add=TRUE)


#clinics density
clinics_density <- rasterize(coordinates(clinics_sp),
                              chicago_r, fun='count', background=0)
clinics_density <- mask(clinics_density, chicago_sp)
plot(clinics_density, main = "Chicago Clinics Density")
plot(chicago_quads, add=TRUE)


#grocery
grocery_density <- rasterize(coordinates(grocery_sp),
                             chicago_r, fun='count', background=0)
grocery_density <- mask(grocery_density, chicago_sp)
plot(grocery_density, main = "Chicago Grocery Density")
plot(chicago_quads, add=TRUE)

#resturant
resturant_density <- rasterize(coordinates(resturant_sp),
                             chicago_r, fun='count', background=0)
resturant_density <- mask(resturant_density, chicago_sp)
plot(resturant_density, main = "Chicago Resturant Density")
plot(chicago_quads, add=TRUE)

#police
police_density <- rasterize(coordinates(police_sp),
                               chicago_r, fun='count', background=0)
police_density <- mask(police_density, chicago_sp)
plot(police_density, main = "Chicago Police Station Density")
plot(chicago_quads, add=TRUE)

#CTA
railstation_density <- rasterize(coordinates(railstation_sp),
                            chicago_r, fun='count', background=0)
railstation_density <- mask(railstation_density, chicago_sp)
plot(railstation_density, main = "Chicago CTA Rail Station Density")
plot(chicago_quads, add=TRUE)



#supermarket 
supermarket_sp <- sf::as_Spatial(supermarket)
#supermarket density
supermarket_density <- rasterize(coordinates(supermarket_sp),
                                 chicago_r, fun='count', background=0)
supermarket_density <- mask(supermarket_density, chicago_sp)
plot(supermarket_density, main = "Chicago Supermarket Density")
plot(chicago_quads, add=TRUE)


#quadrant test

eastnorth <- st_coordinates(parks)
parks$Easting <- eastnorth[,1]
parks$Northing <- eastnorth[,2]
#bbox
bbx <- bbox(chicago_sp)
xmin <- bbx[1, 1]
xmax <- bbx[1, 2]
ymin <- bbx[2, 1]
ymax <- bbx[2, 2]
park_ppp <- ppp(parks$Easting, parks$Northing, owin(c(xmin, xmax), c(ymin, ymax)))

#park
plot(park_ppp, main = "Chicago Park Grids")
#visualize the result
quad_test <- quadrat.test(park_ppp, nx = 3, ny = 3)
plot(quad_test)

quad_test


#clinics
eastnorth <- st_coordinates(clinics)
clinics$Easting <- eastnorth[,1]
clinics$Northing <- eastnorth[,2]
#bbox
bbx <- bbox(chicago_sp)
xmin <- bbx[1, 1]
xmax <- bbx[1, 2]
ymin <- bbx[2, 1]
ymax <- bbx[2, 2]
clinics_ppp <- ppp(clinics$Easting, clinics$Northing, owin(c(xmin, xmax), c(ymin, ymax)))

#
plot(clinics_ppp, main = "Chicago Clinics Grids")
#visualize the result
quad_test <- quadrat.test(clinics_ppp, nx = 3, ny = 3)
plot(quad_test)

quad_test

#hospital
eastnorth <- st_coordinates(hospital)
hospital$Easting <- eastnorth[,1]
hospital$Northing <- eastnorth[,2]
#bbox
bbx <- bbox(chicago_sp)
xmin <- bbx[1, 1]
xmax <- bbx[1, 2]
ymin <- bbx[2, 1]
ymax <- bbx[2, 2]
hospital_ppp <- ppp(hospital$Easting, hospital$Northing, owin(c(xmin, xmax), c(ymin, ymax)))

#
plot(hospital_ppp, main = "Chicago Hospital Grids")
#visualize the result
quad_test <- quadrat.test(hospital_ppp, nx = 3, ny = 3)
plot(quad_test)

quad_test

#grocery
eastnorth <- st_coordinates(grocery)
grocery$Easting <- eastnorth[,1]
grocery$Northing <- eastnorth[,2]
#bbox
bbx <- bbox(chicago_sp)
xmin <- bbx[1, 1]
xmax <- bbx[1, 2]
ymin <- bbx[2, 1]
ymax <- bbx[2, 2]
grocery_ppp <- ppp(grocery$Easting, grocery$Northing, owin(c(xmin, xmax), c(ymin, ymax)))

#
plot(grocery_ppp, main = "Chicago Grocery Grids")
#visualize the result
quad_test <- quadrat.test(grocery_ppp, nx = 3, ny = 3)
plot(quad_test)

quad_test



#supermarket
eastnorth <- st_coordinates(supermarket)
supermarket$Easting <- eastnorth[,1]
supermarket$Northing <- eastnorth[,2]
#bbox
bbx <- bbox(chicago_sp)
xmin <- bbx[1, 1]
xmax <- bbx[1, 2]
ymin <- bbx[2, 1]
ymax <- bbx[2, 2]
supermarket_ppp <- ppp(supermarket$Easting, supermarket$Northing, owin(c(xmin, xmax), c(ymin, ymax)))

#
plot(supermarket_ppp, main = "Chicago Supermarket Grids")
#visualize the result
quad_test <- quadrat.test(supermarket_ppp, nx = 3, ny = 3)
plot(quad_test)

quad_test


#resturant
eastnorth <- st_coordinates(resturant)
resturant$Easting <- eastnorth[,1]
resturant$Northing <- eastnorth[,2]
#bbox
bbx <- bbox(chicago_sp)
xmin <- bbx[1, 1]
xmax <- bbx[1, 2]
ymin <- bbx[2, 1]
ymax <- bbx[2, 2]
resturant_ppp <- ppp(resturant$Easting, resturant$Northing, owin(c(xmin, xmax), c(ymin, ymax)))

#
plot(resturant_ppp, main = "Chicago Resturant Grids")
#visualize the result
quad_test <- quadrat.test(resturant_ppp, nx = 3, ny = 3)
plot(quad_test)

quad_test

#police
eastnorth <- st_coordinates(police)
police$Easting <- eastnorth[,1]
police$Northing <- eastnorth[,2]
#bbox
bbx <- bbox(chicago_sp)
xmin <- bbx[1, 1]
xmax <- bbx[1, 2]
ymin <- bbx[2, 1]
ymax <- bbx[2, 2]
police_ppp <- ppp(police$Easting, police$Northing, owin(c(xmin, xmax), c(ymin, ymax)))

#
plot(police_ppp, main = "Chicago Police Grids")
#visualize the result
quad_test <- quadrat.test(police_ppp, nx = 3, ny = 3)
plot(quad_test)

quad_test



#Kernel density
#set chicago spatiallines dataframe
chicago_bnd_sp <- as(chicago_sp, 'SpatialLinesDataFrame')

#park KD
park_KD <- density(park_ppp, sigma = 1500)
plot(park_KD, main = "Chicago Park Kernel Density, Sigma = 1500 m", las = 1)
plot(chicago_bnd_sp, col = "white", add=TRUE)
contour(park_KD, add=TRUE)


#restaurant KD
resturant_KD <- density(resturant_ppp, sigma = 1500)
plot(resturant_KD, main = "Chicago Resturant Kernel Density, Sigma = 1500 m", las = 1)
plot(chicago_bnd_sp, col = "white", add=TRUE)
contour(resturant_KD, add=TRUE)

#supermarket KD
supermarket_KD <- density(supermarket_ppp, sigma = 1500)
plot(supermarket_KD, main = "Chicago Supermarket Kernel Density, Sigma = 1500 m", las = 1)
plot(chicago_bnd_sp, col = "white", add=TRUE)
contour(supermarket_KD, add=TRUE)

#grocery KD
grocery_KD <- density(grocery_ppp, sigma = 1500)
plot(grocery_KD, main = "Chicago Grocery Kernel Density, Sigma = 1500 m", las = 1)
plot(chicago_bnd_sp, col = "white", add=TRUE)
contour(grocery_KD, add=TRUE)

#hospital KD
hospital_KD <- density(hospital_ppp, sigma = 1500)
plot(hospital_KD, main = "Chicago Hospital Kernel Density, Sigma = 1500 m", las = 1)
plot(chicago_bnd_sp, col = "white", add=TRUE)
contour(hospital_KD, add=TRUE)

#clinics KD
clinics_KD <- density(clinics_ppp, sigma = 1500)
plot(clinics_KD, main = "Chicago Clinics Kernel Density, Sigma = 1500 m", las = 1)
plot(chicago_bnd_sp, col = "white", add=TRUE)
contour(clinics_KD, add=TRUE)

#police KD
police_KD <- density(police_ppp, sigma = 1500)
plot(police_KD, main = "Chicago Police Kernel Density, Sigma = 1500 m", las = 1)
plot(chicago_bnd_sp, col = "white", add=TRUE)
contour(police_KD, add=TRUE)

#K functions
#park
K_func <- Kest(park_ppp)
K_enve <- envelope(park_ppp, Kest)

#plot
plot(K_func, main = "K Function Park")

#plot with envelope
plot(K_enve, main = "K Function with Envelope Park")

#resturant
K_func <- Kest(resturant_ppp)
K_enve <- envelope(resturant_ppp, Kest)
#only 3000 
#plot
plot(K_func, main = "K Function Resturant")

#plot with envelope
plot(K_enve, main = "K Function with Envelope Resturant")


#grocery
K_func <- Kest(grocery_ppp)
K_enve <- envelope(grocery_ppp, Kest)

#plot
plot(K_func, main = "K Function Grocery")

#plot with envelope
plot(K_enve, main = "K Function with Envelope Grocery")

#supermarket
K_func <- Kest(supermarket_ppp)
K_enve <- envelope(supermarket_ppp, Kest)

#plot
plot(K_func, main = "K Function Supermarket")

#plot with envelope
plot(K_enve, main = "K Function with Envelope Supermarket")


#hospital
K_func <- Kest(hospital_ppp)
K_enve <- envelope(hospital_ppp, Kest)

#plot
plot(K_func, main = "K Function Hospital")

#plot with envelope
plot(K_enve, main = "K Function with Envelope Hospital")

#clinics
K_func <- Kest(clinics_ppp)
K_enve <- envelope(clinics_ppp, Kest)

#plot
plot(K_func, main = "K Function Clinics")

#plot with envelope
plot(K_enve, main = "K Function with Envelope Clinics")


#police
K_func <- Kest(police_ppp)
K_enve <- envelope(police_ppp, Kest)

#plot
plot(K_func, main = "K Function Police")

#plot with envelope
plot(K_enve, main = "K Function with Envelope Police")



#population and blocks
#get the illinoi blocks
IL_block <- st_read("./Chicago Data/tl_2010_17_tabblock10/tl_2010_17_tabblock10.shp")
#GEOID10 block id


#get the block population
#bottom are csv files
population <- read.csv("./Chicago Data/Population_by_2010_Census_Block.csv", header = TRUE)
#CENSUS.BLOCK.FULL is GEOID10

IL_block$GEOID10 <- as.character(IL_block$GEOID10)
population$CENSUS.BLOCK.FULL <- as.character(population$CENSUS.BLOCK.FULL)

#add a column with population based on the geoid
IL_block_pop <- IL_block %>%
  left_join(population, by = c("GEOID10" = "CENSUS.BLOCK.FULL"))

# Remove rows where TOTAL.POPULATION is NA
IL_block_pop_clean <- IL_block_pop %>%
  filter(!is.na(TOTAL.POPULATION))

IL_block_pop_clean <- IL_block_pop_clean %>%
  rename(Block_Population = TOTAL.POPULATION
  )
#do it in NAD 83 
IL_block_pop_clean <-st_transform(IL_block_pop_clean, crs = "EPSG:26916")

tm_shape(IL_block_pop_clean) + 
  tm_polygons(col = "Block_Population", palette = "-viridis",
              title = "Population per Block",
              border.col = "NA") +  # remove borders to avoid clutter
  tm_layout(main.title = "Population Density in Chicago Blocks",
            main.title.size = 1.5,
            legend.outside = TRUE, 
            legend.outside.size = 0.25, 
            legend.outside.position = "bottom") +
  tm_compass(position = c("right", "top"), size = 1.5) +
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20), 
               position = c("left", "bottom"), size = 1) 



# Add a new variable that is the log of the block population
IL_block_pop_clean$log_Block_Population <- log1p(IL_block_pop_clean$Block_Population)


# Plotting
tm_shape(IL_block_pop_clean) + 
  tm_polygons(col = "log_Block_Population", palette = "-viridis",
              title = "Log(Population) per Block",
              border.col = "NA") +  # removing borders to avoid clutter
  tm_layout(main.title = "Population Density Log Chicago",
            main.title.size = 1,
            legend.outside = TRUE, 
            legend.outside.size = 0.25, 
            legend.outside.position = "bottom") +
  tm_compass(position = c("right", "top"), size = 1.5) +
  tm_scale_bar( 
               position = c("left", "bottom"), size = 1) 


# Creating a single map police population
police_population <- tm_shape(IL_block_pop_clean) + 
  tm_polygons(col = "log_Block_Population", palette = "-viridis",
              title = "Log(Population) per Block", border.col = "NA") +  
  tm_shape(police) +
  tm_dots(size = 0.5, col = "purple") +
  tm_layout(main.title = "Police Stations & Population in Chicago",
            main.title.size = 1,
            legend.title.size = 1,
            legend.outside = TRUE, 
            legend.outside.size = 0.25, 
            legend.outside.position = "bottom") +
  tm_compass(position = c("right", "top"), size = 1.5) +
  tm_scale_bar(position = c("left", "bottom"), size = 1)


police_population


#railway and population density
railway_population <- tm_shape(IL_block_pop_clean) + 
  tm_polygons(col = "log_Block_Population", palette = "-viridis",
              title = "Log(Population) per Block", border.col = "NA") +  
  tm_shape(railstation) +
  tm_dots(size = 0.2, col = "salmon") +
  tm_layout(main.title = "CTI Stations & Population in Chicago",
            main.title.size = 1,
            legend.title.size = 1,
            legend.outside = TRUE, 
            legend.outside.size = 0.25, 
            legend.outside.position = "bottom") +
  tm_compass(position = c("right", "top"), size = 1.5) +
  tm_scale_bar(position = c("left", "bottom"), size = 1)

railway_population


#supermarket and population density
supermarket_population <- tm_shape(IL_block_pop_clean) + 
  tm_polygons(col = "log_Block_Population", palette = "-viridis",
              title = "Log(Population) per Block", border.col = "NA") +  
  tm_shape(supermarket) +
  tm_dots(size = 0.2, col = "blue") +
  tm_layout(main.title = "Supermarket & Population in Chicago",
            main.title.size = 1,
            legend.title.size = 1,
            legend.outside = TRUE, 
            legend.outside.size = 0.25, 
            legend.outside.position = "bottom") +
  tm_compass(position = c("right", "top"), size = 1.5) +
  tm_scale_bar(position = c("left", "bottom"), size = 1)

supermarket_population




#police k self function

#check the head of police 
print(head(police))
#rename it
police <- police %>% rename(
  x = X.COORDINATE,
  y = Y.COORDINATE
)


x_police = police$x
y_police = police$y
df_police = data.frame(x_police, y_police)

#set up a fake key
df_police <- df_police %>% mutate (k = 1)

#perform the join, remove the key, then create the distance
df_police = df_police %>%
  full_join(df_police, by = "k") %>%
  mutate(dist = sqrt((x_police.x - x_police.y)^2 + (y_police.x - y_police.y)^2))

#Euclidean calculation
dis_police = as.matrix(df_police$dist)


m_police = matrix(0, nrow = nrow(df_police), ncol = nrow(df_police))
for(i in 1:nrow(m_police)){
  for(j in 1:nrow(m_police)){
    m_police[i, j] = dis_police[(i-1)*nrow(m_police) + j]
  }
}
# Ensure the matrix looks as expected
print(head(m_police))


#k function calculation
distance_police <- seq(1, 9000, 300)
kd_police = sapply(distance_police, function(x) sum(dis_police < x))
cityarea = st_area(chicago) # Ensure the city area is correct
den_police = nrow(df_police) / cityarea # Ensure density is calculated as expected
kd_police = kd_police / (length(kd_police) * den_police)

print(head(cbind(distance_police, kd_police)))

plot(distance_police, kd_police, type = 'l', lwd = 2)

hist(df_police$dist, breaks = 50, las = 1,
     xlab = "distance",
     ylab = "number of event",
     main = "historgram",
     col = "purple",
     xlim = c(0, 80000),
     ylim = c(0, 50)
)

#Hospital

# Check the head of hospitals 
print(head(hospital))

# No need to rename if 'X' and 'Y' are already your coordinate columns.
x_hospital = hospital$X
y_hospital = hospital$Y
df_hospital = data.frame(x_hospital, y_hospital)

# Set up a fake key
df_hospital <- df_hospital %>% mutate (k = 1)

# Perform the join, remove the key, then create the distance
df_hospital = df_hospital %>%
  full_join(df_hospital, by = "k") %>%
  mutate(dist = sqrt((x_hospital.x - x_hospital.y)^2 + (y_hospital.x - y_hospital.y)^2))

# Checkpoints: Ensure the distances are computed and stored
print(head(df_hospital))

dis_hospital = as.matrix(df_hospital$dist)

# Initialize a matrix to store distances and fill it 
m_hospital = matrix(0, nrow = nrow(df_hospital), ncol = nrow(df_hospital))
for(i in 1:nrow(m_hospital)){
  for(j in 1:nrow(m_hospital)){
    m_hospital[i, j] = dis_hospital[(i-1)*nrow(m_hospital) + j]
  }
}

# Ensure the matrix looks as expected
print(head(m_hospital))

# K function calculation
distance_hospital <- seq(1, 9000, 300)
kd_hospital = sapply(distance_hospital, function(x) sum(dis_hospital < x))
cityarea = st_area(chicago) # Ensure the city area is correct
den_hospital = nrow(df_hospital) / cityarea # Ensure density is calculated as expected
kd_hospital = kd_hospital / (length(kd_hospital) * den_hospital)

print(head(cbind(distance_hospital, kd_hospital)))

# Plotting K-function
plot(distance_hospital, kd_hospital, type = 'l', lwd = 2)

# Histogram of distances
hist(df_hospital$dist, breaks = 50, las = 1,
     xlab = "distance",
     ylab = "number of event",
     main = "Histogram",
     col = "skyblue",
     xlim = c(0, 90000),
     ylim = c(0, 100)
)



      