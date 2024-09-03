#project 06
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(tmap)
library(sf)
library(viridis)
library(cluster)
library(spdep)
library(gridExtra)





#1
# Read the files
indiana_county_sf <- st_read("./Census Data/Census_Counties/Census_County_TIGER00_IN.shp")
indiana_tract_sf <- st_read('./Census Data/Census_Tracts/Census_Tracts_TIGER00_IN.shp')

# Display Column names and pick the selection
colnames(indiana_county_sf)
colnames(indiana_tract_sf)

head (indiana_county_sf)

# POP2000, HISPANIC, MED_AGE

#2

# Calculate neighborhoods with Queen and Rook methods
queen_nb <- poly2nb(indiana_county_sf, queen = TRUE)
rook_nb <- poly2nb(indiana_county_sf, queen = FALSE)

# Convert neighborhood object to sf lines using the method you found
nb_queen_link <- as(nb2lines(queen_nb, coords = st_centroid(st_geometry(indiana_county_sf))), 'sf')
nb_queen_link <- st_set_crs(nb_queen_link, st_crs(indiana_county_sf))

nb_rook_link <- as(nb2lines(rook_nb, coords = st_centroid(st_geometry(indiana_county_sf))), 'sf')
nb_rook_link <- st_set_crs(nb_rook_link, st_crs(indiana_county_sf))

# Plot the map links
plotMapLinks <- function(sf_object, links, title) {
  tm_shape(sf_object) + 
    tm_borders() + 
    tm_shape(links) +  
    tm_lines(col = "blue", lwd = 1.5) + 
    tm_compass(type="4star", position=c("left", "top"), size=1.5) + # Add compass
    tm_scale_bar(position=c("right", "bottom"), size=1) + # Add scale bar
    tm_layout(title = title)
}

tmap_mode("plot")
plotMapLinks(indiana_county_sf, nb_queen_link, 'Queen-County-Links')
plotMapLinks(indiana_county_sf, nb_rook_link, 'Rook-County-Links')




#3 Weight Matrix
# Form three weight matrices 
weight_W_county <- nb2listw(neighbours = rook_nb, style = 'W')
weight_B_county <- nb2listw(neighbours = rook_nb, style = 'B')
weight_C_county <- nb2listw(neighbours = rook_nb, style = 'C')

summary(weight_W_county)
summary(weight_B_county)
summary(weight_C_county)


# For weight_W_county
W_moran_i <- moran(x = indiana_county_sf$POP2000, 
                   listw = weight_W_county, 
                   n = length(weight_W_county$neighbours), 
                   S0 = Szero(weight_W_county))

# For weight_B_county
B_moran_i <- moran(x = indiana_county_sf$POP2000, 
                   listw = weight_B_county, 
                   n = length(weight_B_county$neighbours), 
                   S0 = Szero(weight_B_county))

# For weight_C_county
C_moran_i <- moran(x = indiana_county_sf$POP2000, 
                   listw = weight_C_county, 
                   n = length(weight_C_county$neighbours), 
                   S0 = Szero(weight_C_county))

summary(W_moran_i)
summary(B_moran_i)
summary(C_moran_i)

# 4 Moran I Evaluation

moran.test(x = indiana_county_sf$POP2000, listw = weight_W_county, randomisation = TRUE)
moran.test(x = indiana_county_sf$POP2000, listw = weight_W_county, randomisation = FALSE)

moran.test(x = indiana_county_sf$HISPANIC, listw = weight_W_county, randomisation = TRUE)
moran.test(x = indiana_county_sf$HISPANIC, listw = weight_W_county, randomisation = FALSE)

moran.test(x = indiana_county_sf$MED_AGE, listw = weight_W_county, randomisation = TRUE)
moran.test(x = indiana_county_sf$MED_AGE, listw = weight_W_county, randomisation = FALSE)


# Monte Carlo
# Monte Carlo Implementation

moran_mc_pop2000 <- moran.mc(x = indiana_county_sf$POP2000, listw = weight_W_county, nsim = 50000)
moran_mc_hispanic <- moran.mc(x = indiana_county_sf$HISPANIC, listw = weight_W_county, nsim = 50000)
moran_mc_med_age <- moran.mc(x = indiana_county_sf$MED_AGE, listw = weight_W_county, nsim = 50000)

# Print and plot Monte Carlo results

print(moran_mc_pop2000)
print(moran_mc_hispanic)
print(moran_mc_med_age)

plot(moran_mc_pop2000, type='b', main="Monte Carlo for POP2000")
plot(moran_mc_hispanic, type='b', main="Monte Carlo for HISPANIC")
plot(moran_mc_med_age, type='b', main="Monte Carlo for MED_AGE")


# 5 Local Moran's I Evaluation
# Setting tmap mode to plot
tmap_mode('plot')

# Create a default function
create_default_plot = function(sf, attribute){
  default_plot <- tm_shape(shp = sf) +
    tm_polygons(col = attribute) + 
    tm_compass(position = c('left', 'top')) + 
    tm_scale_bar(text.size = 1, position = c('right', 'bottom')) + 
    tm_layout(
      frame = TRUE,
      legend.frame = 'black',
      legend.outside = TRUE,
      legend.outside.size = 0.5,
      outer.margins = c(0, 0, 0, 0),
      inner.margins = c(0.15, 0.15, 0.15, 0.15),
      legend.width = 0.8
    )
  
  return(default_plot)
}

# Use the default function
create_default_plot(indiana_county_sf, 'POP2000')
create_default_plot(indiana_county_sf, 'HISPANIC')
create_default_plot(indiana_county_sf, 'MED_AGE')





# 5-2Lagged Average
# Moran's plot for POP2000
moran.plot(indiana_county_sf$POP2000, listw = weight_W_county, main = "Moran's Plot for POP2000")

# Moran's plot for MED_AGE
moran.plot(indiana_county_sf$MED_AGE, listw = weight_W_county, main = "Moran's Plot for MED_AGE")

# Moran's plot for HISPANIC
moran.plot(indiana_county_sf$HISPANIC, listw = weight_W_county, main = "Moran's Plot for HISPANIC")




# 5-3 Evaluation LOCALMORAN
# Calculate local Moran I's for the three attributes
local_moran_i_pop2000 <- localmoran(x = indiana_county_sf$POP2000, listw = weight_W_county)
local_moran_i_hispanic <- localmoran(x = indiana_county_sf$HISPANIC, listw = weight_W_county)
local_moran_i_med_age <- localmoran(x = indiana_county_sf$MED_AGE, listw = weight_W_county)

# Normalize indicators
mean_norm_pop2000 <- indiana_county_sf$POP2000 - mean(indiana_county_sf$POP2000)
mean_norm_hispanic <- indiana_county_sf$HISPANIC - mean(indiana_county_sf$HISPANIC)
mean_norm_med_age <- indiana_county_sf$MED_AGE - mean(indiana_county_sf$MED_AGE)

mean_norm_moran_i_pop2000 <- local_moran_i_pop2000[,1] - mean(local_moran_i_pop2000[,1])
mean_norm_moran_i_hispanic <- local_moran_i_hispanic[,1] - mean(local_moran_i_hispanic[,1])
mean_norm_moran_i_med_age <- local_moran_i_med_age[,1] - mean(local_moran_i_med_age[,1])

# Quadrant
assignQuadrants <- function(local_moran_i, mean_norm_attribute) {
  quadrant <- rep(NA, length(mean_norm_attribute))
  
  # High-High
  quadrant[which(local_moran_i[,1] > 0 & mean_norm_attribute > 0)] <- 4 #"High-High"
  
  # Low-Low
  quadrant[which(local_moran_i[,1] < 0 & mean_norm_attribute < 0)] <- 1 #"Low-Low"
  
  # High-Low (outliers where the region value is high, but its neighbors are low)
  quadrant[which(local_moran_i[,1] > 0 & mean_norm_attribute < 0)] <- 2 #"High-Low"
  
  # Low-High (outliers where the region value is low, but its neighbors are high)
  quadrant[which(local_moran_i[,1] < 0 & mean_norm_attribute > 0)] <- 3 #"Low-High"
  
  quadrant[local_moran_i[, 5] >  .05] <- 0 #Insignificant
  return(quadrant)
}

# Assign quadrants
pop2000_quadrant <- assignQuadrants(local_moran_i_pop2000, mean_norm_pop2000)
hispanic_quadrant <- assignQuadrants(local_moran_i_hispanic, mean_norm_hispanic)
med_age_quadrant <- assignQuadrants(local_moran_i_med_age, mean_norm_med_age)


# Plot clusters
plotLocalMoranClusters <- function(sf_object, quadrant, title) {
  
  # Define breaks and colors
  brks <- c(0, 1, 2, 3, 4)
  colors <- c("white", "blue",
              rgb(0, 0, 1, alpha=0.4),
              rgb(1, 0, 0, alpha=0.4),
              "red")
  
  # Assign colors to clusters based on the quadrant
  LMI_map <- sf_object
  print(table(findInterval(quadrant, brks, all.inside=FALSE)))
  
  LMI_map$LMI <- colors[findInterval(quadrant, brks, all.inside=FALSE)]
  
  # Create the plot
  plot <- tm_shape(LMI_map) + 
    tm_fill('LMI', style = "pretty") + 
    tm_borders(alpha=0.4) + 
    tm_scale_bar(text.size = 0.5, position = c('right','bottom'), width = 0.2) + 
    tm_compass(position = c('left','top'), text.size = 0.5, size = 1.5) + 
    tm_layout(panel.labels = title, legend.outside = TRUE) + 
    tm_add_legend(labels = c("Insignificant", "Low-Low", "Low-High", "High-Low", "High-High"),
                  col = colors, border.lwd = 0.5, title = "Pattern")
  
  # Display the plot
  show(plot)
  
  
}

# Plot Local Moran's I clusters for different attributes
plotLocalMoranClusters(indiana_county_sf, pop2000_quadrant, 'POP2000')
plotLocalMoranClusters(indiana_county_sf, hispanic_quadrant, 'HISPANIC')
plotLocalMoranClusters(indiana_county_sf, med_age_quadrant, 'MED_AGE')





# 6 Pick a County: Tippecanoe: ID 157



# Filter for Tippecanoe County tracts based on the given COUNTY  code
tippecanoe_sf <- indiana_tract_sf[indiana_tract_sf$COUNTY == '157',]


# Calculate neighborhoods with Queen and Rook methods for Tippecanoe County
queen_nb_tippecanoe <- poly2nb(tippecanoe_sf, queen = TRUE)
rook_nb_tippecanoe <- poly2nb(tippecanoe_sf, queen = FALSE)

# Convert the neighborhood objects to sf lines 
nb_queen_link_tippecanoe <- as(nb2lines(queen_nb_tippecanoe, coords = st_centroid(st_geometry(tippecanoe_sf))), 'sf')
nb_queen_link_tippecanoe <- st_set_crs(nb_queen_link_tippecanoe, st_crs(tippecanoe_sf))

nb_rook_link_tippecanoe <- as(nb2lines(rook_nb_tippecanoe, coords = st_centroid(st_geometry(tippecanoe_sf))), 'sf')
nb_rook_link_tippecanoe <- st_set_crs(nb_rook_link_tippecanoe, st_crs(tippecanoe_sf))

# Plot the map links for Tippecanoe County
plotMapLinks(tippecanoe_sf, nb_queen_link_tippecanoe, 'Queen-Links for Tippecanoe County')
plotMapLinks(tippecanoe_sf, nb_rook_link_tippecanoe, 'Rook-Links for Tippecanoe County')


# 6 (3) County Tract Weight Matrix
# Form three weight matrices for Tippecanoe County
weight_W_tippecanoe <- nb2listw(neighbours = rook_nb_tippecanoe, style = 'W')
weight_B_tippecanoe <- nb2listw(neighbours = rook_nb_tippecanoe, style = 'B')
weight_C_tippecanoe <- nb2listw(neighbours = rook_nb_tippecanoe, style = 'C')

summary(weight_W_tippecanoe)
summary(weight_B_tippecanoe)
summary(weight_C_tippecanoe)


# For weight_W_tippecanoe
W_moran_i_tippecanoe <- moran(x = tippecanoe_sf$POP2000, 
                              listw = weight_W_tippecanoe, 
                              n = length(weight_W_tippecanoe$neighbours), 
                              S0 = Szero(weight_W_tippecanoe))

# For weight_B_tippecanoe
B_moran_i_tippecanoe <- moran(x = tippecanoe_sf$POP2000, 
                              listw = weight_B_tippecanoe, 
                              n = length(weight_B_tippecanoe$neighbours), 
                              S0 = Szero(weight_B_tippecanoe))

# For weight_C_tippecanoe
C_moran_i_tippecanoe <- moran(x = tippecanoe_sf$POP2000, 
                              listw = weight_C_tippecanoe, 
                              n = length(weight_C_tippecanoe$neighbours), 
                              S0 = Szero(weight_C_tippecanoe))

summary(W_moran_i_tippecanoe)
summary(B_moran_i_tippecanoe)
summary(C_moran_i_tippecanoe)


# 6-4 Moran I Evaluation for Tippecanoe County Tracts

# For POP2000
moran.test(x = tippecanoe_sf$POP2000, listw = weight_W_tippecanoe, randomisation = TRUE)
moran.test(x = tippecanoe_sf$POP2000, listw = weight_W_tippecanoe, randomisation = FALSE)

# For HISPANIC
moran.test(x = tippecanoe_sf$HISPANIC, listw = weight_W_tippecanoe, randomisation = TRUE)
moran.test(x = tippecanoe_sf$HISPANIC, listw = weight_W_tippecanoe, randomisation = FALSE)

# For MED_AGE
moran.test(x = tippecanoe_sf$MED_AGE, listw = weight_W_tippecanoe, randomisation = TRUE)
moran.test(x = tippecanoe_sf$MED_AGE, listw = weight_W_tippecanoe, randomisation = FALSE)

# Monte Carlo Implementation for Tippecanoe County Tracts

moran_mc_pop2000_tippecanoe <- moran.mc(x = tippecanoe_sf$POP2000, listw = weight_W_tippecanoe, nsim = 50000)
moran_mc_hispanic_tippecanoe <- moran.mc(x = tippecanoe_sf$HISPANIC, listw = weight_W_tippecanoe, nsim = 50000)
moran_mc_med_age_tippecanoe <- moran.mc(x = tippecanoe_sf$MED_AGE, listw = weight_W_tippecanoe, nsim = 50000)

# Print and plot Monte Carlo results for Tippecanoe County Tracts

print(moran_mc_pop2000_tippecanoe)
print(moran_mc_hispanic_tippecanoe)
print(moran_mc_med_age_tippecanoe)

plot(moran_mc_pop2000_tippecanoe, type='b', main="Monte Carlo for POP2000 in Tippecanoe County Tracts")
plot(moran_mc_hispanic_tippecanoe, type='b', main="Monte Carlo for HISPANIC in Tippecanoe County Tracts")
plot(moran_mc_med_age_tippecanoe, type='b', main="Monte Carlo for MED_AGE in Tippecanoe County Tracts")


# 6 5 Local Moran's I Evaluation for Tippecanoe County Tracts

# Use the default function for Tippecanoe County Tracts
create_default_plot(tippecanoe_sf, 'POP2000')
create_default_plot(tippecanoe_sf, 'HISPANIC')
create_default_plot(tippecanoe_sf, 'MED_AGE')


# 6 5-2 Lagged Average for Tippecanoe County Tracts

# Moran's plot for POP2000 in Tippecanoe
moran.plot(tippecanoe_sf$POP2000, listw = weight_W_tippecanoe, main = "Moran's Plot for POP2000 in Tippecanoe")

# Moran's plot for MED_AGE in Tippecanoe
moran.plot(tippecanoe_sf$MED_AGE, listw = weight_W_tippecanoe, main = "Moran's Plot for MED_AGE in Tippecanoe")

# Moran's plot for HISPANIC in Tippecanoe
moran.plot(tippecanoe_sf$HISPANIC, listw = weight_W_tippecanoe, main = "Moran's Plot for HISPANIC in Tippecanoe")


# 6 5-3 Evaluation LOCALMORAN for Tippecanoe
# Calculate local Moran I's for the three attributes
local_moran_i_pop2000 <- localmoran(x = tippecanoe_sf$POP2000, listw = weight_W_tippecanoe)
local_moran_i_hispanic <- localmoran(x = tippecanoe_sf$HISPANIC, listw = weight_W_tippecanoe)
local_moran_i_med_age <- localmoran(x = tippecanoe_sf$MED_AGE, listw = weight_W_tippecanoe)

# Normalize indicators
mean_norm_pop2000 <- tippecanoe_sf$POP2000 - mean(tippecanoe_sf$POP2000)
mean_norm_hispanic <- tippecanoe_sf$HISPANIC - mean(tippecanoe_sf$HISPANIC)
mean_norm_med_age <- tippecanoe_sf$MED_AGE - mean(tippecanoe_sf$MED_AGE)

mean_norm_moran_i_pop2000 <- local_moran_i_pop2000[,1] - mean(local_moran_i_pop2000[,1])
mean_norm_moran_i_hispanic <- local_moran_i_hispanic[,1] - mean(local_moran_i_hispanic[,1])
mean_norm_moran_i_med_age <- local_moran_i_med_age[,1] - mean(local_moran_i_med_age[,1])

# Assign quadrants
pop2000_quadrant <- assignQuadrants(local_moran_i_pop2000, mean_norm_pop2000)
hispanic_quadrant <- assignQuadrants(local_moran_i_hispanic, mean_norm_hispanic)
med_age_quadrant <- assignQuadrants(local_moran_i_med_age, mean_norm_med_age)

# Plot Local Moran's I clusters for different attributes in Tippecanoe
plotLocalMoranClusters(tippecanoe_sf, pop2000_quadrant, 'POP2000 in Tippecanoe')
plotLocalMoranClusters(tippecanoe_sf, hispanic_quadrant, 'HISPANIC in Tippecanoe')
plotLocalMoranClusters(tippecanoe_sf, med_age_quadrant, 'MED_AGE in Tippecanoe')

