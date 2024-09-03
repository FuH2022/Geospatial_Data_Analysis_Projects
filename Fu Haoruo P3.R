#project 03
library(tmap)
library(sf)
library(sp)
library(rgdal)
library(osmdata)
library(units)
library(tidyverse)
library(classInt)
library(dbplyr)
library(ggplot2)
library(gridExtra)



#1 Thematic Mapping with R
#the data used for this project will be 2010 data

#1.1.2 read the shape file in R
tract_in <- sf::st_read('./Tract_2010Census_DP1_IN/Tract_2010Census_DP1_IN/utm', layer='tract_in_selected_utm')
sf::st_crs(tract_in)

#1.1.3 check the CRS Project into WGS 84 UTM 16N if needed
tract_crs <- sf::st_transform(tract_in, crs=32616)

#1.1.4 calculate the size area of each census unit(tract or block, in km^2)
head (tract_in, 5)# find the column

tract_crs$blockarea <- sf::st_area(tract_crs) %>% set_units(km^2)
tract_crs
tract_crs <- tract_crs[,c("GEOID10", "NAMELSAD10", "ALAND10", "AWATER10",
                            "INTPTLAT10", "INTPTLON10", "DP0010001" , "blockarea")]
#1.1.5 create new sf file
class(tract_crs)

#1.2 map design population density

#1.2.1 get population density
tract_crs$density <- tract_crs$DP0010001/tract_crs$blockarea
range(tract_crs$density)
#do a log density
tract_crs$density_log <- log1p(tract_crs$density)
range(tract_crs$density_log)

#write to file
sf::st_write(tract_crs, dsn='.', layer='projectedTracts.shp',
             driver='ESRI Shapefile', delete_layer=TRUE)

#1.2.2do the map
tmap_mode("plot")


tm_shape(tract_crs) +
  tm_borders(lwd = 2) +
  tm_fill(col = "density",  #use the eiter dentisty or the density log for calculation
          palette = "-RdYlBu",
          title = "People per km^2 (log)") +
  tm_scale_bar(text.size = 0.5, position = c("left", "top")) + #scale bar
  tm_compass(type = "arrow", position = c("left", "top")) + #compass north point
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010\n Date:09/24/2023", #credit map maker
             position = c("right", "bottom"),
             size = 0.5, align = "right") +
  tm_layout(title = "Population Density IN 2010",
            title.size = 3,
            legend.outside = TRUE,
            title.position = c("center", "top")) 


#1.3 classification method evaluations

#equal interval
tm_shape(tract_crs) + 
  tm_fill(col = "density", 
          style = "equal", 
          palette = "-RdYlBu", 
          title = "Population Density \n(Equal Interval)\nLog Scale") +
  tm_borders(lwd = 1) +
  tm_scale_bar(text.size = 1, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "Density Map\nIN 2010",
            title.size = 4,
            legend.title.size = 2,
            legend.outside = TRUE)


#quantile
tm_shape(tract_crs) + 
  tm_fill(col = "density", 
          style = "quantile", 
          palette = "-RdYlBu", 
          title = "Population Density\n(Quantile)\n (log scale)") +
  tm_borders(lwd = 1) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "Density Map\nIN 2010", 
            title.size = 4, 
            legend.title.size = 2,
            legend.outside = TRUE)


#Standard Deviation
tm_shape(tract_crs) + 
  tm_fill(col = "density", 
          style = "sd", 
          palette = "-RdYlBu", 
          title = "Population Density\nStandard Deviation\n Log Scale") +
  tm_borders(lwd = 1) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "Density Map\nIN 2010", 
            title.size = 4, 
            legend.title.size = 2,
            legend.outside = TRUE)


#Jenks
tm_shape(tract_crs) + 
  tm_fill(col = "density", 
          style = "jenks", 
          palette = "-RdYlBu", 
          title = "Population Density\nJenks\n") +
  tm_borders(lwd = 1) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "Density Map \nIN 2010", 
            title.size = 4, 
            legend.title.size = 2,
            legend.outside = TRUE)


# print out break points
#equal interval
equal_breaks <- classIntervals(tract_crs$density, n=5, style="equal")$brks
print("Equal Interval Breaks:")
print(equal_breaks)

#quantile
quantile_breaks <- classIntervals(tract_crs$density, n=5, style="quantile")$brks
print("Quantile Breaks:")
print(quantile_breaks)

#Standard Deviation
sd_breaks <- classIntervals(tract_crs$density, n=5, style="sd")$brks
print("Standard Deviation Breaks:")
print(sd_breaks)

#Jenk
tract_crs$density <- as.numeric(tract_crs$density)
jenks_breaks <- classIntervals(tract_crs$density, n=5, style="jenks")$brks
print("Jenks Natural Breaks:")
print(jenks_breaks)

print("Equal Interval Breaks:")
print(equal_breaks)
print("Quantile Breaks:")
print(quantile_breaks)
print("Standard Deviation Breaks:")
print(sd_breaks)
print("Jenks Natural Breaks:")
print(jenks_breaks)


#draw histograms
#list of breaks and titles
breaks_list <- list(Equal = equal_breaks, 
                    Quantile = quantile_breaks, 
                    StandardDev = sd_breaks, 
                    Jenks = jenks_breaks)

titles <- names(breaks_list)

#create a plotting function
plot_histogram <- function(breaks, title) {
  ggplot(tract_crs, aes(x=density)) +
    geom_histogram(breaks=breaks, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
    labs(title=paste("Histogram with", title, "Breaks"), 
         x="Density", 
         y="Count") +
    theme_minimal()
}

#create a list to hold the plots
plot_list <- list()

#Generate the histograms and store them in the list
for(i in 1:length(breaks_list)) {
  plot_list[[i]] <- plot_histogram(breaks_list[[i]], titles[i])
}

#plot histograms in a 2x2 grid
grid.arrange(grobs=plot_list, ncol=2)





#2 data classification implement
#2.1 self codings Equal Interval
equal_interval_func <- function(data, n=5) {
  range_val <- range(data, na.rm = TRUE)
  interval <- (range_val[2] - range_val[1]) / n
  breaks <- seq(range_val[1], range_val[2], by=interval)
  return(breaks)
}


#self equal interval breaks
equal_breaks <- equal_interval_func(tract_crs$density, n=5)
print("Equal Interval Breaks:")
print(equal_breaks)

tract_crs$equal_class <- cut(tract_crs$density, breaks = equal_breaks, labels = FALSE, include.lowest = TRUE)

#plot the map
tm_shape(tract_crs) + 
  tm_polygons(col = "equal_class", palette = "-RdYlBu", title = "Density ") +
  tm_borders(lwd = 2) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "Population Density \n IN 2010\nSelf equal interval", 
            title.size = 4, 
            legend.title.size = 2,
            legend.outside = TRUE)

#quantile
quantile_func <- function(data, n=5) {
  quantile(data, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
}


#print the quantile breaks self code
quantile_breaks <- quantile_func(tract_crs$density, n=5)
print("Quantile Breaks:")
print(quantile_breaks)

tract_crs$quantile_class <- cut(tract_crs$density, breaks = quantile_breaks, labels = FALSE, include.lowest = TRUE)

tm_shape(tract_crs) + 
  tm_polygons(col = "quantile_class", palette = "-RdYlBu", title = "Density ") +
  tm_layout(title = "Population Density in Indiana 2010 (Quantile)", 
            title.size = 4, 
            legend.title.size = 2,
            legend.outside = TRUE)


# Standard Deviation
standard_deviation_func <- function(data) {
  data <- as.numeric(data)  # Convert data to plain numeric
  mean_val <- mean(data, na.rm = TRUE)
  sd_val <- sd(data, na.rm = TRUE)
  seq(mean_val - 2*sd_val, mean_val + 10*sd_val, by = 2*sd_val)# tried to change the number of breaks. 
}

sd_breaks <- standard_deviation_func(tract_crs$density)
print("Standard Deviation Breaks:")
print(sd_breaks)

tract_crs$sd_class <- cut(tract_crs$density, breaks = sd_breaks, labels = FALSE, include.lowest = TRUE)

# Plot the map
tm_shape(tract_crs) +
  tm_polygons(col = "sd_class", palette = "-RdYlBu", title = "Density") +
  tm_layout(title = "Population Density in Indiana 2010 (Standard Deviation)", title.size = 10, legend.outside = TRUE)


# Jenks
jenks_func <- function(data, n=5) {
  data_numeric <- as.numeric(data)  #explicitly convert to numeric
  classIntervals <- classInt::classIntervals(data_numeric, n = n, style = "jenks")
  breaks <- classIntervals$brks
  return(breaks)
}

# jenks manual 
jenks_manual <- function(data, n) {
  data <- sort(data)
  mtrx <- matrix(0, nrow=length(data)+1, ncol=n)
  
  #get the first row maximum value of 1000
  mtrx[1, ] <- 1000
  
  for(i in 2:n) {
    mtrx[i, 1] <- sum((1:i - mean(data[1:i], na.rm=TRUE))^2)
    
  }
  
  for(l in 2:n) {
    for(i in (l+1):length(data)) {
      vals <- (1:i-l+1)
      posposs <- (1:length(vals) + 1)
      SSB <- numeric(length(vals))
      for(pp in posposs) {
        SSB[pp] <- sum((vals[pp:length(vals)] - mean(data[pp:i], na.rm=TRUE))^2)
        
      }
      mtrx[i,l] <- min(SSB + mtrx[pp:length(vals), l-1])
    }
  }
  
  breaks <- numeric(n)
  breaks[n] <- data[length(data)]
  k <- length(data)
  
  for(i in n:2) {
    if(any(is.na(mtrx[1:k, i]) | is.na(mtrx[1:k, i-1]))) next
    
    mask <- (mtrx[1:k, i] - mtrx[1:k, i-1] - sum((data[1:k] - mean(data[1:k], na.rm=TRUE))^2)) == 0
    
    
    #handle situation when mask has no TRUE values
    if (all(!mask)) {
      breaks[i-1] <- NA
    } else {
      breaks[i-1] <- data[rev(which(mask))[1]]
      k <- rev(which(mask))[1]
    }
  }
  return(breaks)
}
#remove rows with NA or Inf in density column
tract_crs <- tract_crs[!is.na(tract_crs$density) & !is.infinite(tract_crs$density), ]


jenks_breaks_manual_result <- jenks_breaks_manual(tract_crs$density, 6)


tract_crs$density <- as.numeric(tract_crs$density)


#remove rows with NA or Inf in density column
tract_crs <- tract_crs[!is.na(tract_crs$density) & !is.infinite(tract_crs$density), ]

#get the jenks result manual 
jenks_manual_result <- jenks_manual(tract_crs$density, 6)


# Print the result
print("Jenks Natural Breaks (Manual):")
print(jenks_breaks_manual_result)


#print the jenks breaks self code
jenks_breaks <- jenks_func(tract_crs$density, n=5)
print("Jenks Breaks:")
print(jenks_breaks)

tract_crs$jenks_class <- cut(tract_crs$density, breaks = jenks_breaks, labels = FALSE, include.lowest = TRUE)

tm_shape(tract_crs) + 
  tm_polygons(col = "jenks_class", palette = "-RdYlBu", title = "Density") +
  tm_layout(title = "Population Density \nIN 2010 \n(Jenks Natural Breaks)", 
            title.size = 4,
            legend.title.size = 2,
            legend.outside = TRUE)


print("Equal Interval Breaks:")
print(equal_breaks)
print("Quantile Breaks:")
print(quantile_breaks)
print("Standard Deviation Breaks:")
print(sd_breaks)
print("Jenks Breaks:")
print(jenks_breaks)



#3the county part

#download the shapefile from Census website
#read in the county shapefile
county_data <- sf::st_read('./tl_2010_18_tract10/', layer='tl_2010_18_tract10')
county_crs <- sf::st_transform(county_data, crs=32616)

#extract the columns of interest as data frames to avoid spatial join
#only need GEOid and the County id 
county_df <- as.data.frame(county_crs)[, c("GEOID10", "COUNTYFP10")]

#join using dplyr's left_join() function
tract_crs <- left_join(tract_crs, county_df, by = "GEOID10")
head(tract_crs,5)


#aggregate by COUNTYFP10 and union the geometries
county_data <- tract_crs %>%
  group_by(COUNTYFP10) %>%
  summarise(
    total_population = sum(DP0010001, na.rm = TRUE),
    total_area = sum(blockarea, na.rm = TRUE), 
    #use union funciton to do the geometry sum
    geometry = st_union(geometry)
  ) %>%
  ungroup()


#base map
tm_shape(county_data) + 
  tm_borders(lwd = 0.5) +
  tm_fill(col = "total_population", 
          style = "quantile", 
          palette = "-RdYlBu", 
          title = "Total Population") +
  tm_layout(main.title = "Total Population by County")

#population density
county_data$density <- county_data$total_population/county_data$total_area
range(tract_crs$density)
#log population density
county_data$density_log <- log10(county_data$total_population / county_data$total_area)

# equal interval 
tm_shape(county_data) + 
  tm_fill(col = "density", style = "equal", palette = "-RdYlBu", title = "Density ") +
  tm_borders(lwd = 1) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "County Population Density in Indiana 2010 (Equal Interval)", 
            title.size = 4, 
            legend.outside = TRUE)

#equal interval reaks
equal_breaks <- classIntervals(county_data$density, n=5, style="equal")$brks
print("Equal Interval Breaks:")
print(equal_breaks)

#quantile map
tm_shape(county_data) + 
  tm_fill(col = "density", style = "quantile", palette = "-RdYlBu", title = "Density (Log)") +
  tm_borders(lwd = 2) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "County Population Density in Indiana 2010 (Quantile)", title.size = 10, legend.outside = TRUE)

#quantile breaks
quantile_breaks <- classIntervals(county_data$density, n=5, style="quantile")$brks
print("Quantile Breaks:")
print(quantile_breaks)

#SD  Map
tm_shape(county_data) + 
  tm_fill(col = "density", style = "sd", palette = "-RdYlBu", title = "Density (Log)") +
  tm_borders(lwd = 2) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "County Population Density in Indiana 2010 (Standard Deviation)", title.size = 10, legend.outside = TRUE)

#SD breaks
sd_breaks <- classIntervals(county_data$density, n=5, style="sd")$brks
print("Standard Deviation Breaks:")
print(sd_breaks)

#Jenks Map
tm_shape(county_data) + 
  tm_fill(col = "density", style = "jenks", palette = "-RdYlBu", title = "Density (Log)") +
  tm_borders(lwd = 2) +
  tm_scale_bar(text.size = 0.5, position = c("right", "bottom")) +
  tm_compass(type = "arrow", position = c("left", "top")) + 
  tm_credits("Map Maker: Haoruo Fu\nData Source: Census 2010", 
             position = c("left", "bottom"), size = 0.5) +
  tm_layout(title = "County Population Density in Indiana 2010 (Jenks Natural Breaks)", title.size = 10, legend.outside = TRUE)

#Jenks break
county_data$density <- as.numeric(county_data$density)
jenks_breaks <- classIntervals(county_data$density_log, n=5, style="jenks")$brks
print("Jenks Natural Breaks:")
print(jenks_breaks)




print("Equal Interval Breaks:")
print(equal_breaks)
print("Quantile Breaks:")
print(quantile_breaks)
print("Standard Deviation Breaks:")
print(sd_breaks)
print("Jenks Natural Breaks:")
print(jenks_breaks)


#equal interval breaks histogram
ggplot(county_data, aes(x=density)) +
    geom_histogram(breaks=equal_breaks, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
    labs(title="Histogram with Equal Interval Breaks", 
         x="Density", 
         y="Count") +
    theme_minimal()

#quantile histogram
ggplot(county_data, aes(x=density)) +
  geom_histogram(breaks=quantile_breaks, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
  labs(title="Histogram with Equal Interval Breaks", 
       x="Density", 
       y="Count") +
  theme_minimal()


#sd breaks histogram
ggplot(county_data, aes(x=density)) +
  geom_histogram(breaks=sd_breaks, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
  labs(title="Histogram with Equal Interval Breaks", 
       x="Density", 
       y="Count") +
  theme_minimal()

#Jenks histogram
ggplot(county_data, aes(x=density)) +
  geom_histogram(breaks=jenks_breaks, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
  labs(title="Histogram with Jenks Natural Breaks", 
       x="Density", 
       y="Count") +
  theme_minimal()




#MAUP comparison
#tried to do some comparison here. Just a sample calculation
# For tract level
tm_shape(tract_crs) + 
  tm_fill(col = "density_log") +
  tm_layout(title = "Tract Level Density")

# For county level
tm_shape(county_data) + 
  tm_fill(col = "density_log") +
  tm_layout(title = "County Level Density")
