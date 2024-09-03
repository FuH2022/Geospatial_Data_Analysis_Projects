#project 02 eaps507
#load required packages
library(sf)
library(sp)
library(rgdal)
library(tmap)
library(leaflet)
library(tmaptools)
library(dplyr)
library(ggplot2)
library(units)



#1.1
#read the given xls file csv
pu2014 <- read.csv('Twitter Data File_pu2014.csv', header = TRUE)

#create 'sp'(SpatialPointsDataFrame)
coords_sp <- cbind(pu2014$longitude, pu2014$latitude) #assign the cord
latlon_sp <- CRS("+proj=longlat +datum=WGS84") # Using WGS84 datum
sp <- SpatialPointsDataFrame(coords_sp, pu2014, proj4string = latlon_sp)

#create 'sf' (simple features object)
sf <- st_as_sf(pu2014, coords = c('longitude', 'latitude'), crs = 4326)

#creat the bounding box
#find the minimum and maximum latitude and longitude coordinates of tweets
xmin <- min(pu2014$longitude)
ymin <- min(pu2014$latitude)
xmax <- max(pu2014$longitude)
ymax <- max(pu2014$latitude)

#made a bit extra space for the coordinates, 0.01 for latitude and longitude
extra_latlong <- 0.01

#expand the latitude and longitude for 'sp' object
#calculate new bounding box coordinates
new_xmin_sp <- xmin - extra_latlong
new_ymin_sp <- ymin - extra_latlong
new_xmax_sp <- xmax + extra_latlong
new_ymax_sp <- ymax + extra_latlong

#set the expanded bounding box for 'sp' object
new_bbox_sp <- matrix(c(new_xmin_sp, new_ymin_sp, new_xmax_sp, new_ymax_sp), nrow = 2, ncol = 2)
attr(sp, "bbox") <- new_bbox_sp

#expand the latitude and longitude for 'sf' object
#calculate new bounding box coordinates
new_xmin_sf <- xmin - extra_latlong
new_ymin_sf <- ymin - extra_latlong
new_xmax_sf <- xmax + extra_latlong
new_ymax_sf <- ymax + extra_latlong

#set the expanded bounding box for 'sf' object
new_sf_bbox <- matrix(c(new_xmin_sf, new_ymin_sf, new_xmax_sf, new_ymax_sf), nrow = 2, ncol = 2)
attr(sf, "bbox") <- new_sf_bbox


#1.2 spatial object input and output conversion
#define file paths and layer names
sp_out_path <- '.'  #use the current directory
sf_out_path <- '.'  #use the current directory
layer_name_sp <- 'sp_out'
layer_name_sf <- 'sf_out'

#write the 'sp' object to an ESRI Shapefile using rgdal
rgdal::writeOGR(obj = sp, dsn = sp_out_path, layer = layer_name_sp, driver = 'ESRI Shapefile', overwrite_layer = TRUE)

#write the 'sf' object to an ESRI Shapefile using sf
sf::st_write(sf, dsn = sf_out_path, layer = layer_name_sf, driver = 'ESRI Shapefile', append = FALSE)

#read the shapefiles back from the working folder
sf_read <- st_read(dsn = sf_out_path, layer = layer_name_sf)
sp_read <- readOGR(dsn = sp_out_path, layer = layer_name_sp)

#convert to geojson file
st_write(sf_read, "sf_geojson.geojson", driver = "GeoJSON", append = FALSE)  # sf file to geojson
writeOGR(obj = sp_read, dsn = "sp_geojson.geojson", driver = "GeoJSON", overwrite_layer = TRUE) # sp files to geojson

#create 'sf' or 'sp' object from read-in data if needed
sf_from_geojson <- st_read("sf_geojson.geojson")
sp_from_geojson <- readOGR("sp_geojson.geojson")


#extra
#create KML file
st_write(sf_read, "sf_kml.kml", driver = "KML", append = FALSE) #sf
writeOGR(obj = sp_read, dsn = "sp_kml.kml", driver = "KML", overwrite_layer = TRUE) #sp

#create object sf or sp from KML
sp_from_kml <- readOGR("sp_kml.kml")
sf_from_kml <- st_read("sf_kml.kml")

#same as above can try geopackage file
#create GeoPackage file
st_write(sf_read, "sf_geopackage.gpkg", driver = "GPKG", append = FALSE) #sf
writeOGR(obj = sp_read, dsn = "sp_geopackage.gpkg", driver = "GPKG", overwrite_layer = TRUE) #sp

#create object sf or sp from GeoPackage
sp_from_geopackage <- readOGR("sp_geopackage.gpkg")
sf_from_geopackage <- st_read("sf_geopackage.gpkg")


#1.3Spatial object’s attribute manipulation (use the sf file from Task 1.1)

#convert the time to epoch time and time zone to eastern 
sf$datetime <- as.POSIXct(sf$epoch, origin = "1970-01-01", tz = "EST")
sf$date <- format(sf$datetime, format = '%d-%m-%Y')
sf$dayOfWeek <- format(sf$datetime, format = '%w')
sf$localTime <- format(sf$datetime, format = '%X')

#define the columns to keep for further use. 
columns_to_keep <- c('user_id', 'geometry', 'epoch', 'datetime', 'date', 'dayOfWeek', 'localTime')

#create a new 'sf' object with the selected columns for following steps
sf_sub <- sf[, columns_to_keep]

#redo the latitude and longitude for future user in case
tmp_c <- data.frame(st_coordinates(sf_sub$geometry))
sf_sub$lat <- tmp_c$Y
sf_sub$lon <- tmp_c$X

#check top 5 rows. 
head(sf_sub, 5)

#1.4  Mapping the overall tweets distribution (20 pts)

#set the tmap to view mode
tmap_mode('view')
#overall distribution of the tweeters
tm_shape(sf_sub) + 
  tm_dots(col='blue', size=0.05, alpha=0.5) +  # size for dot size, alpha for transparent setting
  tm_layout(title='Spatial Distribution of Tweets at  West Lafayette')

#month
#add a month column
sf_sub$month <- strftime(sf_sub$datetime, format='%B')

#convert this month column to month names and put them in order
sf_sub$month <- factor(sf_sub$month, levels=c('January', 'February', 'March', 'April',
                                              'May', 'June', 'July', 'August', 'September', 'October',
                                              'November', 'December'))

# Clustering the tweets by month
tm_shape(sf_sub) + 
  tm_dots('month', clustering=T, style='kmeans', legend.show=F) + 
  tm_shape(sf_sub) + 
  tm_dots('month', size=0.02, title='Month', alpha=.6, palette='Paired') +
  tm_layout(title='Clustered Tweets across Months')

#day of week
#convert the datetime to day-of-the-week
sf_sub$dayOfWeek <- strftime(sf_sub$datetime, format='%A')

#factorize the day-of-the-week to maintain order useing factor function
sf_sub$dayOfWeek <- factor(sf_sub$dayOfWeek, levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

#plot the spatial distribution of tweets by day-of-the-week
tm_shape(sf_sub) + 
  tm_dots('dayOfWeek', title='Day of Week', palette='Paired', size=0.02, alpha=.4) + 
  tm_layout(title='Spatial Distribution of Tweets by Day of Week - West Lafayette')

#draw weekday vs weekend
#categorize the days into 'Weekday' or 'Weekend'
sf_sub$dayCategory <- ifelse(sf_sub$dayOfWeek %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), 'Weekday', 'Weekend')

#plot the two maps using facet
tm_shape(sf_sub) + 
  tm_dots(col='dayCategory', title='Day Category', palette='Set2', size=0.02, alpha=.4) +
  tm_facets(by='dayCategory', ncol=2) + 
  tm_layout(title='Spatial Distribution of Tweets: Weekday vs. Weekend')

#hours
#extract the hour from the datetime column
sf_sub$hour <- strftime(sf_sub$datetime, format='%H') %>%
  factor(levels=c('00', '01', '02', '03', '04', '05', '06', '07', '08',
                  '09', '10', '11', '12', '13', '14', '15', '16', '17',
                  '18', '19', '20', '21', '22', '23'))

#create the hours_cut column to categorize tweets by time of day into four parts
sf_sub$hours_cut <- cut(as.numeric(sf_sub$hour), breaks = seq(0,24,6), 
                        labels=c('Midnight-6AM', '6AM-12PM', '12PM-6PM', '6PM-Midnight'))

#plot the map
tweet_map_by_hour <- tm_shape(sf_sub) + 
  tm_dots('hours_cut', title='Hour of the Day', palette='Set3', size=0.02, alpha=.4) +
  tm_layout(title='Spatial Distribution of Tweets by Hour of the Day - West Lafayette')

#plot the map with facets same as weekday vs weekend 
tweet_map_by_hour_facets <- tm_shape(sf_sub) + 
  tm_dots('hours_cut', title='Hour of the Day', palette='Set3', size=0.02, alpha=.4) +
  tm_facets(by = 'hours_cut', ncol=2, nrow=2) + 
  tm_layout(title='Spatial Distribution of Tweets by Hour of the Day - West Lafayette')

#top three users 
#count tweets per user
tweet_total <- data.frame(table(sf_sub$user_id))
names(tweet_total) <- c('user_id', 'freq')

#sort by frequency in decrement order
tweet_total <- tweet_total[order(tweet_total$freq, decreasing = TRUE), ]

#filter top 3 users
top3_ids <- as.numeric(as.character(tweet_total[seq(1, 3, 1), 'user_id']))

#filter the sf_sub dataset for these top 3 users
top3 <- sf_sub[sf_sub$user_id %in% top3_ids, ]  

#convert user_id to a factor for plotting
top3$user_id <- as.factor(top3$user_id) #if not the category of legend will look strange

#plot the three users. 
tweet_map_top3 <- tm_shape(top3) + 
  tm_dots('user_id', title='Top 3 Tweeters by User ID', palette='Spectral') +
  tm_layout(title='Top 3 Tweeters in Dataset')

#1.5
#draw the moving for three users
#make sure the data is in temporal order for each user
top3 <- top3[order(top3$user_id, top3$datetime),]

#create a function to compute the lines for each user
compute_lines <- function(user_data){
  st_cast(st_combine(st_geometry(user_data)), "LINESTRING")
}

#apply the function to each user
lines <- top3 %>% 
  group_by(user_id) %>% 
  group_map(~ compute_lines(.x))

#convert list of lines to an sf object
lines_sf <- do.call(rbind, lines)

#create an sf dataframe
lines_df <- st_sf(id = rep(1:3, each = nrow(lines_sf)/3), geometry = lines_sf)

#define a more contrasting palette 
palette_lines <- c("red", "blue", "green")#becsue the initial run the color is awful

#plot the movement of the top three users
movement_map <- tm_shape(top3) + 
  tm_dots(col = "user_id", palette = "Set1", title = "Tweets") +
  tm_shape(lines_df) +
  tm_lines(lwd = 2, col = "id", palette = palette_lines, alpha = 0.8) +
  tm_layout(title = "Movement of Top 3 Users over Time")

#Extra holiday analysis
#get day and month from the datetime
sf_sub$day <- day(sf_sub$datetime)
sf_sub$month <- month(sf_sub$datetime)

#identify and label holidays
#initialize the holiday
sf_sub$holiday <- NA
#select the holidays
sf_sub$holiday[sf_sub$day == 1 & sf_sub$month == 1] <- "New Year"
sf_sub$holiday[sf_sub$day == 25 & sf_sub$month == 12] <- "Christmas"
sf_sub$holiday[sf_sub$day == 1 & sf_sub$month == 9] <- "Labor Day"
sf_sub$holiday[sf_sub$day == 13 & sf_sub$month == 1] <- "First day of class (sp)"
sf_sub$holiday[sf_sub$day == 25 & sf_sub$month == 8] <- "First day of class (fa)"
#you can add other holidays as needed. I just did these for fun. 

#filter rows where holiday is not NA
sf_holidays <- sf_sub[!is.na(sf_sub$holiday), ]

#plot the spatial distribution of tweets by holiday
tmap_mode('view')
tm_shape(sf_holidays) + 
  tm_dots('holiday', title='Holiday', palette='Paired', size=0.05, alpha=.8) + 
  tm_layout(title='Spatial Distribution of Tweets by Holiday - West Lafayette')

#draw a bar graph
#count the number of tweets for each holiday
holiday_counts <- sf_holidays %>% 
  group_by(holiday) %>% 
  summarise(count = n())

#plot the counts with different colors for each bar
ggplot(holiday_counts, aes(x = holiday, y = count, fill = holiday)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Tweets per Holiday in West Lafayette",
       x = "Holiday",
       y = "Number of Tweets") +
  theme_minimal() +
  geom_text(aes(label = count), vjust = -0.5) +
  scale_fill_brewer(palette = "Paired")

#1.6 average distance
library(units)
#get the top three user distance
average_daily_distances_top3 <- daily_distances_df %>%
  group_by(user_id) %>%
  summarise(average_distance = mean(total_distance, na.rm = TRUE))
#plot the top three users distance movig in to graph. 
ggplot(daily_distances_df, aes(x = date, y = total_distance, color = as.factor(user_id), group = user_id)) +
  geom_line(size = 1) +
  labs(title = "Daily Moving Distance for Top 3 Users",
       x = "Date",
       y = "Distance (meters)",
       color = "User ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for clarity








