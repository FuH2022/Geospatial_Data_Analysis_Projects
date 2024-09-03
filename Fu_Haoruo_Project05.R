library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(tmap)
library(sf)
library(dbscan)
library(geosphere)
library(viridis)
library(cluster)




#task 01 data exploration
#import data
data <- read.csv("./CrimesChicago20220225.csv", header = TRUE)
data

#converting 'DATE..OF.OCCURRENCE' to datetime format


data$DATE..OF.OCCURRENCE <- as.POSIXct(data$DATE..OF.OCCURRENCE, format="%m/%d/%Y %I:%M:%S %p", tz="America/Chicago")


#delete missing cases in the data
data <- subset(data,
               !is.na(DATE..OF.OCCURRENCE) & 
                 !is.na(LATITUDE) & 
                 !is.na(LONGITUDE))


#create the month, day of week, time in a day and season
data$month <- month(data$DATE..OF.OCCURRENCE, label = TRUE)  # month
data$day <- weekdays(data$DATE..OF.OCCURRENCE)  # Day of week
data$hour <- hour(data$DATE..OF.OCCURRENCE)  # Hour of day
#write a function to get the season
season <- function(date) {
  m <- as.numeric(format(date, "%m"))  # Extract month as a numeric value
  
  if(m %in% 3:5) {
    return("Spring")
  } else if(m %in% 6:8) {
    return("Summer")
  } else if(m %in% 9:11) {
    return("Autumn")
  } else {
    return("Winter")
  }
}
data$season <- sapply(data$DATE..OF.OCCURRENCE, season)
head(data)

#classify crime and get frequency
crime_category_frequency <- data %>%
  group_by(PRIMARY.DESCRIPTION) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

ggplot(crime_category_frequency, aes(x = reorder(PRIMARY.DESCRIPTION, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), hjust = -0.3) +  # This adds the text labels
  coord_flip() +
  labs(title = "Frequency of Crime Categories in Chicago",
       x = "Crime Category",
       y = "Frequency") +
  theme_minimal()

#frequency by month
#group by month
monthly_crime_frequency <- data %>%
  group_by(month) %>%
  summarise(Frequency = n()) 
#plot
ggplot(monthly_crime_frequency, aes(x = month, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(title = "Frequency of Crimes by Month in Chicago",
       x = "Month",
       y = "Frequency") +
  theme_minimal()

#day of week
daily_crime_frequency <- data %>%
  group_by(day) %>%
  summarise(Frequency = n()) %>%
  mutate(day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))  # ordering days

ggplot(daily_crime_frequency, aes(x = day, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(title = "Frequency of Crimes by Day of the Week in Chicago",
       x = "Day of the Week",
       y = "Frequency") +
  theme_minimal()

#weekday vs Weekend
data$weektime <- ifelse(data$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

weektime_crime_frequency <- data %>%
  group_by(weektime) %>%
  summarise(Frequency = n())

ggplot(weektime_crime_frequency, aes(x = weektime, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(title = "Frequency of Crimes: Weekday vs. Weekend in Chicago",
       x = "Day Type",
       y = "Frequency") +
  theme_minimal()


#by season
seasonal_crime_frequency <- data %>%
  group_by(season) %>%
  summarise(Frequency = n()) %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")))  # Ordering the seasons

ggplot(seasonal_crime_frequency, aes(x = season, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(title = "Frequency of Crimes by Season in Chicago",
       x = "Season",
       y = "Frequency") +
  theme_minimal()

#by hour
hourly_crime_frequency <- data %>%
  group_by(hour) %>%
  summarise(Frequency = n())

ggplot(hourly_crime_frequency, aes(x = hour, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.5) +
  labs(title = "Frequency of Crimes by Hour in Chicago",
       x = "Hour of the Day (0-23)",
       y = "Frequency") +
  theme_minimal()


#select three crimes
three_crime <- subset(data, PRIMARY.DESCRIPTION %in% c("MOTOR VEHICLE THEFT", "CRIMINAL SEXUAL ASSAULT", "ROBBERY"))

#clean data with no latitude longitude
three_crime <- subset(three_crime, !is.na(LATITUDE) & !is.na(LONGITUDE))

three_crime$Latitude <- as.numeric(three_crime$LATITUDE)
three_crime$Longitude <- as.numeric(three_crime$LONGITUDE)

#visualize them

tmap_mode("view")
# Convert data frame to spatial object
three_crime_sf <- st_as_sf(three_crime, coords = c("Longitude", "Latitude"), crs = 4326)

# Plotting
tm_shape(three_crime_sf) +
  tm_dots(col = "PRIMARY.DESCRIPTION", 
          title = "Type of Crime", 
          palette = "Set1", 
          size = 0.01) + 
  tm_layout(title = "Selected Crimes in Chicago", legend.position = c("right", "bottom"))

# For CRIMINAL SEXUAL ASSAULT
sexual_assault <- subset(three_crime_sf, PRIMARY.DESCRIPTION == "CRIMINAL SEXUAL ASSAULT")
tm_shape(sexual_assault) +
  tm_dots(col = "red", size = 0.01) +
  tm_layout(title = "Criminal Sexual Assault in Chicago")

# For MOTOR VEHICLE THEFT
motor_theft <- subset(three_crime_sf, PRIMARY.DESCRIPTION == "MOTOR VEHICLE THEFT")
tm_shape(motor_theft) +
  tm_dots(col = "blue", size = 0.01) +
  tm_layout(title = "Motor Vehicle Theft in Chicago")

# For ROBBERY
robbery <- subset(three_crime_sf, PRIMARY.DESCRIPTION == "ROBBERY")
tm_shape(robbery) +
  tm_dots(col = "green", size = 0.01) +
  tm_layout(title = "Robbery in Chicago")




# Define the three crime categories
crime_categories <- c("MOTOR VEHICLE THEFT", "CRIMINAL SEXUAL ASSAULT", "ROBBERY")

# Parameters for DBSCAN
eps <- 700  # 1 kilometer in 1000 meters
minPts <- 35 #this cahged a lot during assignment

# Convert to spatial data in a suitable UTM zone
three_crime_sf <- st_as_sf(three_crime, coords = c("Longitude", "Latitude"), crs = 4326)
three_crime_sf <- st_transform(three_crime_sf, 32616)  # chicago zone

# Function to generate k-distance plot for choosing eps
k_distance_plot <- function(data, k, crime) {
  # Compute k-distances
  kdistances <- dbscan::kNNdist(data, k) #into meters
  
  # Sort the distances
  sorted_dists <- sort(kdistances, decreasing = TRUE)
  
  # Create the plot
  df <- data.frame(point = 1:length(sorted_dists), distance = sorted_dists)
  p <- ggplot(df, aes(x = point, y = distance)) +
    geom_line() +
    labs(title = paste(crime, "K-Distance Graph for k =", k),
         x = "Points sorted by distance",
         y = "kth Nearest Neighbor Distance") +
    theme_minimal()
  return(p)
}

# Display k-distance plots for each crime category
k = 10
for (crime in crime_categories) {
  crime_data <- subset(three_crime_sf, PRIMARY.DESCRIPTION == crime)
  coords <- as.data.frame(st_coordinates(crime_data))
  plot <- k_distance_plot(coords, k, crime)
  print(plot)
}

# Apply DBSCAN and visualize results 
for (crime in crime_categories) {
  crime_data <- subset(three_crime_sf, PRIMARY.DESCRIPTION == crime)
  coords <- as.data.frame(st_coordinates(crime_data))
  dbscan_result <- dbscan::dbscan(coords, eps = eps, minPts = minPts)
  
  crime_data$dbscancluster <- dbscan_result$cluster
  
  # Visualization with tmap
  m <- tm_shape(crime_data) +
    tm_dots(col = "dbscancluster", 
            title = paste(crime, '\n', "eps =", eps, "minPts =", minPts),
            palette = rainbow(max(crime_data$dbscancluster, na.rm = TRUE) + 1), 
            shape = 20, 
            popup.vars = "PRIMARY.DESCRIPTION")
  print(m)
}

# Convert to UTM for Chicago
selected_crimes_sf <- st_as_sf(selected_crimes, coords = c("Longitude", "Latitude"), crs = 4326)
selected_crimes_sf <- st_transform(selected_crimes_sf, 32616)

# OPTICS

minPts <- 30
xi <- 0.02

# Loop through each crime category
for (crime in crime_categories) {
  # Subset data
  crime_data <- subset(three_crime_sf, PRIMARY.DESCRIPTION == crime)
  coords <- st_coordinates(crime_data)
  # Apply OPTICS
  optics_result <- optics(coords, minPts = minPts)
  # Extract clusters using Xi
  clust <- extractXi(optics_result, xi = xi)
  crime_data$cluster <- clust$cluster
  # Convert back to lat/long for visualization
  crime_sf <- st_transform(crime_data, 4326)
  # Using tmap to visualize
  m <- tm_shape(crime_sf) +
    tm_dots(col = "cluster", 
            palette = viridis(length(unique(crime_sf$cluster))),
            shape = 20, 
            title = paste("OPTICS with Xi clustering for", crime))
  print(m)
  
  # Plotting reachability plot
  plot(optics_result, main = paste("Reachability plot for", crime))
}


#HDBSCAN

# Define crime categories
crime_categories <- c("MOTOR VEHICLE THEFT", "CRIMINAL SEXUAL ASSAULT", "ROBBERY")

# Convert to spatial data in a suitable UTM zone
three_crime_sf <- st_as_sf(three_crime, coords = c("Longitude", "Latitude"), crs = 4326)
three_crime_sf <- st_transform(three_crime_sf, 32616)  # chicago zone

# Handle NA values and select required columns
three_crime_sf <- na.omit(three_crime_sf[, c("Longitude", "Latitude", "PRIMARY.DESCRIPTION")])

# Set minPts for HDBSCAN
minPts <- 30

# Loop through each crime category
for (crime in crime_categories) {
  
  # Subset data
  crime_data <- subset(three_crime_sf, PRIMARY.DESCRIPTION == crime)
  coords <- as.data.frame(st_coordinates(crime_data))
  
  # Apply HDBSCAN
  hdbscan_result <- hdbscan(coords, minPts = minPts)
  
  # Handle NA in clustering results
  if(any(is.na(hdbscan_result$cluster))) {
    warning(paste("NA values found in clustering results for", crime, "- possibly due to noise points. Replacing NAs with 0."))
    hdbscan_result$cluster[is.na(hdbscan_result$cluster)] <- 0
  }
  
  crime_data$hdbscancluster <- hdbscan_result$cluster
  
# Visualization with tmap
  m <- tm_shape(crime_data) +
    tm_dots(col = "hdbscancluster",
            title = paste(crime, "HDBSCAN", "\nminPts =", minPts),
            palette = rainbow(max(crime_data$hdbscancluster, na.rm = TRUE) + 1),
            shape = 20,
            popup.vars = "PRIMARY.DESCRIPTION") +
    tm_layout(main.title = paste("HDBSCAN clustering for", crime),
              main.title.size = 1.5)
  
  print(m)
}




#Task 03

# Convert to spatial data 
three_crime_sf <- st_as_sf(three_crime, coords = c("Longitude", "Latitude"), crs = 4326)
three_crime_sf <- st_transform(three_crime_sf, 32616)  # chicago zone

calculate_centroids <- function(data, eps, minPts) {
  coords <- as.data.frame(st_coordinates(data))
  dbscan_result <- dbscan::dbscan(coords, eps = eps, minPts = minPts)$cluster
  # Handling NA in clustering results
  dbscan_result[is.na(dbscan_result)] <- 0
  coords$dbscancluster <- dbscan_result  # Assign cluster results directly to coords dataframe
  centroids <- aggregate(cbind(X, Y) ~ dbscancluster, data = coords, FUN = mean)
  return(centroids)
}


find_closest_centroids <- function(centroids) {
  # Initialize minimum distance as infinity
  min_distance <- Inf
  # Initialize closest pair
  closest_pair <- c(NA, NA)
  
  # Loop through all possible pairs of centroids
  for (i in 1:(nrow(centroids) - 1)) {
    for (j in (i + 1):nrow(centroids)) {
      
      # Calculate the Euclidean distance since we're in UTM
      dist <- sqrt((centroids[i, "X"] - centroids[j, "X"])^2 + (centroids[i, "Y"] - centroids[j, "Y"])^2)
      
      # If the current distance is smaller than the known minimum, update min_distance and closest_pair
      if (dist < min_distance) {
        min_distance <- dist
        closest_pair <- c(i, j)
      }
    }
  }
  
  # Return the minimum distance and the indices of the two closest centroids
  return(list(min_distance = min_distance, closest_pair = closest_pair))
}



# Apply the functions and get results for each crime category
for (crime in crime_categories) {
  crime_data <- subset(three_crime_sf, PRIMARY.DESCRIPTION == crime)
  
  centroids <- calculate_centroids(crime_data, eps, minPts)
  closest_results <- find_closest_centroids(centroids)
  #print out result
  cat("\nFor crime category:", crime)
  cat("\nThe minimum distance between cluster centers is:", closest_results$min_distance, "meters.")
  cat("\nThe closest pair of clusters are:", closest_results$closest_pair[1], "and", closest_results$closest_pair[2], "\n")
}



#motor vehicle
# Define parameters
eps <- 750  # 750 meters
minPts <- 5

# Data Filtering for MOTOR VEHICLE THEFT
data_theft <- subset(three_crime_sf, PRIMARY.DESCRIPTION == "MOTOR VEHICLE THEFT")

# Calculating Cluster Centroids
theft_centroids <- calculate_centroids(data_theft, eps = eps, minPts = minPts)
theft_centroids

theft_all_distances <- dist(theft_centroids[, c("X", "Y")])

# Convert distances matrix to a vector, considering only unique distances
distances_vector <- as.vector(upper.tri(theft_all_distances) * theft_all_distances)
distances_vector <- distances_vector[distances_vector > 0]

# Plotting a histogram of distances using ggplot2
ggplot() +
  geom_histogram(aes(x = distances_vector), bins = 30, fill="skyblue", color="black") +
  labs(x = "Distance (meters)", 
       y = "Frequency", 
       title = "Histogram of Distances between Cluster Centers",
       subtitle = "MOTOR VEHICLE THEFT") +
  theme_minimal()

#robbery
# Data Filtering for ROBBERY
data_robbery <- subset(three_crime_sf, PRIMARY.DESCRIPTION == "ROBBERY")

# Calculating Cluster Centroids
robbery_centroids <- calculate_centroids(data_robbery, eps = eps, minPts = minPts)
robbery_centroids

robbery_all_distances <- dist(robbery_centroids[, c("X", "Y")])

# Convert distances matrix to a vector, considering only unique distances
distances_vector_robbery <- as.vector(upper.tri(robbery_all_distances) * robbery_all_distances)
distances_vector_robbery <- distances_vector_robbery[distances_vector_robbery > 0]

# Plotting a histogram of distances using ggplot2
ggplot() +
  geom_histogram(aes(x = distances_vector_robbery), bins = 30, fill="lightcoral", color="black") +
  labs(x = "Distance (meters)", 
       y = "Frequency", 
       title = "Histogram of Distances between Cluster Centers",
       subtitle = "ROBBERY") +
  theme_minimal()

#sexual assault crime

# Data Filtering for SEXUAL ASSAULT
data_sexual_assault <- subset(three_crime_sf, PRIMARY.DESCRIPTION == "CRIMINAL SEXUAL ASSAULT")

# Calculating Cluster Centroids
sexual_assault_centroids <- calculate_centroids(data_sexual_assault, eps = eps, minPts = minPts)
sexual_assault_centroids

sexual_assault_all_distances <- dist(sexual_assault_centroids[, c("X", "Y")])

# Convert distances matrix to a vector, considering only unique distances
distances_vector_sexual_assault <- as.vector(upper.tri(sexual_assault_all_distances) * sexual_assault_all_distances)
distances_vector_sexual_assault <- distances_vector_sexual_assault[distances_vector_sexual_assault > 0]

# Plotting a histogram of distances using ggplot2
ggplot() +
  geom_histogram(aes(x = distances_vector_sexual_assault), bins = 30, fill="purple", color="black") +
  labs(x = "Distance (meters)", 
       y = "Frequency", 
       title = "Histogram of Distances between Cluster Centers",
       subtitle = "SEXUAL ASSAULT") +
  theme_minimal()


#task3-2
# Function to get the minimum non-zero distance for a distance matrix
get_min_distance <- function(dist_matrix) {
  # Convert distances matrix to a vector, zero excluded
  distances_vector <- as.vector(upper.tri(dist_matrix) * dist_matrix)
  distances_vector <- distances_vector[distances_vector > 0]
  
  return(min(distances_vector))
}

#MOTOR VEHICLE THEFT
min_distance_theft <- get_min_distance(theft_all_distances)

#ROBBERY 
min_distance_robbery <- get_min_distance(robbery_all_distances)

# For SEXUAL ASSAULT
min_distance_sexual_assault <- get_min_distance(sexual_assault_all_distances)

# Average Minimum Distance
average_min_distance <- mean(c(min_distance_theft, min_distance_robbery, min_distance_sexual_assault))



# Print Results
cat("Minimum Distance for MOTOR VEHICLE THEFT:", min_distance_theft, "\n")
cat("Minimum Distance for ROBBERY:", min_distance_robbery, "\n") 
cat("Minimum Distance for SEXUAL ASSAULT:", min_distance_sexual_assault, "\n")
cat("Average Minimum Distance:", average_min_distance, "\n")


#3-2modofication
# Add a category column to each centroid dataset
theft_centroids$category <- "MOTOR VEHICLE THEFT"
robbery_centroids$category <- "ROBBERY"
sexual_assault_centroids$category <- "CRIMINAL SEXUAL ASSAULT"

all_centroids <- rbind(theft_centroids, robbery_centroids, sexual_assault_centroids)
closest_result <- find_closest_centroids(all_centroids)

print(paste("Closest centroids are from categories:", all_centroids$category[closest_result$closest_pair[1]], "and", all_centroids$category[closest_result$closest_pair[2]]))
print(paste("The minimum distance between centroids is:", closest_result$min_distance, "meters"))





# Convert data frame to sf object
all_centroids_sf <- st_as_sf(all_centroids, coords = c("X", "Y"), crs = 4326)

# Function to calculate all pairwise distances
calculate_all_distances <- function(centroids) {
  distances <- numeric()
  for (i in 1:(nrow(centroids)-1)) {
    for (j in (i+1):nrow(centroids)) {
      dist <- distHaversine(centroids[i, c("X", "Y")], 
                            centroids[j, c("X", "Y")])
      distances <- c(distances, dist)
    }
  }
  return(distances)
}
distance
# Calculate all pairwise distances
all_distances <- calculate_all_distances(all_centroids)





