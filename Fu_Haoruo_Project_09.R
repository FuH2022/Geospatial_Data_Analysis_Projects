#   Project 09

library(osmdata)
library(sf)
library(sp)
library(tmap)
library(dplyr)


available_features()

#    Task 01 read data for West Lafayette
bbox <- getbb('west lafayette, usa')
typeof(bbox)
class(bbox)


# Add building features
wl_blds <- opq(bbox) # Open a query based on the bbox
wl_blds <- add_osm_feature(wl_blds, key = 'building') 
wl_blds_sf<- osmdata_sf(wl_blds) # Convert osm to sf

ttm()
qtm(wl_blds_sf$osm_polygons) # Plot the polygons

# Add roads features
wl_rds <- opq(bbox)
wl_rds <- add_osm_feature(wl_rds, key = 'highway')
wl_rds_sf<- osmdata_sf(wl_rds)

qtm(wl_rds_sf$osm_lines) # Plot the roads

# Add restaurant features (Own Interest)
wl_restaurants <- opq(bbox)
wl_restaurants <- add_osm_feature(wl_restaurants, key = 'amenity', value = 'restaurant')
wl_restaurants_sf <- osmdata_sf(wl_restaurants)

# Plot the restaurants 
qtm(wl_restaurants_sf$osm_points)


# Add restaurant features as polygons
wl_restaurants_poly <- opq(bbox)
wl_restaurants_poly <- add_osm_feature(wl_restaurants_poly, key = 'amenity', value = 'restaurant')
wl_restaurants_sf_poly <- osmdata_sf(wl_restaurants_poly)

# Plot the restaurants as polygons
qtm(wl_restaurants_sf_poly$osm_polygons)



# Add parking lot features 
wl_parking <- opq(bbox)
wl_parking <- add_osm_feature(wl_parking, key = 'amenity', value = 'parking')
wl_parking_sf <- osmdata_sf(wl_parking)

# Plot the parking lots
qtm(wl_parking_sf$osm_polygons) # or osm_points, depending on the data

# Get bounding box for buildings
bbox_buildings <- st_bbox(wl_blds_sf$osm_polygons)
print(bbox_buildings)

# Get bounding box for roads
bbox_roads <- st_bbox(wl_rds_sf$osm_lines)
print(bbox_roads)

# Get bounding box for parking lots
bbox_parking <- st_bbox(wl_parking_sf$osm_polygons) # or osm_points, depending on the data
print(bbox_parking)

#  Save shapefile
# Define file paths for the shapefiles
path_to_buildings_shp <- "./buildings.shp"
path_to_roads_shp <- "./roads.shp"
path_to_parking_shp <- "./parking.shp"

# Write the datasets to shapefiles with overwrite capability
st_write(wl_blds_sf$osm_polygons, path_to_buildings_shp, delete_layer = TRUE)
st_write(wl_rds_sf$osm_lines, path_to_roads_shp, delete_layer = TRUE)
st_write(wl_parking_sf$osm_polygons, path_to_parking_shp, delete_layer = TRUE)





#   Task 02 
# Manual calculations
calculate_length_manual <- function(geom) {
  coords <- st_coordinates(geom)
  # Calculate the sum of distances between consecutive vertices
  length_sum <- sum(sqrt(diff(coords[,1])^2 + diff(coords[,2])^2))
  return(length_sum)
}

calculate_area_manual <- function(geom) {

  area <- st_area(geom)
  return(as.numeric(area))
}

calculate_centroid_manual <- function(geom) {
  coords <- st_coordinates(geom)
  x_centroid <- mean(coords[, "X"])
  y_centroid <- mean(coords[, "Y"])
  return(c(x_centroid, y_centroid))
}

#extract it
actual_sf_object <- wl_blds_sf$osm_polygons

# Now apply the manual functions to each building's geometry
wl_blds_sf$Length_Manual <- sapply(st_geometry(actual_sf_object), calculate_length_manual)
wl_blds_sf$Area_Manual <- sapply(st_geometry(actual_sf_object), calculate_area_manual)
centroids_manual <- t(sapply(st_geometry(actual_sf_object), calculate_centroid_manual))

# Add the results to the wl_blds_sf data frame
wl_blds_sf$Centroid_X_Manual <- centroids_manual[, 1]
wl_blds_sf$Centroid_Y_Manual <- centroids_manual[, 2]


# Add manual calculation results as new columns
wl_blds_sf$Length_Manual <- wl_blds_sf$Length_Manual
wl_blds_sf$Area_Manual <- wl_blds_sf$Area_Manual
wl_blds_sf$Centroid_X_Manual <- wl_blds_sf$Centroid_X_Manual
wl_blds_sf$Centroid_Y_Manual <- wl_blds_sf$Centroid_Y_Manual


# Summary statistics for length
length_summary <- summary(wl_blds_sf$Length_Manual)

# Summary statistics for area
area_summary <- summary(wl_blds_sf$Area_Manual)

# Summary statistics for X centroid
x_centroid_summary <- summary(wl_blds_sf$Centroid_X_Manual)

# Summary statistics for Y centroid
y_centroid_summary <- summary(wl_blds_sf$Centroid_Y_Manual)

# Combine all summaries into a list for easier viewing
summaries <- list(
  Length = length_summary,
  Area = area_summary,
  X_Centroid = x_centroid_summary,
  Y_Centroid = y_centroid_summary
)

# Print summaries
print(summaries)



#   Build in function
# Calculate length, area, and centroid using built-in functions
wl_blds_sf_geo <- st_geometry(wl_blds_sf$osm_polygons)
wl_blds_sf_lines <- st_cast(wl_blds_sf_geo, "LINESTRING")

wl_blds_sf$Length_BuiltIn <- st_length(wl_blds_sf_lines)
wl_blds_sf$Area_BuiltIn <- st_area(wl_blds_sf$osm_polygons)
wl_blds_sf$Centroid_X_BuiltIn <- st_coordinates(st_centroid(wl_blds_sf$osm_polygons))[, 1]
wl_blds_sf$Centroid_Y_BuiltIn <- st_coordinates(st_centroid(wl_blds_sf$osm_polygons))[, 2]



# Summary statistics for built-in length
length_builtin_summary <- summary(wl_blds_sf$Length_BuiltIn)

# Summary statistics for built-in area
area_builtin_summary <- summary(wl_blds_sf$Area_BuiltIn)

# Summary statistics for built-in X centroid
x_centroid_builtin_summary <- summary(wl_blds_sf$Centroid_X_BuiltIn)

# Summary statistics for built-in Y centroid
y_centroid_builtin_summary <- summary(wl_blds_sf$Centroid_Y_BuiltIn)

# Combine all built-in summaries into a list for easier viewing
builtin_summaries <- list(
  Length_BuiltIn = length_builtin_summary,
  Area_BuiltIn = area_builtin_summary,
  X_Centroid_BuiltIn = x_centroid_builtin_summary,
  Y_Centroid_BuiltIn = y_centroid_builtin_summary
)

# Print built-in summaries
print(builtin_summaries)



# Calculate the differences
wl_blds_sf$Area_Diff <- wl_blds_sf$Area_Manual - as.numeric(wl_blds_sf$Area_BuiltIn)
wl_blds_sf$Length_Diff <- wl_blds_sf$Length_Manual - as.numeric(wl_blds_sf$Length_BuiltIn)
wl_blds_sf$Centroid_X_Diff <- wl_blds_sf$Centroid_X_Manual - wl_blds_sf$Centroid_X_BuiltIn
wl_blds_sf$Centroid_Y_Diff <- wl_blds_sf$Centroid_Y_Manual - wl_blds_sf$Centroid_Y_BuiltIn

# Histogram for Area Differences
hist(wl_blds_sf$Area_Diff, main = "Histogram of Area Differences", xlab = "Area Difference (Manual - Built-In)", col = "skyblue", breaks = 50)

# Histogram for Length Differences
hist(wl_blds_sf$Length_Diff, main = "Histogram of Length Differences", xlab = "Length Difference (Manual - Built-In)", col = "salmon", breaks = 50)

# Histogram for Centroid X Differences
hist(wl_blds_sf$Centroid_X_Diff, main = "Histogram of Centroid X Differences", xlab = "Centroid X Difference (Manual - Built-In)", col = "lightgreen", breaks = 50)

# Histogram for Centroid Y Differences
hist(wl_blds_sf$Centroid_Y_Diff, main = "Histogram of Centroid Y Differences", xlab = "Centroid Y Difference (Manual - Built-In)", col = "wheat", breaks = 50)




# Calculate the centroids
centroids <- st_centroid(wl_blds_sf_polygons)

# Using st_intersects to check if centroids intersect their respective polygons
intersects_matrix <- st_intersects(wl_blds_sf_polygons, centroids, sparse = FALSE)

# Find buildings where centroids do not intersect their polygons
outside_centroids <- which(apply(intersects_matrix, 1, function(row) !any(row)))

# Get the buildings with centroids outside as an sf object
buildings_with_external_centroids <- wl_blds_sf_polygons[outside_centroids, ]

# Show a couple of examples
print(buildings_with_external_centroids[1:2, ])

# Plot the map with tmap
tmap_mode("view")
tm_shape(wl_blds_sf_polygons) +
  tm_polygons() +
  tm_shape(wl_blds_sf_polygons[outside_centroids, ]) +
  tm_borders(col = "red") + 
  tm_shape(centroids[outside_centroids, ]) +
  tm_dots(col = "blue", size = 0.05) +
  tm_layout(main.title = "Buildings with External Centroids")



# Manual
library(sf)

# Manual calculation of centroids from the provided functions
centroids_manual <- t(sapply(st_geometry(wl_blds_sf_polygons), calculate_centroid_manual))
centroids_manual_sf <- st_sf(geometry = st_sfc(lapply(1:nrow(wl_blds_sf_polygons), function(i) st_point(centroids_manual[i, ])), crs = st_crs(wl_blds_sf_polygons)))

# Check if the manually calculated centroids are properly within the polygons
contains_properly <- st_contains_properly(wl_blds_sf_polygons, centroids_manual_sf, sparse = FALSE)

# Find indices where centroids are not properly contained within the polygons
outside_indices_manual <- which(apply(contains_properly, 1, function(row) !any(row)))

# Extract those polygons
buildings_with_external_centroids_manual <- wl_blds_sf_polygons[outside_indices_manual, ]

# Print a couple of examples
print(buildings_with_external_centroids_manual[1:2, ])

library(tmap)

# Plot the map with tmap
tmap_mode("view")

# Set up the base layer with all polygons
tm_shape(wl_blds_sf_polygons) +
  tm_polygons() +
  # Add a layer for buildings with external centroids, highlighted in red
  tm_shape(buildings_with_external_centroids_manual) +
  tm_borders(col = "red") +
  # Add a layer for the centroids of those buildings
  tm_shape(centroids_manual_sf[outside_indices_manual, ]) +
  tm_dots(col = "blue", size = 0.05) +
  # Finalize the layout of the map
  tm_layout(main.title = "Buildings with External Centroids (Manual)")




#        Task 03
library(sf)
library(tmap)
library(dplyr)
library(stringr)
# Load the Purdue campus data 
purdue_data_2014 <- st_read("./PurdueBuilddingShpFile/buildings.shp")

# Remove empty geometries
purdue_data_2014 <- purdue_data_2014[!st_is_empty(purdue_data_2014), ]

# Transform the data to WGS84 before performing any operations
purdue_data_2014_wgs84 <- st_transform(purdue_data_2014, crs = 4326)

# Make geometries valid
purdue_data_2014_wgs84 <- st_make_valid(purdue_data_2014_wgs84)

# Filter out rows where GIS_BID is NA
purdue_data_2014_wgs84 <- purdue_data_2014_wgs84 %>%
  filter(!is.na(GIS_BID))

# Remove duplicate buildings based on GIS_BID
purdue_data_2014_unique <- purdue_data_2014_wgs84 %>%
  distinct(GIS_BID, .keep_all = TRUE)

# Dissolve overlapping polygons by GIS_BID
purdue_data_2014_dissolved <- purdue_data_2014_unique %>%
  group_by(GIS_BID) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# Check for any remaining invalid geometries after dissolve
invalid_geoms_dissolved <- st_is_valid(purdue_data_2014_dissolved, reason = TRUE)
print(invalid_geoms_dissolved)

# If there are still invalid geometries, attempt to simplify them
if (any(invalid_geoms_dissolved != "Valid geometry")) {
  purdue_data_2014_dissolved <- st_simplify(purdue_data_2014_dissolved, preserveTopology = TRUE)
}

# Set tmap to view mode for an interactive map
tmap_mode("view")

# Create a map of Purdue University Buildings in 2014 (WGS84) after dissolving
tm_map <- tm_shape(purdue_data_2014_dissolved) +
  tm_borders() +
  tm_fill(col = "blue", alpha = 0.5) +
  tm_layout(main.title = "Purdue University Buildings in 2014 (Dissolved)")

# Print the map
print(tm_map)


# Load the GeoJSON file containing the boundaries of Purdue University
purdue_boundaries <- st_read('./export.geojson')

# Convert linestrings to polygons if necessary
if (any(st_geometry_type(purdue_boundaries) == "LINESTRING")) {
  purdue_boundaries_polygons <- st_polygonize(purdue_boundaries)
  print(st_geometry_type(purdue_boundaries_polygons))
} else {
  purdue_boundaries_polygons <- purdue_boundaries
}

# Perform the spatial intersection on the dissolved data
purdue_buildings_within_boundaries <- st_intersection(purdue_data_2014_dissolved, purdue_boundaries_polygons)

# Check the intersection result
print(purdue_buildings_within_boundaries)

# Set tmap to view mode for an interactive map
tmap_mode("view")

# Create the map object, starting with the boundaries
tm_map_boundaries <- tm_shape(purdue_boundaries_polygons) +
  tm_borders(col = "red") +
  tm_layout(main.title = "Purdue University Buildings within Boundaries (2014)")

# Add the buildings to the map
tm_map_boundaries <- tm_map_boundaries + tm_shape(purdue_buildings_within_boundaries) +
  tm_polygons(col = "green", alpha = 0.5)

# Print the map with both layers
print(tm_map_boundaries)










# Current Purdue Buildings

# Extract the 'sf' object for polygons which contain the buildings from the OSM data
osm_buildings_sf <- wl_blds_sf$osm_polygons

# Remove duplicates based on osm_id
osm_buildings_sf <- osm_buildings_sf %>%
  distinct(osm_id, .keep_all = TRUE)

# Transform the CRS of the OSM buildings to match the CRS of the Purdue boundaries
osm_buildings_sf <- st_transform(osm_buildings_sf, st_crs(purdue_boundaries_polygons))

# Dissolve any overlapping polygons based on osm_id
osm_buildings_dissolved <- osm_buildings_sf %>%
  group_by(osm_id) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# Load the GeoJSON file containing the boundaries of Purdue University, if not already loaded
purdue_boundaries <- st_read('./export.geojson')

# Convert linestrings to polygons if necessary, if not already done
if (any(st_geometry_type(purdue_boundaries) == "LINESTRING")) {
  purdue_boundaries_polygons <- st_polygonize(purdue_boundaries)
} else {
  purdue_boundaries_polygons <- purdue_boundaries
}

# Perform a spatial intersection to get buildings within the Purdue boundaries
buildings_within_purdue <- st_intersection(osm_buildings_dissolved, purdue_boundaries_polygons)

# Check for any remaining invalid geometries after the intersection
invalid_geoms <- st_is_valid(buildings_within_purdue, reason = TRUE)
print(invalid_geoms)

# If there are still invalid geometries, attempt to simplify them
if (any(invalid_geoms != "Valid geometry")) {
  buildings_within_purdue <- st_simplify(buildings_within_purdue, preserveTopology = TRUE)
}

# Now plot the Purdue boundaries and the OSM buildings within those boundaries using tmap
tmap_mode("view")
tm_map <- tm_shape(purdue_boundaries_polygons) +
  tm_borders(col = "red") +
  tm_shape(buildings_within_purdue) +
  tm_polygons(col = "blue", alpha = 0.5) +
  tm_layout(main.title = "OSM Buildings within Purdue University Boundaries")

# Print the map
print(tm_map)


# Calculate the total area of Purdue buildings in 2014 within the boundaries
total_area_2014 <- sum(st_area(purdue_buildings_within_boundaries))

# Calculate the total area of current OSM Purdue buildings within the boundaries
total_area_current <- sum(st_area(buildings_within_purdue))

# Calculate the change in area
area_change <- total_area_current - total_area_2014
area_change_percentage <- (area_change / total_area_2014) * 100

# Find buildings from 2014 that are no longer present in the OSM data
removed_buildings <- st_difference(purdue_buildings_within_boundaries, buildings_within_purdue)

# Find new buildings that were not present in 2014 but are in the OSM data
new_buildings <- st_difference(buildings_within_purdue, purdue_buildings_within_boundaries)

# Find buildings that are unchanged (intersecting in both datasets)
unchanged_buildings <- st_intersection(purdue_buildings_within_boundaries, buildings_within_purdue)

# Calculate the number of buildings
number_removed_buildings <- nrow(removed_buildings)
number_new_buildings <- nrow(new_buildings)
number_unchanged_buildings <- nrow(unchanged_buildings)

# Report the results
cat("Total area of Purdue buildings in 2014 (sq meters):", total_area_2014, "\n")
cat("Total area of current OSM Purdue buildings (sq meters):", total_area_current, "\n")
cat("Area change (sq meters):", area_change, "\n")
cat("Percentage change in total area:", area_change_percentage, "%\n")
cat("Number of buildings removed since 2014:", number_removed_buildings, "\n")
cat("Number of new buildings since 2014:", number_new_buildings, "\n")
cat("Number of buildings unchanged since 2014:", number_unchanged_buildings, "\n")


# Ensure tmap is in view mode for interactive mapping
tmap_mode("view")

# Start the map with the boundaries of Purdue University
tm_map <- tm_shape(purdue_boundaries_polygons) +
  tm_borders(col = "black") +
  tm_layout(main.title = "Comparison of Purdue University Buildings: 2014 vs Current OSM")

# Add the 2014 buildings in green
tm_map <- tm_map + tm_shape(purdue_buildings_within_boundaries) +
  tm_polygons(col = "green", alpha = 0.5, border.col = "green", title = "2014 Buildings")

# Add the current OSM buildings in blue
tm_map <- tm_map + tm_shape(buildings_within_purdue) +
  tm_polygons(col = "blue", alpha = 0.5, border.col = "blue", title = "Current OSM Buildings")

# Print the map
print(tm_map)


#    Task 04 own interest. 
# Convert bbox to an sf polygon
bbox_polygon <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]), crs = st_crs(4326)))

# Calculate the area of the bounding box in square kilometers
bbox_area_km2 <- st_area(bbox_polygon) / 1e6

# Now, proceed with the density calculation
# Extract parking lot polygons
parking_lots_sf <- wl_parking_sf$osm_polygons

# Count the number of parking lots
num_parking_lots <- nrow(parking_lots_sf)

# Calculate the density of parking lots per square kilometer
parking_density_per_km2 <- num_parking_lots / bbox_area_km2

# Output the result
print(paste("Density of parking lots: ", parking_density_per_km2, " per square kilometer"))



# Define the buffer distance (100 meters)
buffer_distance <- 100

# Create buffers around parking lots
parking_buffers <- st_buffer(wl_parking_sf_projected, dist = buffer_distance)

# Check the result
print(parking_buffers)

# Visualize the parking lots and their buffers
tm_shape(wl_parking_sf_projected) +
  tm_borders(col = "red") +
  tm_shape(parking_buffers) +
  tm_borders(col = "blue") +
  tm_layout(main.title = "Parking Lot Buffers (100 meters)")


# Centroid
parking_sf <- wl_parking_sf$osm_polygons  # Adjust this if needed

# Calculate centroids and add as columns
parking_sf$Centroid_X <- st_coordinates(st_centroid(parking_sf))[, 1]
parking_sf$Centroid_Y <- st_coordinates(st_centroid(parking_sf))[, 2]

# Create an sf object for centroids
centroids_sf <- st_as_sf(parking_sf, coords = c("Centroid_X", "Centroid_Y"), crs = st_crs(parking_sf))

# Plotting the centroids
tmap_mode("view")
tm <- tm_shape(centroids_sf) +
  tm_dots(size = 0.05, col = "blue") +
  tm_layout(title = "Parking Lot Centroids")

print(tm)

# Create a map with parking lot centroids and restaurant points
tm_map <- tm_shape(centroids_sf) +
  tm_dots(size = 0.05, col = "blue", title = "Parking Centroids") +
  tm_shape(wl_restaurants_sf$osm_points) +
  tm_dots(size = 0.05, col = "red", title = "Restaurants") +
  tm_layout(main.title = "West Lafayette: Parking Lots and Restaurants")

print(tm_map)


# Create a map with parking lot centroids, restaurant points, and roads
tm_map <- tm_shape(wl_rds_sf$osm_lines) +
  tm_lines(col = "black") +
  tm_shape(centroids_sf) +
  tm_dots(size = 0.05, col = "blue") +
  tm_shape(wl_restaurants_sf$osm_points) +
  tm_dots(size = 0.05, col = "red") +
  tm_layout(main.title = "West Lafayette: Roads, Parking Lots, and Restaurants") +
  tm_legend(title = "Legend")

print(tm_map)


# Set tmap to view mode for interactive plotting
tmap_mode("view")

# Create a map with parking lot polygons, restaurant points, and roads
tm_map <- tm_shape(wl_rds_sf$osm_lines) +
  tm_lines(col = "black") +
  tm_shape(wl_parking_sf$osm_polygons) +
  tm_polygons(col = "blue", alpha = 0.5, border.col = "blue") +
  tm_shape(wl_restaurants_sf$osm_points) +
  tm_dots(size = 0.05, col = "red") +
  tm_layout(main.title = "West Lafayette: Roads, Parking Lots, and Restaurants") +
  tm_legend(title = "Legend")

print(tm_map)
