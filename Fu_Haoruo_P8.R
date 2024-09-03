
# Project 08

library(sf)
library(sp)
library(tmap)
library(gstat)
library(rgdal)
library(raster)
library(spData)
library(ggplot2)
library(spatstat)
library(viridis)
library(remotes)
library(deldir)
library(rgl)
library("USAboundaries")



# Task 01 

#     Check Random Sample Points

# Load the sample data shapefile
data <- st_read("./Data/random samples/samples10000/sample10000h.shp")

# Transform the coordinate system to EPSG:26916
data <- st_transform(data, crs = "EPSG:26916")

# Calculate x and y coordinates of the points
data$x <- coordinates(st_as_sf(data))[,1]
data$y <- coordinates(st_as_sf(data))[,2]

# Exclude outliers where the RASTERVALU is -9999
data <- data[(data$RASTERVALU > -9999),]

# Convert to SpatialPointsDataFrame 
data_sp <- as(data, "Spatial")

# Load the map of Indiana, transform it, and plot it
indiana_map <- us_states(states = "IN", resolution = "high", cb = TRUE)
indiana_map <- st_transform(indiana_map, crs = "EPSG:26916")

# Plot the data on the map
tmap_mode("plot")
tm_shape(indiana_map) +
  tm_borders() +
  tm_shape(data_sp) +
  tm_dots(col = "RASTERVALU", palette = "viridis", 
          title = "Elevation (m)", size = 0.05) +
  tm_legend(panel.labels = "Indiana Elevation", legend.outside = TRUE)

# Create Thiessen polygons using dirichlet from deldir package
th_fit <- deldir(data$x, data$y)
w <- tile.list(th_fit)
polys <- vector(mode = "list", length = length(w))
for (i in seq(w)) {
  p <- w[[i]]
  polys[[i]] <- st_polygon(list(matrix(c(p$x, p$y), ncol = 2, byrow = TRUE)))
}
th_polygons <- st_sfc(polys, crs = st_crs(data))

# Interpolate RASTERVALU within each Thiessen polygon
th_dem <- raster::intersect(th_polygons, data_sp)

# Plot the interpolation result
tm_shape(st_as_sf(th_dem)) +
  tm_polygons(col = "RASTERVALU", palette = "viridis",
              title = "Predicted Elevation (m)") +
  tm_shape(data) +
  tm_dots(col = "lightgray", size = 0.05) +
  tm_legend(panel.labels = "Thiessen Polygon Interpolation", legend.outside = TRUE)



#  Triangulation

x <- coordinates(th_dem)[, 1]
y <- coordinates(th_dem)[, 2]
z <- th_dem@data$RASTERVALU

# Create a color mapping based on the z values (elevation)
col <- heat.colors(20)[1 + round(19*(z - min(z))/diff(range(z)))]

# Perform Delaunay triangulation using deldir
dxyz <- deldir(x, y, z, rw = NULL, eps = 1e-09, sort = TRUE, plot = FALSE, round = TRUE, digits = 6)

# Create a 3D perspective plot using rgl
open3d()
persp3d(dxyz, col = col, coords = c("z", "x", "y"), smooth = FALSE)



#     Task 02
library(sp)
library(gstat)
library(raster)
library(tmap)
library(caret)
library(Metrics)

# IDW interpolation function
get_idw <- function(p, data_sp, indiana_map) {
  # Create an empty grid over the extent of 'data_sp'
  data_grid <- as.data.frame(spsample(data_sp, "regular", n = 50000))
  names(data_grid) <- c("x", "y")
  coordinates(data_grid) <- ~x+y
  gridded(data_grid) <- TRUE
  fullgrid(data_grid) <- TRUE
  proj4string(data_grid) <- proj4string(data_sp)
  
  # Perform IDW interpolation
  ids_fit <- gstat::idw(RASTERVALU ~ 1, data_sp, newdata = data_grid, idp = p)
  ids_raster <- raster(ids_fit)
  
  # Mask the interpolated raster with the Indiana map boundary
  ids_mask <- mask(ids_raster, indiana_map)
  return(ids_mask)
}

# Interpolate with different powers of p and plot
tmap_mode("plot")

# p = 1
ids_mask1 <- get_idw(1.0, data_sp, indiana_map)
tm_shape(ids_mask1) +
  tm_raster(palette = "plasma", title = "Predicted elevation (m)") +
  tm_legend(panel.labels = paste("IDW Interpolation, p = ", 1), legend.outside = TRUE)

# p = 2
ids_mask2 <- get_idw(2.0, data_sp, indiana_map)
tm_shape(ids_mask2) +
  tm_raster(palette = "plasma", title = "Predicted elevation (m)") +
  tm_legend(panel.labels = paste("IDW Interpolation, p = ", 2), legend.outside = TRUE)

# p = 3
ids_mask3 <- get_idw(3.0, data_sp, indiana_map)
tm_shape(ids_mask3) +
  tm_raster(palette = "plasma", title = "Predicted elevation (m)") +
  tm_legend(panel.labels = paste("IDW Interpolation, p = ", 3), legend.outside = TRUE)

# Cross-validation
library(Metrics)
library(lattice)
library(caret)

set.seed(123)  # Setting seed for reproducibility
folds <- createFolds(data_sp$RASTERVALU, k = 5, list = TRUE)

# Initialize minimum RMSE to a high value
minRMSE = 999999
best_idw = NULL

# Function to perform IDW and calculate RMSE for each fold
idw_CV <- function(p, folds, data_sp) {
  idw_RMSE = numeric(length(folds))
  for (i in seq_along(folds)) {
    # Split the data into training and testing sets
    train = data_sp[-folds[[i]], ]
    test = data_sp[folds[[i]], ]
    
    # Fit the IDW model using training data
    idw_fit = gstat(formula = RASTERVALU ~ 1, locations = train, set = list(idp = p))
    idw_pred = predict(idw_fit, newdata = test)$var1.pred
    
    # Calculate RMSE and compare to find the best model
    idw_RMSE[i] = rmse(test$RASTERVALU, idw_pred)
    if (idw_RMSE[i] < minRMSE) {
      best_idw = idw_fit
      minRMSE = idw_RMSE[i]
    }
  }
  
  # Return a list containing the RMSE for each fold, the best model, and the minimum RMSE
  list(idw_RMSE = idw_RMSE, bestmodel = best_idw, minRMSE = minRMSE)
}

# result
results <- idw_CV(p = 1, folds = folds, data_sp = data_sp)
mean(results$idw_RMSE)

results <- idw_CV(p = 2, folds = folds, data_sp = data_sp)
mean(results$idw_RMSE)

results <- idw_CV(p = 3, folds = folds, data_sp = data_sp)
mean(results$idw_RMSE)




#    Task   03
library(sp)
library(gstat)
library(ggplot2)
library(automap)
library(raster)

# Step 1: Compute the experimental variogram
data_kriging <- gstat(formula = RASTERVALU ~ 1, locations = data_sp)
data_v <- variogram(data_kriging)

# Plot the experimental variogram
ggplot(data_v, aes(x = dist, y = gamma)) +
  geom_point() +
  xlab("Distance") +
  ylab("Semivariance") +
  ggtitle("Variogram Plot")

# Step 2: Fit variogram models
data_v_sph <- fit.variogram(data_v, vgm("Sph"))
data_v_exp <- fit.variogram(data_v, vgm("Exp"))
data_v_gau <- fit.variogram(data_v, vgm("Gau"))

# Create lines for the fitted models
data_v_sph_line <- as.data.frame(variogramLine(data_v_sph, maxdist = 150000))
data_v_exp_line <- as.data.frame(variogramLine(data_v_exp, maxdist = 150000))
data_v_gau_line <- as.data.frame(variogramLine(data_v_gau, maxdist = 150000))

# Plot the fitted models on the variogram
ggplot(data_v, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_line(data = data_v_sph_line, aes(x = dist, y = gamma, color = "Spherical")) +
  geom_line(data = data_v_exp_line, aes(x = dist, y = gamma, color = "Exponential")) +
  geom_line(data = data_v_gau_line, aes(x = dist, y = gamma, color = "Gaussian")) +
  xlab("Distance") +
  ylab("Semivariance") +
  ggtitle("Fitted Variogram Models") +
  scale_color_manual(values = c("Spherical" = "blue", "Exponential" = "green", "Gaussian" = "red")) +
  theme(legend.title = element_blank())

# Step 3: Perform automatic kriging
auto_fit <- autoKrige(RASTERVALU ~ 1, data_sp)

# Extract the automatically fitted variogram model
auto_model <- auto_fit$var_model
auto_variogram_line <- as.data.frame(variogramLine(auto_model, maxdist = 150000))

# Plot the automatic model with the experimental variogram and fitted models
ggplot(data_v, aes(x = dist, y = gamma)) +
  geom_point() +
  geom_line(data = data_v_sph_line, aes(x = dist, y = gamma, color = "Spherical")) +
  geom_line(data = data_v_exp_line, aes(x = dist, y = gamma, color = "Exponential")) +
  geom_line(data = data_v_gau_line, aes(x = dist, y = gamma, color = "Gaussian")) +
  geom_line(data = auto_variogram_line, aes(x = dist, y = gamma, color = "Automatic")) +
  xlab("Distance") +
  ylab("Semivariance") +
  ggtitle("Variogram Model Comparison") +
  scale_color_manual(values = c("Spherical" = "blue", "Exponential" = "green", "Gaussian" = "red", "Automatic" = "black")) +
  theme(legend.title = element_blank())

# Step 4: Mask the kriging output with the Indiana map boundary and convert to sf object
autokrig_raster <- raster(auto_fit$krige_output)
autokrig_mask <- mask(autokrig_raster, indiana_map)
kriging_values <- rasterToPoints(autokrig_mask, na.rm = TRUE)
kriging_sf <- st_as_sf(data.frame(kriging_values), coords = c("x", "y"), crs = crs(autokrig_mask))

# Step 5: Plot the kriging result using tmap
tmap_mode("view")
tm_map <- tm_shape(autokrig_mask) +
  tm_raster(palette = "plasma", title = "Predicted elevation (m)") +
  tm_layout(legend.width = 0.3) +
  tm_legend(panel.labels = "Automatic Kriging Interpolation", legend.outside = TRUE)

# View the map
tm_map

# Step 6: Cross-validation
set.seed(123)  # for reproducibility
folds <- createFolds(data_sp$RASTERVALU, k = 5, list = TRUE)

# Initialize a vector to store the RMSE for each fold
kriging_RMSE <- numeric(length(folds))

# Perform cross-validation
for(i in seq_along(folds)) {
  # Split data into training and testing sets based on the folds
  train_data <- data_sp[-folds[[i]], ]
  test_data <- data_sp[folds[[i]], ]
  
  # Fit the variogram model to the training data
  kriging_model <- gstat(formula = RASTERVALU ~ 1, data = train_data, model = auto_model)
  
  # Predict using the kriging model on the testing set
  predictions <- predict(kriging_model, newdata = test_data)
  
  # Calculate RMSE for the current fold and store it
  kriging_RMSE[i] <- sqrt(mean((test_data$RASTERVALU - predictions$var1.pred)^2, na.rm = TRUE))
}

# Output the RMSE for each fold
kriging_RMSE


#   Task 04
#    Option 01: Thin Spine Interpolation
library(fields)
library(raster)
library(sp)
library(tmap)

data_df <- as.data.frame(data_sp)
complete_data <- data_df[complete.cases(data_df), ]
head(complete_data)
grid_new <- expand.grid(x = seq(min(complete_data$coords.x1), max(complete_data$coords.x1), length = 100),
                        y = seq(min(complete_data$coords.x2), max(complete_data$coords.x2), length = 100))

# Fit a Thin Plate Spline (TPS) model
tps_model <- Tps(xyz = complete_data[, c("coords.x1", "coords.x2")], Z = complete_data$RASTERVALU)

# Interpolate the values on the grid
predicted_surface <- predict(tps_model, grid_new)

# Convert the result to a RasterLayer
tps_raster <- rasterFromXYZ(cbind(grid_new, z = predicted_surface))

# Assign the CRS to match original spatial data
crs(tps_raster) <- crs(data_sp)

# Mask the raster with the Indiana map boundary
tps_mask <- mask(tps_raster, indiana_map)

# Plot the results
tmap_mode("plot")
tm_shape(tps_mask) +
  tm_raster(style = "cont", palette = "plasma", title = "Predicted Elevation (m)") +
  tm_legend(legend.outside = TRUE)

# Cross-validation for Thin Plate Spline
set.seed(123) # For reproducibility
folds <- createFolds(complete_data$RASTERVALU, k = 5, list = TRUE)
tps_RMSE <- numeric(length(folds))

for(i in 1:5) {
  # Split the data into training and testing sets
  train_indices <- which(!(1:nrow(complete_data) %in% folds[[i]]))
  test_indices <- which(1:nrow(complete_data) %in% folds[[i]])
  training_set <- complete_data[train_indices, ]
  testing_set <- complete_data[test_indices, ]
  
  # Fit the TPS model on training data
  tps_model <- Tps(xyz = training_set[, c("coords.x1", "coords.x2")], Z = training_set$RASTERVALU)
  
  # Predict using the TPS model on the testing set
  predictions <- predict(tps_model, testing_set[, c("coords.x1", "coords.x2")])
  
  # Calculate RMSE for the current fold
  tps_RMSE[i] <- sqrt(mean((testing_set$RASTERVALU - predictions)^2, na.rm = TRUE))
}

# Output the RMSE for each fold
tps_RMSE


# Option 02:  KNN
library(FNN)
library(raster)
library(sp)
library(tmap)

# Convert 'data_sp' to a regular data frame
data_df <- as.data.frame(data_sp)

# Remove NA values
complete_data <- na.omit(data_df)

head(complete_data)
# Define the interpolation grid using the correct column names
grid_new <- expand.grid(x = seq(min(complete_data$coords.x1), max(complete_data$coords.x1), length = 100),
                        y = seq(min(complete_data$coords.x2), max(complete_data$coords.x2), length = 100))

# Perform KNN interpolation
k <- 5 # Number of nearest neighbors
knn_result <- knn.reg(train = complete_data[, c("coords.x1", "coords.x2")], y = complete_data$RASTERVALU,
                      test = grid_new, k = k)

# Convert the result to a RasterLayer
knn_raster <- rasterFromXYZ(cbind(grid_new, z = knn_result$pred))

# Assign the CRS to match original spatial data
crs(knn_raster) <- crs(data_sp)

# Mask the raster with the Indiana map boundary
knn_mask <- mask(knn_raster, indiana_map)

# Plot the results
tmap_mode("view")
tm_shape(knn_mask) +
  tm_raster(style = "cont", palette = "plasma", title = "Predicted Elevation (m)") +
  tm_legend(legend.outside = TRUE)

# Cross-validation for KNN
set.seed(123) # For reproducibility
folds <- createFolds(complete_data$RASTERVALU, k = 5, list = TRUE)
knn_RMSE <- numeric(length(folds))

for(i in 1:5) {
  # Split the data into training and testing sets
  train_indices <- which(!(1:nrow(complete_data) %in% folds[[i]]))
  test_indices <- which(1:nrow(complete_data) %in% folds[[i]])
  training_set <- complete_data[train_indices, ]
  testing_set <- complete_data[test_indices, ]
  
  # Perform KNN interpolation on training data
  knn_model <- knn.reg(train = training_set[, c("coords.x1", "coords.x2")], y = training_set$RASTERVALU,
                       test = testing_set[, c("coords.x1", "coords.x2")], k = k)
  
  # Calculate RMSE for the current fold
  knn_RMSE[i] <- sqrt(mean((testing_set$RASTERVALU - knn_model$pred)^2, na.rm = TRUE))
}

# Output the RMSE for each fold
knn_RMSE


library(raster)
library(sp)

# Task 05
# Load the ground truth DEM for Indiana
ground_truth <- raster("./Data/indiana dem/indiana_utm84.tif") 


dem_idw <- ids_mask3
dem_kriging <- autokrig_raster
dem_knn <- knn_mask # Make sure this is the raster result from  KNN interpolation

# Resample the interpolated DEMs to match the ground truth resolution
dem_idw_resampled <- resample(dem_idw, ground_truth, method = 'bilinear')
dem_kriging_resampled <- resample(dem_kriging, ground_truth, method = 'bilinear')
dem_knn_resampled <- resample(dem_knn, ground_truth, method = 'bilinear')

# Calculate the differences between the interpolated DEMs and the ground truth
diff_idw <- ground_truth - dem_idw_resampled
diff_kriging <- ground_truth - dem_kriging_resampled
diff_knn <- ground_truth - dem_knn_resampled

# Plot histograms of the differences
hist(diff_idw, main = 'IDW Differences', xlab = 'Elevation difference (m)')
hist(diff_kriging, main = 'Kriging Differences', xlab = 'Elevation difference (m)')
hist(diff_knn, main = 'KNN Differences', xlab = 'Elevation difference (m)')

# Visualize the spatial distribution of the differences
plot(diff_idw, main = 'Spatial Distribution of IDW Differences')
plot(diff_kriging, main = 'Spatial Distribution of Kriging Differences')
plot(diff_knn, main = 'Spatial Distribution of KNN Differences')




#   BONUS
# Step 1: Read the new sample data and clean it
data_2 <- st_read("./Data/random samples/samples20000/sample20000h.shp")
data_2 <- st_transform(data_2, crs = "EPSG:26916")
data_2$x <- st_coordinates(data_2)[, 1]
data_2$y <- st_coordinates(data_2)[, 2]
data_2 <- data_2[!is.na(data_2$RASTERVALU) & data_2$RASTERVALU > -9999, ] # exclude outliers

# Ensure SpatialPointsDataFrame
data_2_sp <- as(data_2, "Spatial")


# IDW interpolation function that can be used for different values of p
get_idw_2 <- function(p){
  data_2_grid <- as.data.frame(spsample(data_2_sp, "regular", n = 50000))
  names(data_2_grid) <- c("x", "y")
  coordinates(data_2_grid) <- ~x+y
  gridded(data_2_grid) <- TRUE
  fullgrid(data_2_grid) <- TRUE
  proj4string(data_2_sp) <- CRS(proj4string(data_2_sp))
  proj4string(data_2_grid) <- CRS(proj4string(data_2_sp))
  
  ids_fit <- gstat::idw(formula = RASTERVALU ~ 1, data_2_sp, newdata = data_2_grid, idp = p)
  ids_raster <- raster(ids_fit)
  ids_mask <- mask(ids_raster, indiana_map)
  return(ids_mask)
}

# Apply IDW interpolation for p = 1, 2, and 3
ids_mask1_2 <- get_idw_2(1.0)
ids_mask2_2 <- get_idw_2(2.0)
ids_mask3_2 <- get_idw_2(3.0)

# Cross-validation setup
set.seed(123)  # for reproducibility
folds <- createFolds(data_2_sp$RASTERVALU, k = 5, list = TRUE)
minRMSE <- 999999  # Initialize high value for RMSE
best_idw <- NULL  # Placeholder for the best IDW model

# Function to perform IDW and calculate RMSE for each fold
idw_CV <- function(p){
  idw_RMSE <- numeric(length(folds))
  for(i in seq_along(folds)){
    # Split the data into training and testing sets
    train <- data_2_sp[-folds[[i]], ]
    test <- data_2_sp[folds[[i]], ]
    
    # Fit the IDW model using training data
    idw_fit <- gstat(formula = RASTERVALU ~ 1, locations = train, set = list(idp = p))
    idw_pred <- predict(idw_fit, newdata = test)
    
    # Calculate RMSE and compare to find the best model
    idw_RMSE[i] <- rmse(test$RASTERVALU, idw_pred$var1.pred)
    if(idw_RMSE[i] < minRMSE){
      best_idw <- idw_fit
      minRMSE <- idw_RMSE[i]
    }
  }
  list(idw_RMSE = idw_RMSE, bestmodel = best_idw, minRMSE = minRMSE)
}

# Calculate IDW interpolation RMSE for different values of p
results_p1 <- idw_CV(1)
results_p2 <- idw_CV(2)
results_p3 <- idw_CV(3)

# Compare with the ground truth DEM
# Resample the IDW rasters to match the ground truth resolution
dem_idw_resampled_2 <- resample(ids_mask3_2, ground_truth, method = 'bilinear')

# Calculate differences
diff_idw_2 <- ground_truth - dem_idw_resampled_2


# Plot histograms and spatial distribution of differences
hist(diff_idw_2, main = 'IDW Differences (sample20000)', xlab = 'Elevation difference (m)')
plot(diff_idw_2, main = 'Spatial Distribution of IDW Differences (sample20000)')
