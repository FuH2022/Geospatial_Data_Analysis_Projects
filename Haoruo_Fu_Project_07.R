#project 07
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
library(corrplot)


#1
# Read the files
#Chicago boundary zip
chicago_boundary_zip_sf <- st_read("./Data/Boundaries - ZIP Codes/geo_export_ca6e3217-2ab0-4289-8936-c9961e0616d0.shp")

# Read csv files cases
cases <- read.csv ("./data/COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")

# Read Chicago Demographic
demographics <- read.csv ("./data/Chicago_Demographics.csv")



# Merge case and Chicago boundary zip
chicago_boundary_zip_df <- as.data.frame(chicago_boundary_zip_sf)

# Rename the 'ZIP.Code' to 'zip'
names(cases)[names(cases) == 'ZIP.Code'] <- 'zip'

# Merge data shapefile and cases
merged_data <- merge(chicago_boundary_zip_sf, cases, by = 'zip')

# Merge the merged_data with the demographics data frame
final_merged_data <- merge(merged_data, demographics, by = 'zip')
final_merged_data <- st_transform(final_merged_data, 32616)
str(final_merged_data)


# Select data for plotting
plot_data <- final_merged_data[c('zip', 'Tests...Weekly', 'popden', 'MAge', 'MHI', 'geometry')]


# Set the tmap mode to plotting
tmap_mode("plot")

# Create the thematic maps
tests_map <- tm_shape(selected_data) +
  tm_polygons("Tests...Weekly", style = "equal", n = 5) +
  tm_layout(panel.labels = "Tests Weekly", legend.outside = TRUE)

popden_map <- tm_shape(selected_data) +
  tm_polygons("popden", style = "equal", n = 5) +
  tm_layout(panel.labels = "Population Density", legend.outside = TRUE)

mage_map <- tm_shape(selected_data) +
  tm_polygons("MAge", style = "equal", n = 5) +
  tm_layout(panel.labels = "Medeian Age", legend.outside = TRUE)

mhi_map <- tm_shape(selected_data) +
  tm_polygons("MHI", style = "equal", n = 5) +
  tm_layout(panel.labels = "Mean Household Income", legend.outside = TRUE)

tests_map
popden_map
mage_map
mhi_map


# Arrange the maps in a grid
tmap_arrange(tests_map, popden_map, mage_map, mhi_map)




# Select data Test Weekly for y and Population Density, Mean age, Mean Household income (MHI), for x1,x2,x3

#correlation matrix for xi's 
selected_data <- final_merged_data[c('Tests...Weekly', 'popden', 'MAge', 'MHI')]
str(selected_data)

selected_data_df <- as.data.frame(selected_data)
selected_data_df$geometry <- NULL  # Remove geometry column for correlation

colnames(selected_data_df) <- c('Test/Week', 'PopDensity', 'MedAge', 'MeanHouseIncome')


#           Correlation Matrix
# Remove any non-numeric columns like 'geometry'
selected_data_df$geometry <- NULL

# Use the `cor` function to calculate the correlation matrix for the numeric columns
correlation_matrix <- cor(selected_data_df, use = "complete.obs")  # use "complete.obs" to handle missing values

# Visualize the correlation matrix using corrplot
corrplot::corrplot(correlation_matrix, method = "circle")




#              Animation map to show 
# Transform the data to the correct projection, if needed
final_merged_data <- st_transform(final_merged_data, 32616)

# Filter data to only include relevant columns for the animation
animation_data <- final_merged_data[c('geometry', 'zip', 'Tests...Weekly', 'Week.Number')]

# Set tmap mode to view
tmap_mode("view")

# Create the maps for each time frame
tests_maps <- tm_shape(animation_data) +
  tm_polygons("Tests...Weekly", id = "zip") +
  tm_facets(by = "Week.Number") +
  tm_layout(frame = FALSE)

# Create the animation
tmap_animation(tests_maps, filename = "Tests_Weekly_Animation.gif", delay = 250)




# split the data for 2020 and 2021
final_merged_data$Week.Start <- as.Date(final_merged_data$Week.Start, format = "%m/%d/%Y")
final_merged_data$Week.End <- as.Date(final_merged_data$Week.End, format = "%m/%d/%Y")

# Extract the year from the Week.Start or Week.End column
final_merged_data$Year <- year(final_merged_data$Week.Start)

# Split the data into two sets for 2020 and 2021
data_2020 <- filter(final_merged_data, Year == 2020)
data_2021 <- filter(final_merged_data, Year == 2021)
summary (data_2020)
summary (data_2021)



# For each subset of data, rank the weeks in order starting from 1
data_2020 <- data_2020 %>%
  arrange(Week.Start) %>%
  mutate(Week.Number = dense_rank(Week.Start))

data_2021 <- data_2021 %>%
  arrange(Week.Start) %>%
  mutate(Week.Number = dense_rank(Week.Start))

# Define a function to create the animation
create_animation <- function(data, filename) {
  # Transform the data to the correct projection, if needed
  data <- st_transform(data, 32616)
  
  # Set tmap mode to view
  tmap_mode("view")
  
  # Create the maps for each time frame
  tests_maps <- tm_shape(data) +
    tm_polygons("Tests...Weekly", id = "zip",
                style = "equal",
                palette="-RdYlBu") +
    tm_facets(by = "Week.Number") +  
    tm_layout(frame = FALSE)
  
  # Create the animation
  tmap_animation(tests_maps, filename = filename, delay = 250)
}

# Create the animations for each year
create_animation(data_2020, "Tests_Weekly_Animation_2020_1.gif")
create_animation(data_2021, "Tests_Weekly_Animation_2021_1.gif")


# Aggregate the data
aggregated_data <- final_merged_data %>%
  group_by(zip) %>%
  summarize(Total_Tests_Weekly = sum(`Tests...Weekly`, na.rm = TRUE),
            PopDensity = first(popden),
            MedAge = first(MAge),
            MeanHouseIncome = first(MHI)) %>%
  ungroup()
 

summarize (aggregated_data)


#   Test Map
aggregated_data <- st_as_sf(aggregated_data, sf_column_name = "geometry")

# Now create the thematic map
tm_shape(aggregated_data) +
  tm_polygons("Total_Tests_Weekly", title = "Total Weekly Tests") +
  tm_layout(legend.position = c("left", "bottom"))






#                 Task 02    Ordinary linerar model

# Fit the linear model
model <- lm(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome, data = aggregated_data)

# Summary of the model to view the results
summary(model)


# Extract residuals
aggregated_data$residuals <- residuals(model)



# Eesidual space distribution
tm_shape(aggregated_data) +
  tm_polygons("residuals", title = "Residuals") +
  tm_layout(legend.position = c("left", "bottom"))

# Histogram
hist(model$residuals)


# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)


# log transform
aggregated_data <- aggregated_data %>%
  mutate(Log_PopDensity = log(PopDensity + 1),
         Log_MeanAge = log(MeanAge + 1),
         Log_MeanHouseIncome = log(MeanHouseIncome + 1),
         Log_Total_Tests_Weekly = log(Total_Tests_Weekly + 1))

# Fit the linear model on the log-transformed variables
log_model <- lm(Log_Total_Tests_Weekly ~ Log_PopDensity + Log_MeanAge + Log_MeanHouseIncome, data = aggregated_data)

# Summary of the log-transformed model
summary(log_model)

# Diagnostic plots for the log-transformed model
par(mfrow = c(2, 2))
plot(log_model)





#       Task 03
# GWR

library(spgwr)

library(sp)
# Set Class to Spatial
aggregated_data_sp <- as(aggregated_data, Class = "Spatial")

# Select an appropriate bandwidth using cross-validation

bandwidth <- gwr.sel(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome, 
                     data = aggregated_data_sp, gweight = gwr.Gauss)

# Fit the GWR model using the selected bandwidth
gwr_model <- gwr(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome, 
                 data = aggregated_data_sp, bandwidth = bandwidth, gweight = gwr.Gauss, hatmatrix = TRUE, se.fit = TRUE)

# Summary of the GWR model to view the results
gwr_model
gwr_model_sf <- st_as_sf(gwr_model$SDF) #Spatial data frame
plot(gwr_model_sf)

# Create the map using 'tmap'
tm_shape(gwr_model_sf) +
  tm_polygons("gwr.e", title = "Residuals from GWR Gauss") +
  tm_layout(legend.position = c("left", "bottom"))


# Histogram
hist(gwr_model_sf$gwr.e)




#bisquare
# Replace the variable names 
bandwidth <- gwr.sel(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome, 
                     data = aggregated_data_sp, gweight = gwr.bisquare)

# Fit the GWR model using the selected bandwidth
gwr_model <- gwr(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome, 
                 data = aggregated_data_sp, bandwidth = bandwidth, gweight = gwr.bisquare, hatmatrix = TRUE, se.fit = TRUE)

# Summary of the GWR model to view the results
gwr_model
gwr_model_sf <- st_as_sf(gwr_model$SDF) #Spatial data frame
plot(gwr_model_sf)

# Create the map
tm_shape(gwr_model_sf) +
  tm_polygons("gwr.e", title = "Residuals from GWR bisquare") +
  tm_layout(legend.position = c("left", "bottom"))


# Histogram
hist(gwr_model_sf$gwr.e)



#tricube

# Replace the variable names 
bandwidth <- gwr.sel(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome, 
                     data = aggregated_data_sp, gweight = gwr.tricube)

# Fit the GWR model using the selected bandwidth
gwr_model <- gwr(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome, 
                 data = aggregated_data_sp, bandwidth = bandwidth, gweight = gwr.tricube, hatmatrix = TRUE, se.fit = TRUE)

# Summary of the GWR model to view the results
gwr_model
gwr_model_sf <- st_as_sf(gwr_model$SDF) #Spatial data frame
plot(gwr_model_sf)

# Create the map 
tm_shape(gwr_model_sf) +
  tm_polygons("gwr.e", title = "Residuals from GWR tricube") +
  tm_layout(legend.position = c("left", "bottom"))


# Histogram
hist(gwr_model_sf$gwr.e)





#    Task 4        GWR with other R packages


library(GWmodel)

tmap_mode("view")

# WKT geometry
aggregated_data_sf <- st_as_sf(aggregated_data, wkt = "geometry")


# Convert 'sf' object to 'SpatialPointsDataFrame' for 'GWmodel'
aggregated_data_sp <- as(aggregated_data_sf, "Spatial")


# Gaussian Kernel
# Define bandwidth
bandwidth_gauss <- bw.gwr(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome,
                          data = aggregated_data_sp,
                          approach = "AICc",
                          kernel = "gaussian",
                          adaptive = FALSE)

# Fit the GWR model
results_gauss <- gwr.basic(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome,
                           data = aggregated_data_sp, bw = bandwidth_gauss, adaptive = FALSE)

print(results_gauss)


# Convert the GWR results to an sf object for mapping
results_sf_gauss <- st_as_sf(results_gauss$SDF)

# Create the map for Gaussian kernel
tm_shape(results_sf_gauss) +
  tm_polygons("residual", title = "Residuals from GWR Gaussian") +
  tm_layout(main.title = "Map of GWR Gaussian Residuals", main.title.position = "center")

# Histogram of GWR residuals for Gaussian kernel
hist(results_sf_gauss$residual, main = "Histogram of GWR Residuals (Gaussian)", xlab = "Residuals")


# Bisquare Kernel
# Define bandwidth
bandwidth_bisquare <- bw.gwr(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome,
                             data = aggregated_data_sp,
                             approach = "AICc",
                             kernel = "bisquare",
                             adaptive = FALSE)

# Fit the GWR model
results_bisquare <- gwr.basic(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome,
                              data = aggregated_data_sp, bw = bandwidth_bisquare, adaptive = FALSE)


print(results_bisquare)

# Convert the GWR results to an sf object for mapping
results_sf_bisquare <- st_as_sf(results_bisquare$SDF)

# Create the map for Bisquare kernel
tm_shape(results_sf_bisquare) +
  tm_polygons("residual", title = "Residuals from GWR Bisquare") +
  tm_layout(main.title = "Map of GWR Bisquare Residuals", main.title.position = "center")

# Histogram of GWR residuals for Bisquare kernel
hist(results_sf_bisquare$residual, main = "Histogram of GWR Residuals (Bisquare)", xlab = "Residuals")



# Tricube Kernel
# Define bandwidth
bandwidth_tricube <- bw.gwr(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome,
                            data = aggregated_data_sp,
                            approach = "AICc",
                            kernel = "tricube",
                            adaptive = FALSE)

# Fit the GWR model
results_tricube <- gwr.basic(Total_Tests_Weekly ~ PopDensity + MedAge + MeanHouseIncome,
                             data = aggregated_data_sp, bw = bandwidth_tricube, adaptive = FALSE)

print(results_tricube)

# Convert the GWR results to an sf object for mapping
results_sf_tricube <- st_as_sf(results_tricube$SDF)

# Create the map for Tricube kernel
tm_shape(results_sf_tricube) +
  tm_polygons("residual", title = "Residuals from GWR Tricube") +
  tm_layout(main.title = "Map of GWR Tricube Residuals", main.title.position = "center")

# Histogram of GWR residuals for Tricube kernel
hist(results_sf_tricube$residual, main = "Histogram of GWR Residuals (Tricube)", xlab = "Residuals")










