#install.packages("tmap")
#install.packages("pROC")
#install.packages("irr")
#install.packages("caret")
# Load required packages
library(raster)
library(pROC)
library(tmap)

# Read the coffee species occurrence data
occurrence <- read.csv("C:/Users/ADMIN/Desktop/CoffeeData.csv")
setwd("C:/Users/ADMIN/Desktop/bio_layers")

# Read the bioclimatic variables for Karnataka
bio4 <- raster("bio4.tif")
bio10 <- raster("bio10.tif")
bio2 <- raster("bio2.tif")
bio8 <- raster("bio8.tif")
bio5 <- raster("bio5.tif")
bio6 <- raster("bio6.tif")
bio12 <- raster("bio12.tif")

# Set the extent to Karnataka's extent
karnataka_extent <- extent(74, 78.5, 11.5, 18)

# Crop the bioclimatic variables to Karnataka extent
bio4_karnataka <- crop(bio4, karnataka_extent)
bio10_karnataka <- crop(bio10, karnataka_extent)
bio2_karnataka <- crop(bio2, karnataka_extent)
bio8_karnataka <- crop(bio8, karnataka_extent)
bio5_karnataka <- crop(bio5, karnataka_extent)
bio6_karnataka <- crop(bio6, karnataka_extent)
bio12_karnataka <- crop(bio12, karnataka_extent)

# Create a RasterStack object with the cropped bioclimatic variables
env_stack <- stack(bio4_karnataka, bio10_karnataka, bio2_karnataka,
                   bio8_karnataka, bio5_karnataka, bio6_karnataka, bio12_karnataka)

# Extract environmental values at occurrence points
env_data <- extract(env_stack, occurrence)

# Create presence-absence data for modeling
presence <- rep(1, nrow(env_data))

# Train the GLM model using current occurrence data and bioclimatic variables
model <- glm(presence ~ ., data = as.data.frame(env_data), family = binomial())

# Project the GLM model to future bioclimatic variables
projected_map <- predict(env_stack, model)

# Calculate the current distribution map using the trained model
current_map <- predict(env_stack, model, type = "response")


# Calculate change in distribution
change_map <- projected_map - current_map
resolution <- res(change_map)
resolution


# Convert occurrence data frame to SpatialPointsDataFrame
coordinates(occurrence) <- c("Longitude", "Latitude")
proj4string(occurrence) <- CRS("+proj=longlat +datum=WGS84")

# Plot the current distribution map with occurrence points and basemap
tm_shape(current_map) +
  tm_raster(style = "cont", palette = "Oranges", title = "Current Distribution") +
  tm_shape(occurrence) +
  tm_dots(col = "red", size = 0.6) +
  tm_basemap(server = "OpenStreetMap") +
  tm_layout(main.title = "Current Distribution of Coffee Species",
            legend.outside = TRUE, legend.position = c("left", "bottom"),
            legend.title.size = 0.8, legend.text.size = 0.7,
            legend.bg.color = "white", legend.bg.alpha = 0.8,
            legend.width = 0.5, legend.height = 0.4,
            frame = FALSE)

# Plot the projected distribution map with occurrence points and basemap
tm_shape(projected_map) +
  tm_raster(style = "cont", palette = "Blues", title = "Projected Distribution") +
  tm_shape(occurrence) +
  tm_dots(col = "red", size = 0.6) +
  tm_basemap(server = "OpenStreetMap") +
  tm_layout(main.title = "Projected Distribution of Coffee Species",
            legend.outside = TRUE, legend.position = c("left", "bottom"),
            legend.title.size = 0.8, legend.text.size = 0.7,
            legend.bg.color = "white", legend.bg.alpha = 0.8,
            legend.width = 0.5, legend.height = 0.4,
            frame = FALSE)

# Plot the change in distribution map with occurrence points and basemap
tm_shape(change_map) +
  tm_raster(style = "cont", palette = "RdYlBu", title = "Change in Distribution") +
  tm_shape(occurrence) +
  tm_dots(col = "red", size = 0.6) +
  tm_basemap(server = "OpenStreetMap") +
  tm_layout(main.title = "Change in Distribution of Coffee Species",
            legend.outside = TRUE, legend.position = c("left", "bottom"),
            legend.title.size = 0.8, legend.text.size = 0.7,
            legend.bg.color = "white", legend.bg.alpha = 0.8,
            legend.width = 0.5, legend.height = 0.4,
            frame = FALSE)

# Calculate statistics for species movement
moved_pixels <- cellStats(change_map, stat = "sum", na.rm = TRUE)  # Number of pixels where species moved
total_pixels <- ncell(change_map)  # Total number of pixels in the map
pixel_change_distance <- resolution * moved_pixels
pixel_change_distance
pixel_change_distance_meters <- pixel_change_distance * 10
pixel_change_distance_meters 



moved_percentage <- moved_pixels / total_pixels * 100  # Percentage of movement

# Print the statistics
cat("Number of pixels where species moved:", moved_pixels, "\n")
cat("Percentage of movement:", moved_percentage, "%\n")
cat("Pixel change in distance:", pixel_change_distance, "\n")


# Save the plots as separate files with increased size
png("current_distribution.png", width = 10, height = 10, units = "in", res = 300)
plot(current_map, main = "Current Distribution of Coffee Species")
dev.off()

png("projected_distribution.png", width = 10, height = 10, units = "in", res = 300)
plot(projected_map, main = "Projected Distribution of Coffee Species")
dev.off()

png("change_in_distribution.png", width = 10, height = 10, units = "in", res = 300)
plot(change_map, main = "Change in Distribution of Coffee Species")
dev.off()

# Calculate model performance metrics
threshold <- 0.5  # Choose a threshold for classification
predicted_probs <- predict(model, newdata = as.data.frame(env_data), type = "response")

# Convert predicted probabilities to binary predictions based on the threshold
predicted <- ifelse(predicted_probs >= threshold, 1, 0)

# Calculate true positive, true negative, false positive, false negative
tp <- sum(predicted == 1 & presence == 1)
tn <- sum(predicted == 0 & presence == 0)
fp <- sum(predicted == 1 & presence == 0)
fn <- sum(predicted == 0 & presence == 1)

# Calculate metrics
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
precision <- tp / (tp + fp)
recall <- sensitivity
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the metrics
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")




