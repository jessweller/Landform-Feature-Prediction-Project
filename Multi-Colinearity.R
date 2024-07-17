################################################################################
######################### Load environmental attributes ######################## 
# Load package for raster stack
library(terra)

# Load package for multicolinearity test (VIF algorithm)
library(usdm) 

# Load package to save correlation matrix as an excel workbook
library(openxlsx) 

# Read path (use $MYSCRATCH when on kaya)
path <- ("C:/Users/22481051/OneDrive - The University of Western Australia/PhD/Thesis Chapters/Chapter 4 + 5 - Regolith Depth Prediction/(a)Landform-Classification/Landform-Feature-Prediction-Project/Data")

# Read multiple single-layer datasets of same extent and resolution to a multi-layer SpatRaster
files_list <- list.files(path, pattern = '.tif$', 
                         all.files = T, full.names = T)

files_list

# Create a SpatRaster with all raster files (opens connection to tif files - reduces RAM)
(
  
  multi_layer_SpatRaster <- rast(files_list)
  
)

multi_layer_SpatRaster

## Plot the first layer of the stacked raster (optional)
# plot(multi_layer_SpatRaster[[1]])

## Check the names of the layers in the 'raster_stack' (optional)
# layer_names <- names(multi_layer_SpatRaster)
# print(layer_names)

################################################################################
######################## (1) Multicollinearity Analysis ########################
# Load package for findCorrelation
library(caret)

# Calculate vif for the variables in multi_layer_SpatRaster
vif(multi_layer_SpatRaster)

# Identify collinear variables that should be excluded
vifcor_results <- vifcor(multi_layer_SpatRaster, th = 0.9) # th = threshold for correlation coefficient

vifcor_results@results

# Access the correlation matrix from vifcor results
correlation_matrix <- vifcor_results@corMatrix

# Find attributes that are highly correlated (ideally > 0.90)
highly_correlated <- findCorrelation(correlation_matrix, cutoff = 0.9)

# Print indexes of highly correlated attributes
print(highly_correlated)

# Remove correlated variables from multi_layer_SpatRaster (only complete if there are highly correlated variables)
multi_layer_SpatRaster_clean <- multi_layer_SpatRaster[-highly_correlated]


################################################################################
######################## (3) Automated K-means Clustering ######################
# Load required packages
library(stats) 
library(factoextra) # to create graph of clusters generated with the kmeans() function
library(dplyr) # %>% function

# Extract raster values
raster_data <- as.data.frame(multi_layer_SpatRaster_clean, xy = TRUE) # converts raster layer into an array (matrix)
str(raster_data)

# Preprocess the data
data <- na.omit(raster_data)
data_scaled <- scale(data[ , -c(1, 2)]) # Assuming first two columns are x and y coordinates
head(data, n = 5) # View the first 5 rows

# Estimating the optimal number of clusters using the Elbow method
fviz_nbclust(data,
  kmeans,
  # wss = within sum of squares
  method = "wss"
) + geom_vline(xintercept = 8, linetype = 2)

## Alternative method to "wss" is average silhouette for kmeans (optional to compare)
# fviz_nbclust(data,
#             kmeans,
#             method = "silhouette") + geom_vline(xintercept = 8, linetype = 2)

# Computing K-means clustering with optimal number of clusters (change centres)
set.seed(2056)
optimal_kmncluster <- kmeans(data, centres = 4, nstart = 100) # or nstart = 50 for stable results
print(optimal_kmncluster)
optimal_kmncluster$cluster # A vector of integers (from 1:k) indicating the cluster to which each point is allocated
optimal_kmncluster$size # The number of observations in each cluster
optimal_kmncluster$centers # A matrix of cluster centers (cluster means)

# Visualising k-means clusters
fviz_cluster(optimal_kmncluster, data = data,
             # Change number of palettes to match number of clusters
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
             )

################################# Plot Results #################################
# Replace values in raster_data_s with their cluster membership and put these values into a RasterLayer
library(rgdal) # writing GeoTIFF Files

## Alternative method if below does not work
# raster_data_s[i] <- optimal_kmncluster$cluster
# kmn_raster <- setValues(raster_stack, raster_data_s)
# plot(kmn_raster)


# Create a new RasterLayer for the clustering results using the first layer of the stack as a template
# Extract cluster information
kmncluster_raster <- raster_stack[[1]]  # Use the first layer as a template
kmncluster_raster[] <- optimal_kmncluster$cluster

# Save the raster as a GeoTIFF
writeRaster(kmncluster_raster, filename = "kmeans_clusters.tif", format = "GTiff", overwrite = TRUE)

# Plot the raster to visualize it
colors <- brewer.pal(max(optimal_kmncluster$cluster), "Set3")
plot(kkmncluster_raster, col = colors, main = "K-means Clusters")

### Hierarchical clustering (alternative to kmeans) (optional comparison)
## Compute hierarchical clustering
# res.hc <- raster_data %>%
#  scale() %>% # Scale the data
#  dist(method = "euclidean") %>% # Compute dissimilarity matrix
#  hclust(method = "ward.D2") # Compute hierarchical clustering

# Visualise using factoextra (cut in X groups and colour by groups)
# fviz_dend(res.hc, k = 4, # Cut in four groups
#          cex = 0.5, # Label size
#          k_colours = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
#          colour_labels_by_k = TRUE, # Colour labels by groups
#          rect = TRUE #Add rectangle around groups
#          )


################################################################################
############################## (4) GRTS Sampling ##############################
# Load required package for GRTS sampling approach
library(spsurvey) 
set.seed(51)

# Load the saved raster layer
kmn_raster <- raster("kmeans_clusters.tif")

# Convert the raster to a data frame
kmn_raster_df <- as.data.frame(kmn_raster, xy = TRUE)
names(kmn_raster_df)[3] <- "cluster"

# Create a SpatialPointsDataFrame
coordinates(kmn_raster_df) <- ~ x + y
proj4string(kmn_raster_df) <- CRS(proj4string(raster_stack))

# Convert to sf object
kmn_raster_sf <- st_as_sf(kmn_raster_df)

## Define the stratified design for GRTS sampling
# Example: 40 points per stratum
strata_names <- unique(kmn_raster_sf$cluster)
strata_names

# could be strata_n <- c((cluster name in kmn_raster_sf) = 40, "2" = 40, "3" = 40 etc)
strata_n <- lapply(strata_names, function(stratum){
  list(panel=c(Base = 40))
})

names(strata_n) <- strata_names

# Example parameters for grts function based on your context
grts_result <- grts(
  sframe = kmn_raster_sf,
  stratum_var = "cluster",       # Variable defining strata
  n_base = strata_n,             # Incorporate the point deisgn defined above
  seltype = "Equal",             # Equal inclusion method
  maxtry = 10,                   # Maximum number of attempts to place a point
  SiteBegin = 1,                 # Starting index for sample sites
  sep = "-",                     # Separator for site ID
  projcrs_check = TRUE           # Check for projection CRS
)

# Example: Saving the GRTS points to a CSV file
write.csv(grts_result, file = "grts_points.csv", row.names = FALSE)

# Display summary of GRTS sampling
summary(grts_result)


################################################################################
############################# (5) Model Development ############################
library(caret)  # For feature selection (findcorrelation, rfe)

# Load data with "landform" observations
new_data <- read.csv(".csv") # multi-colinear variables removed prior to inputting observations

####################### Create Data Partition for Models #######################
# Creation of the partition 75% and 25%
set.seed(1815) #provare 1234
data_sampling_index <- createDataPartition(new_data$landforms, times = 1, p= 0.75, list = FALSE)
train_data <- new_data[data_sampling_index, ]
test_data <- new_data[-data_sampling_index, ]

# Create a control to allow comparisons between models
fitControl <- trainControl(method="cv",   # Control the computational nuances of the train function
                           number = 5,    # Either the number of folds or number of resampling iterations
                           repeats = 100,  # For repeated k-fold cross-validation only: the number of complete sets of folds to compute
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

################################# Train Models #################################
### Random Forest
library(randomForest)

# Creation of Random Forest Model
rf_model <- randomForest(landforms ~ .,
                  data = train_data,
                  ntree = 1000,  # Number of trees to grow
                  maxnodes = 6,  # Maximum number of terminal nodes trees in the forest can have
                  importance = TRUE # Assess importance of predictors
                  )

rf_model

# Alternate method
# rf_model <- train(landforms ~ .,
# data = train_data,
# method = "rf",
# trControl = fitControl,
# tuneGrid = expand.grid(ntree = 1000, maxnodes = 6),
# importance = TRUE)

# Plot of top important variables
plot(varImp(rf_model), top = 10, main = "Top variables - Random Forest")
png(file = "variable_importance_plot.png", width = 800, height = 600, units = "px", res = 300)
plot(varImp(rf_model), top = 10, main = "Top variables - Random Forest")
dev.off()

# Save model for future use
saveRDS(rf_model, "my_rf_model.rds") # to open saved model use = rf2 <- readRDS("my_rf_model.rds")

### Cubist
library(Cubist) 


### Support Vector Machine
library(kernlab) 


### Artificial Neural Network
library(avNNet) # or library(nnet)
model_avnnet <- avNNet(landforms ~ ., 
                       data = train_data, 
                       repeats=10,
                       size=20, 
                       decay=0.1, 
                       linout=TRUE, 
                       maxit = 3000, 
                       allowParallel=TRUE)

# example
nnetTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "avNNet",
                  tuneGrid = nnetGrid,  # Defines a data frame specifying the set of tuning parameters to search over during model training
                  trControl = ctrl,
                  preProc = c("center", "scale"), # Specifies the pre-processing methods to apply to the data before modeling
                  linout = TRUE,  # Controls whether the output layer of the neural network should use linear activation 
                  trace = FALSE,  # Controls whether to display progress messages during model training
                  MaxNWts = 13 * (ncol(solTrainXtrans) + 1) + 13 + 1,  # Specifies the maximum number of weights in the neural network model
                  maxit = 1000,  # Specifies the maximum number of iterations for training the neural network
                  allowParallel = FALSE)  # Controls whether to allow parallel processing during model training
nnetTune



### Generalised Linear Model
library(glm2)


################################# Test Models ##################################
### Random Forest
prediction_rf <- predict(rf_model, test_data) # rf_model may change depending on whether you've read past .rds file


### Cubist



### Support Vector Machine



### Artificial Neural Network



### Generalised Linear Model



################################################################################
######################## (6) Model Accuracy / Uncertainty ######################
### Random Forest
# Confusion matrix
confusionmatrix_rf <- confusionMatrix(prediction_rf, test_data$landforms, positive = "M")
confusionmatrix_rf

### Cubist



### Support Vector Machine



### Artificial Neural Network



### Generalised Linear Model


