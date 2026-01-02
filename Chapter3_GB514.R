# =====================================================
# Blood Pressure Prediction Using Socioeconomic Factors
# -----------------------------------------------------
# Goal: Identify which socioeconomic factors influence systolic blood pressure
# =====================================================

# Load Required Libraries
library(tidyverse)    # Data wrangling and visualization
library(ggcorrplot)   # Correlation matrix visualization
library(DescTools)    # Winsorization (handling outliers)
library(cluster)      # Clustering analysis
library(factoextra)   # Visualizing clustering results
library(rpart)        # Decision tree classification
library(rpart.plot)   # Visualizing decision trees
library(caret)        # Model evaluation (RMSE, accuracy)
library(randomForest) # Random Forest regression 


# =====================================================
# Step 1: Load and Explore the Data
# =====================================================

# Set working directory (Change this to your actual directory)
setwd("C://Users//Gbrai//OneDrive - University of Kent//STAGE 2//MAST5956//Group Project")

# Load dataset
orgdata <- read.csv("IntOrg_NCD_variables_2024_02_02.csv")

# Check for missing values in each column
missing_per_column <- colSums(is.na(orgdata))
print(missing_per_column)  # Prints the count of missing values per column

# Remove an unnecessary column ("Diabetes_prevalence"), as it is not required
orgdata1 <- orgdata[, !names(orgdata) %in% c("Diabetes_prevalence")]

# Remove rows where the row name is "2016" (possibly unnecessary data)
orgdata2 <- orgdata1[!(rownames(orgdata1) %in% "2016"), ]

# Detect and count duplicate rows in the dataset
sum(duplicated(orgdata))

# =====================================================
# Step 2: Handle Outliers Using IQR Method
# =====================================================

# Function to remove outliers using the Interquartile Range (IQR) method
remove_outliers <- function(df, cols) {
  for (col in cols) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    # Define lower and upper bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Remove rows where values fall outside the defined bounds
    df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  }
  return(df)
}

# Select only numeric columns for outlier removal
numeric_cols <- names(orgdata)[sapply(orgdata, is.numeric)]

# Apply outlier removal function to the dataset
orgdata_cleaned <- remove_outliers(orgdata, numeric_cols)

# =====================================================
# Step 3: Exploratory Data Analysis (EDA)
# =====================================================

# Compute correlation matrix for blood pressure and socioeconomic factors
cor_matrix <- cor(orgdata[, c("Systolic_blood_pressure", "Years_of_education", 
                              "Urbanisation", "Western_diet_score", "GDP_USD")], 
                  use="complete.obs")

# Visualize correlation matrix
ggcorrplot(cor_matrix, method="circle")

# Histogram: Distribution of key variables
orgdata_cleaned %>%
  select(Systolic_blood_pressure, Years_of_education, Urbanisation, Western_diet_score, GDP_USD) %>%
  gather(variable, value) %>%
  ggplot(aes(x=value, fill=variable)) + 
  geom_histogram(bins=30, alpha=0.6) +
  facet_wrap(~variable, scales="free") +
  theme_minimal() +
  labs(title="Distribution of Key Variables", x="Value", y="Count")

# =====================================================
# Step 4: Feature Transformation
# =====================================================

# Remove missing values in GDP before proceeding
orgdata_cleaned <- orgdata_cleaned[!is.na(orgdata_cleaned$GDP_USD), ]

# Winsorization: Reducing impact of extreme outliers in GDP
orgdata_cleaned$GDP_USD <- Winsorize(orgdata_cleaned$GDP_USD, probs = c(0.05, 0.95))

# Normalize Urbanisation using Z-score scaling
orgdata_cleaned$Urbanisation <- scale(orgdata_cleaned$Urbanisation)

# Log transformation for GDP_USD to handle skewed distribution
orgdata_cleaned$GDP_USD <- log1p(orgdata_cleaned$GDP_USD)

# =====================================================
# Step 5: Clustering (K-Means)
# =====================================================

# Perform K-Means clustering on Urbanisation and GDP
cluster_data <- orgdata_cleaned %>%
  select(Urbanisation, GDP_USD) %>%
  scale()

# Determine optimal number of clusters using the Elbow Method
fviz_nbclust(cluster_data, kmeans, method = "wss")

# Apply K-Means clustering with 3 clusters
set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 3, nstart = 25)

# Add cluster labels to the dataset
orgdata_cleaned$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clustering results
fviz_cluster(kmeans_result, data = cluster_data, geom = "point", ellipse = TRUE) +
  labs(title = "K-Means Clustering of Urbanisation and GDP")

# =====================================================
# Compute the Average Systolic Blood Pressure for Each Cluster
# =====================================================

# The 'aggregate' function groups the data by 'Cluster' 
# and computes the mean Systolic Blood Pressure for each group.
aggregate(orgdata_cleaned$Systolic_blood_pressure, 
          by = list(orgdata_cleaned$Cluster),  # Group by Cluster
          FUN = mean)  # Calculate the mean BP for each cluster


# Create a boxplot to compare the distribution of Systolic Blood Pressure 
# across different Clusters
ggplot(orgdata_cleaned, aes(x = Cluster,  # X-axis represents clusters
                            y = Systolic_blood_pressure,  # Y-axis represents BP
                            fill = Cluster)) +  # Fill color based on cluster group
  geom_boxplot() +  # Generate a boxplot
  labs(title = "Blood Pressure by Cluster",  # Set the title of the plot
       x = "Cluster",  # Label for X-axis
       y = "Systolic BP")  # Label for Y-axis


# =====================================================
# Step 6: Classification (Decision Tree)
# =====================================================

# Categorize blood pressure into Low, Normal, and High
orgdata_cleaned$BP_Category <- cut(orgdata_cleaned$Systolic_blood_pressure, 
                                   breaks = c(-Inf, 120, 140, Inf), 
                                   labels = c("Low", "Normal", "High"))

# Train a Decision Tree model to classify blood pressure categories
bp_model <- rpart(BP_Category ~ Urbanisation + GDP_USD + Cluster, data = orgdata_cleaned, method = "class")

# Visualize the Decision Tree
rpart.plot(bp_model, type = 4, extra = 101)

# =====================================================
# Step 7: Regression (Linear Model & Random Forest)
# =====================================================

# Train a linear regression model to predict systolic blood pressure
model <- lm(Systolic_blood_pressure ~ Urbanisation + GDP_USD + Cluster, data = orgdata_cleaned)
summary(model)

# Train a Random Forest model for regression
rf_model <- randomForest(Systolic_blood_pressure ~ Urbanisation + GDP_USD + Cluster, 
                         data = orgdata_cleaned, ntree = 100)

# Predict blood pressure using Random Forest
rf_predictions <- predict(rf_model, newdata = orgdata_cleaned)

# Compute RMSE for Random Forest regression model
rf_rmse <- RMSE(rf_predictions, orgdata_cleaned$Systolic_blood_pressure)
print(paste("Random Forest RMSE:", rf_rmse))



