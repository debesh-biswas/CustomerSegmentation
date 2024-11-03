install.packages('tidyverse')
install.packages('DT')
install.packages('GGally')
install.packages('RColorBrewer')
install.packages('ggplot2')
install.packages('ggforce')
install.packages('concaveman')
install.packages('factoextra')
install.packages('FactoMineR')

library(tidyverse)
library(DT)
library(GGally)
library(RColorBrewer)
library(ggplot2)
library(ggforce)
library(concaveman)
library(factoextra)
library(FactoMineR)

# Load dataset
data <- read.csv("Dataset.csv")

# Display data table with scroll
datatable(data, options = list(scrollX = TRUE))
str(data)

# Calculate missing value percentage
colSums(is.na(data)/nrow(data))

# Count missing values per column
colSums(is.na(data))

# Drop rows with missing values in specified columns
data_na <- data %>%
  drop_na(CREDIT_LIMIT, MINIMUM_PAYMENTS)

# Verify missing values removed
data_na
colSums(is.na(data_na))

# Check dimensions of new dataset
dim(data_na)

# Remove CUST_ID column
data_clean <- data_na %>%
  select(-CUST_ID)

# Check dimensions of cleaned data
dim(data_clean)

# Summary statistics for data
summary(data_clean)

# Plot correlation matrix
ggcorr(data_clean, hjust=1, layout.exp = 2, label = T,
       label_size = 4, low = "#7d9029", mid = "white",
       high = "#3580d2")

# Data scaling
data_z <- scale(data_clean)
data_z

# Summary of scaled data
summary(data_z)

# K-Means Clustering
# Elbow Method for optimal clusters
fviz_nbclust(data_clean, FUNcluster = kmeans, method = "wss",
             k.max = 10, print.summary = TRUE) + labs(subtitle = "Elbow method")

# Silhouette Method for optimal clusters
fviz_nbclust(data_clean, FUNcluster = kmeans, method = "silhouette", 
             k.max = 10, print.summary = TRUE) + labs(subtitle = "Silhouette Method")

# Gap Statistics for optimal clusters
fviz_nbclust(data_clean, FUNcluster = kmeans, method = "gap_stat", 
             k.max = 10, print.summary = TRUE) + labs(subtitle = "Gap Statistics")

# K-Means with k = 2
set.seed(123)
data_KM2 <- kmeans(x = data_z, centers = 2)

# Show cluster size and centroids
data_KM2$size
data_KM2$centers

# Visualize clusters
fviz_cluster(object = data_KM2, data = data_z, geom = "point") + 
  ggtitle("K-Means Clustering Plot") +
  scale_color_brewer(palette = "Accent") + theme_minimal() + 
  theme(legend.position = "bottom")

# K-Means with k = 4
set.seed(123)
data_KM4 <- kmeans(x = data_z, centers = 4)

# Show cluster size and centroids for k = 4
data_KM4$size
data_KM4$centers

# Visualize clusters for k = 4
fviz_cluster(object = data_KM4, data = data_z,
            geom = "point") + 
  ggtitle("K-Means Clustering Plot") +
  scale_color_brewer(palette = "Accent") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Goodness of Fit
# Within Sum of Squares for k = 2
data_KM2$withinss

# Total Sum of Squares ratio for k = 2
data_KM2$betweenss/data_KM2$totss

# Goodness of Fit for k = 4
# Within Sum of Squares
data_KM4$withinss

# Total Sum of Squares ratio for k = 4
data_KM4$betweenss/data_KM4$totss

# Cluster Profiling
# Add cluster label to data
data_clean$CLUSTER <- as.factor(data_KM4$cluster)

# Aggregated profile by cluster
data_clean %>%
  group_by(CLUSTER) %>%
  summarise_all(mean)

# Aggregation table with min and max values
data_clean %>%
  group_by(CLUSTER) %>%
  summarise_all(mean) %>%
  tidyr::pivot_longer(-CLUSTER) %>%
  group_by(name) %>%
  summarize(cluster_min_val = which.min(value),
            cluster_max_val = which.max(value))

# Plot purchases by cluster
ggplot(data_clean, aes(x = factor(CLUSTER), y = PURCHASES, fill = CLUSTER, colour = CLUSTER)) + 
  geom_bar(stat = "identity", position = "dodge")

# Plot payments by cluster
ggplot(data_clean, aes(x = factor(CLUSTER), y = PAYMENTS, fill = CLUSTER, colour = CLUSTER)) + 
  geom_bar(stat = "identity", position = "dodge")

# Add CUST_ID back to data
data_ID <- data_clean %>%
  mutate(CUST_ID = data_na$CUST_ID)

# Display data table with CUST_ID
datatable(data_ID, options = list(scrollx = TRUE))

# Principal Component Analysis (PCA)
# PCA for dimensionality reduction
data_pca <- PCA(X = data_clean, quali.sup = 18, 
                scale.unit = T, ncp = 17, graph = F)
data_pca$eig

# Variance explained by each dimension
fviz_eig(data_pca, ncp = 17, addlabels = T, 
         main = "Variance explained by each dimensions")

# Plot contribution of variables in PC1
fviz_contrib(X = data_pca, choice = "var", axes = 1)

# Plot contribution of variables in PC2
fviz_contrib(X = data_pca, choice = "var", axes = 2)

# Individual Factor Map for outliers
plot.PCA(x = data_pca, choix = "ind", invisible = "quali", 
         select = "contrib 8", habillage = "CLUSTER") + 
  scale_color_brewer(palette = "Accent") + 
  theme(legend.position = "bottom")

# Variables Factor Map for variable contribution to PCs
fviz_pca_var(data_pca, select.var = list(contrib = 17), col.var = "contrib", 
             gradient.cols = c("red", "white", "blue"), repel = TRUE)

# PCA + K-Means clustering visualization
fviz_pca_biplot(data_pca, habillage = 18, addEllipses = T,
                geom.ind = "point") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_color_brewer(palette = "Accent")
  
# Clustering Results & Business Insights

# Cluster Profiles:
# Cluster 1: Low purchase amounts, minimal withdrawals, and fewer transactions.
# Cluster 2: High purchases, long tenure, low withdrawals, and high full payment rates.
# Cluster 3: High balance, frequent cash advances, high credit limit, lower full payment rates.
# Cluster 4: Lowest balance, credit limit, minimum payments, with short tenure and small transactions.

# Business Suggestions:
# Clusters 1 & 4: Target for reward programs/discounts due to low purchase activity.
# Clusters 2 & 3: Loyalty points could engage these credit-aware customers with high payments.
# Cluster 4: Consider zero-interest offers to boost purchases/payments.

# To find a customer’s cluster, use their ID in the data table’s search tab.
