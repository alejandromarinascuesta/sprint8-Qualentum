# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

# 1. Carga del dataset
mall_data <- read.csv("Mall_Customers.csv")  # Cambia el nombre del archivo si es necesario
head(mall_data)

# 2. Exploración y limpieza de datos
dim(mall_data)
str(mall_data)
summary(mall_data)

# Renombrar columnas para facilitar el manejo
colnames(mall_data) <- c("CustomerID", "Genre", "Age", "AnnualIncome", "SpendingScore")

# Codificar la variable 'Genre' como numérica (1 para masculino, 0 para femenino)
mall_data$Genre <- ifelse(mall_data$Genre == "Male", 1, 0)

# Normalizar las variables numéricas clave (Annual Income y Spending Score)
mall_data$AnnualIncome <- scale(mall_data$AnnualIncome)
mall_data$SpendingScore <- scale(mall_data$SpendingScore)

# 3. Exploración de variables
# Histogramas de variables demográficas y de comportamiento
ggplot(mall_data, aes(x = Age)) + geom_histogram(bins = 30, fill = "lightblue", color = "black")
ggplot(mall_data, aes(x = AnnualIncome)) + geom_histogram(bins = 30, fill = "lightgreen", color = "black")
ggplot(mall_data, aes(x = SpendingScore)) + geom_histogram(bins = 30, fill = "lightcoral", color = "black")

# 4. Entrenamiento de modelos de clustering

# K-Means con diferentes números de clusters
set.seed(123)
kmeans_results <- list()
for (k in 2:6) {
  kmeans_results[[k]] <- kmeans(mall_data[, c("AnnualIncome", "SpendingScore")], centers = k, nstart = 25)
}

# Método del codo para determinar el número óptimo de clusters
wss <- sapply(2:6, function(k) kmeans(mall_data[, c("AnnualIncome", "SpendingScore")], centers = k, nstart = 25)$tot.withinss)
plot(2:6, wss, type = "b", pch = 19, frame = FALSE, xlab = "Número de Clusters", ylab = "Suma de cuadrados dentro de los clusters")

# K-Means con el número óptimo de clusters (supongamos k = 5)
kmeans_final <- kmeans(mall_data[, c("AnnualIncome", "SpendingScore")], centers = 5, nstart = 25)
mall_data$Cluster_KMeans <- kmeans_final$cluster

# Clustering Jerárquico
dissimilarity_matrix <- dist(mall_data[, c("AnnualIncome", "SpendingScore")], method = "euclidean")
hclust_results <- hclust(dissimilarity_matrix, method = "ward.D")
plot(hclust_results)

# Cortar el dendrograma en 5 clusters
hclust_cut <- cutree(hclust_results, k = 5)
mall_data$Cluster_Hierarchical <- hclust_cut

# 5. Evaluación de modelos
# Métrica de Silueta para K-Means
silhouette_kmeans <- silhouette(kmeans_final$cluster, dissimilarity_matrix)
avg_silhouette_kmeans <- mean(silhouette_kmeans[, 3])

# Métrica de Silueta para Clustering Jerárquico
silhouette_hierarchical <- silhouette(hclust_cut, dissimilarity_matrix)
avg_silhouette_hierarchical <- mean(silhouette_hierarchical[, 3])

# Comparar las puntuaciones de silueta
avg_silhouette_kmeans
avg_silhouette_hierarchical

# 6. Análisis descriptivo de segmentos
# Estadísticas descriptivas por cluster para K-Means
aggregate(cbind(Age, AnnualIncome, SpendingScore) ~ Cluster_KMeans, data = mall_data, FUN = mean)

# Estadísticas descriptivas por cluster para Clustering Jerárquico
aggregate(cbind(Age, AnnualIncome, SpendingScore) ~ Cluster_Hierarchical, data = mall_data, FUN = mean)

# 7. Visualización
# Gráfico de dispersión para K-Means
ggplot(mall_data, aes(x = AnnualIncome, y = SpendingScore, color = factor(Cluster_KMeans))) + 
  geom_point(size = 3) + 
  labs(title = "Clusters de K-Means", x = "Ingreso Anual", y = "Puntuación de Gasto")

# Gráfico de dispersión para Clustering Jerárquico
ggplot(mall_data, aes(x = AnnualIncome, y = SpendingScore, color = factor(Cluster_Hierarchical))) + 
  geom_point(size = 3) + 
  labs(title = "Clusters de Clustering Jerárquico", x = "Ingreso Anual", y = "Puntuación de Gasto")

