# Library
library(psych)
library(GPArotation)
library(clValid)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyverse)
library(car)
library(readxl)

# ===========================================
# Data 1A
# ===========================================
# Read data
data <- read_excel("All Datasets.xlsx", sheet = "1A")
data <- data.frame(data)

# Statistik Deskriptif
statdes <- summary(data)
print(statdes)

# Uji Sampel Representatif (KMO)
ujikmo <- KMO(data)
print(ujikmo)

# Korelasi
ujikorelasi <- cor(data, method = 'pearson')
print(ujikorelasi)

# Standarisasi
datastandarisasi <- scale(data)

# Jarak Euclidean
d1 <- dist(datastandarisasi)

# Korelasi Cophenetic untuk 5 metode linkage
cor_values <- data.frame(
  Single = cor(d1, cophenetic(hclust(d1, method = "single"))),
  Average = cor(d1, cophenetic(hclust(d1, method = "average"))),
  Complete = cor(d1, cophenetic(hclust(d1, method = "complete"))),
  Weighted = cor(d1, cophenetic(hclust(d1, method = "mcquitty"))),
  Ward = cor(d1, cophenetic(hclust(d1, method = "ward.D")))
)
print(cor_values)

# Internal validation
inval <- clValid(datastandarisasi, 2:4, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "average")
print(optimalScores(inval))

# Save internal validation plot
plot(inval)

# Jarak dan hasil clustering
hclust_result <- hclust(d1, method = "average")

# Dendrogram
dendrogram_object <- as.dendrogram(hclust_result)
plot(dendrogram_object, main = paste("Dendrogram - 1A"), xlab = "Observasi", ylab = "Jarak")

# Dendrogram + Cluster kotak
clusterhier <- eclust(data, FUNcluster = "hclust", k = 2, hc_method = "average", graph = FALSE)
fviz_dend(clusterhier, rect = TRUE, cex = 0.5, main = paste("Cluster Dendrogram - 1A (boxed)"))

# (Opsional) Simpan hasil cluster & aggregate
cluster_result <- clusterhier$cluster
hasil_aggregat <- aggregate(data, list(cluster_result), mean)
writexl::write_xlsx(hasil_aggregat, "aggregate cluster 1A.xlsx")

# ===========================================
# Data 2A
# ===========================================
# Read data
data <- read_excel("All Datasets.xlsx", sheet = "2A")
data <- data.frame(data)

# Statistik Deskriptif
statdes <- summary(data)
print(statdes)

# Uji Sampel Representatif (KMO)
ujikmo <- KMO(data)
print(ujikmo)

# Korelasi
ujikorelasi <- cor(data, method = 'pearson')
print(ujikorelasi)

# Standarisasi
datastandarisasi <- scale(data)

# Jarak Euclidean
d1 <- dist(datastandarisasi)

# Korelasi Cophenetic untuk 5 metode linkage
cor_values <- data.frame(
  Single = cor(d1, cophenetic(hclust(d1, method = "single"))),
  Average = cor(d1, cophenetic(hclust(d1, method = "average"))),
  Complete = cor(d1, cophenetic(hclust(d1, method = "complete"))),
  Weighted = cor(d1, cophenetic(hclust(d1, method = "mcquitty"))),
  Ward = cor(d1, cophenetic(hclust(d1, method = "ward.D")))
)
print(cor_values)

# Internal validation
inval <- clValid(datastandarisasi, 2:4, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "average")
print(optimalScores(inval))

# Save internal validation plot
plot(inval)

# Dendrogram
hclust_result <- hclust(dist(data), method = "average")
dendrogram_object <- as.dendrogram(hclust_result)

# Dendrogram + Cluster kotak
clusterhier <- eclust(data, FUNcluster = "hclust", k = 2, hc_method = "average", graph = FALSE)

fviz_dend(clusterhier, rect = TRUE, cex = 0.5, main = paste("Cluster Dendrogram - 2A (boxed)"))

# (Opsional) Simpan hasil cluster & aggregate
cluster_result <- clusterhier$cluster
hasil_aggregat <- aggregate(data, list(cluster_result), mean)
writexl::write_xlsx(hasil_aggregat, "aggregate cluster 2A.xlsx")

# ===========================================
# Data 3A
# ===========================================
# Read data
data <- read_excel("All Datasets.xlsx", sheet = "3A")
data <- data.frame(data)

# Statistik Deskriptif
statdes <- summary(data)
print(statdes)

# Uji Sampel Representatif (KMO)
ujikmo <- KMO(data)
print(ujikmo)

# Korelasi
ujikorelasi <- cor(data, method = 'pearson')
print(ujikorelasi)

# Standarisasi
datastandarisasi <- scale(data)

# Jarak Euclidean
d1 <- dist(datastandarisasi)

# Korelasi Cophenetic untuk 5 metode linkage
cor_values <- data.frame(
  Single = cor(d1, cophenetic(hclust(d1, method = "single"))),
  Average = cor(d1, cophenetic(hclust(d1, method = "average"))),
  Complete = cor(d1, cophenetic(hclust(d1, method = "complete"))),
  Weighted = cor(d1, cophenetic(hclust(d1, method = "mcquitty"))),
  Ward = cor(d1, cophenetic(hclust(d1, method = "ward.D")))
)
print(cor_values)

# Internal validation
inval <- clValid(datastandarisasi, 2:4, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "average")
print(optimalScores(inval))

# Save internal validation plot
plot(inval)

# Dendrogram
hclust_result <- hclust(dist(data), method = "average")
dendrogram_object <- as.dendrogram(hclust_result)

# Dendrogram + Cluster kotak
clusterhier <- eclust(data, FUNcluster = "hclust", k = 2, hc_method = "average", graph = FALSE)

fviz_dend(clusterhier, rect = TRUE, cex = 0.5, main = paste("Cluster Dendrogram - 3A (boxed)"))

# (Opsional) Simpan hasil cluster & aggregate
cluster_result <- clusterhier$cluster
hasil_aggregat <- aggregate(data, list(cluster_result), mean)
writexl::write_xlsx(hasil_aggregat, "aggregate cluster 3A.xlsx")

# ===========================================
# Data 1B
# ===========================================
# Read data
data <- read_excel("All Datasets.xlsx", sheet = "1B")
data <- data.frame(data)

# Statistik Deskriptif
statdes <- summary(data)
print(statdes)

# Uji Sampel Representatif (KMO)
ujikmo <- KMO(data)
print(ujikmo)

# Korelasi
ujikorelasi <- cor(data, method = 'pearson')
print(ujikorelasi)

# Standarisasi
datastandarisasi <- scale(data)

# Jarak Euclidean
d1 <- dist(datastandarisasi)

# Korelasi Cophenetic untuk 5 metode linkage
cor_values <- data.frame(
  Single = cor(d1, cophenetic(hclust(d1, method = "single"))),
  Average = cor(d1, cophenetic(hclust(d1, method = "average"))),
  Complete = cor(d1, cophenetic(hclust(d1, method = "complete"))),
  Weighted = cor(d1, cophenetic(hclust(d1, method = "mcquitty"))),
  Ward = cor(d1, cophenetic(hclust(d1, method = "ward.D")))
)
print(cor_values)

# Internal validation
inval <- clValid(datastandarisasi, 2:4, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "average")
print(optimalScores(inval))

# Save internal validation plot
plot(inval)

# Dendrogram
hclust_result <- hclust(dist(data), method = "average")
dendrogram_object <- as.dendrogram(hclust_result)

# Dendrogram + Cluster kotak
clusterhier <- eclust(data, FUNcluster = "hclust", k = 2, hc_method = "average", graph = FALSE)

fviz_dend(clusterhier, rect = TRUE, cex = 0.5, main = paste("Cluster Dendrogram - 1B"))

# (Opsional) Simpan hasil cluster & aggregate
cluster_result <- clusterhier$cluster
hasil_aggregat <- aggregate(data, list(cluster_result), mean)
writexl::write_xlsx(hasil_aggregat, "aggregate cluster 1B.xlsx")

# ===========================================
# Data 2B
# ===========================================
# Read data
data <- read_excel("All Datasets.xlsx", sheet = "2B")
data <- data.frame(data)

# Statistik Deskriptif
statdes <- summary(data)
print(statdes)

# Uji Sampel Representatif (KMO)
ujikmo <- KMO(data)
print(ujikmo)

# Korelasi
ujikorelasi <- cor(data, method = 'pearson')
print(ujikorelasi)

# Standarisasi
datastandarisasi <- scale(data)

# Jarak Euclidean
d1 <- dist(datastandarisasi)

# Korelasi Cophenetic untuk 5 metode linkage
cor_values <- data.frame(
  Single = cor(d1, cophenetic(hclust(d1, method = "single"))),
  Average = cor(d1, cophenetic(hclust(d1, method = "average"))),
  Complete = cor(d1, cophenetic(hclust(d1, method = "complete"))),
  Weighted = cor(d1, cophenetic(hclust(d1, method = "mcquitty"))),
  Ward = cor(d1, cophenetic(hclust(d1, method = "ward.D")))
)
print(cor_values)

# Internal validation
inval <- clValid(datastandarisasi, 2:4, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "average")
print(optimalScores(inval))

# Save internal validation plot
plot(inval)

# Dendrogram
hclust_result <- hclust(dist(data), method = "average")
dendrogram_object <- as.dendrogram(hclust_result)

# Dendrogram + Cluster kotak
clusterhier <- eclust(data, FUNcluster = "hclust", k = 2, hc_method = "average", graph = FALSE)
fviz_dend(clusterhier, rect = TRUE, cex = 0.5, main = paste("Cluster Dendrogram - 2B"))


# (Opsional) Simpan hasil cluster & aggregate
cluster_result <- clusterhier$cluster
hasil_aggregat <- aggregate(data, list(cluster_result), mean)
writexl::write_xlsx(hasil_aggregat, "aggregate cluster 2B.xlsx")

# ===========================================
# Data 3B
# ===========================================
# Read data
data <- read_excel("All Datasets.xlsx", sheet = "3B")
data <- data.frame(data)

# Statistik Deskriptif
statdes <- summary(data)
print(statdes)

# Uji Sampel Representatif (KMO)
ujikmo <- KMO(data)
print(ujikmo)

# Korelasi
ujikorelasi <- cor(data, method = 'pearson')
print(ujikorelasi)

# Standarisasi
datastandarisasi <- scale(data)

# Jarak Euclidean
d1 <- dist(datastandarisasi)

# Korelasi Cophenetic untuk 5 metode linkage
cor_values <- data.frame(
  Single = cor(d1, cophenetic(hclust(d1, method = "single"))),
  Average = cor(d1, cophenetic(hclust(d1, method = "average"))),
  Complete = cor(d1, cophenetic(hclust(d1, method = "complete"))),
  Weighted = cor(d1, cophenetic(hclust(d1, method = "mcquitty"))),
  Ward = cor(d1, cophenetic(hclust(d1, method = "ward.D")))
)
print(cor_values)

# Internal validation
inval <- clValid(datastandarisasi, 2:4, clMethods = "hierarchical", validation = "internal", metric = "euclidean", method = "average")
print(optimalScores(inval))

# Save internal validation plot
plot(inval)

# Dendrogram
hclust_result <- hclust(dist(data), method = "average")
dendrogram_object <- as.dendrogram(hclust_result)

# Dendrogram + Cluster kotak
clusterhier <- eclust(data, FUNcluster = "hclust", k = 2, hc_method = "average", graph = FALSE)
fviz_dend(clusterhier, rect = TRUE, cex = 0.5, main = paste("Cluster Dendrogram - 3B"))

# (Opsional) Simpan hasil cluster & aggregate
cluster_result <- clusterhier$cluster
hasil_aggregat <- aggregate(data, list(cluster_result), mean)
writexl::write_xlsx(hasil_aggregat, "aggregate cluster 3B.xlsx")
