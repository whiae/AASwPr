library(cluster)
library(factoextra)
library(NbClust)


unbalance <- read.csv(file = "C:/unbalance.txt", 
                   sep=" ", dec=".", 
                   header = TRUE)
                   
fviz_nbclust(unbalance, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

for (k in (2:9)) {
  km_res <- kmeans(unbalance, centers = k)
  sil <- silhouette(km_res$cluster, dist(unbalance))
  fviz_silhouette(sil)
}

x <- pam(unbalance, k=2, metric = "manhattan")
fviz_cluster(x)


