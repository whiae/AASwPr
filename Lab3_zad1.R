library(factoextra)
library(NbClust)

seeds <- read.csv(file = "C:/seeds_dataset.txt", 
                         sep=",", dec=".", 
                  header = TRUE)

seeds_stand <- scale(seeds, center = TRUE, scale = TRUE)

fviz_nbclust(seeds_stand, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

fviz_nbclust(seeds_stand, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

nbclust_out <- NbClust(
  data = seeds_stand,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 5,
  method = "kmeans"
)

nbclust_out <- NbClust(
  data = seeds_stand,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 5,
  method = "ward.D"
)


gr <- dist(seeds_stand, method="euclidean")
a <- hclust(gr, method="ward.D")
plot(a)

grupy <- cutree(a, k = 3)
plot(grupy)
rect.hclust(a, k = 3)

b <- kmeans(dist(seeds),3)
plot(seeds, pch=19, col=b$cluster,
     main="K-means clustering")

c <- kmeans(dist(seeds_stand),3)
plot(seeds_stand, pch=19, col=c$cluster,
     main="K-means clustering")
