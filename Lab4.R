library(factoextra)
library(cluster)

#ZADANIE 1
wine <- read.csv(file = "C:/wine.csv", 
                 sep=",", dec=".", 
                 header = TRUE)

wine <- na.omit(wine)
wine_z <- scale(wine)

fviz_nbclust(wine_z, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

d <- kmeans(dist(wine_z),3)
d
plot(wine, pch=19, col=d$cluster,
     main="K-means clustering")

gr0 <- dist(wine_z, method = "euclidean")
c <- hclust(gr0, method = "ward.D")

plot(c)
rect.hclust(c, k = 3)

grupy <- cutree(c, k = 3)
grupy



#ZADANIE 2
dane <- read.csv(file = "C:/shopping-data.csv", 
                         sep=",", dec=".", 
                  header = TRUE)

dane

colnames(dane) <- c("id", 
                       "genre",
                       "age",
                       "income",
                       "score")

dane <- na.omit(dane)
dane_z <- scale(select(dane,income,score))

ggplot(dane, aes(x=score, y=income)) +
  geom_point()

ggplot(dane, aes(x=score, y=age)) +
  geom_point()

ggplot(dane, aes(x=age, y=income)) +
  geom_point()

gr <- dist(dane_z, method = "euclidean")

a <- hclust(gr, method = "ward.D")

plot(a)
rect.hclust(a, k=5)

groups <- cutree(a, 5)
groups

dane$u <- as.factor(groups)
ggplot(dane, aes(x=score, 
                    y=income,
                    color=u)) + geom_point()




#ZADANIE 3
animals <- cluster::animals

colnames(animals) <- c("warm-blooded", 
                       "can fly",
                       "vertebrate",
                       "endangered",
                       "live in groups",
                       "have hair")

animals <- na.omit(animals)
animals_z <- scale(animals)

gr2 <- dist(animals_z, method = "euclidean")
b <- hclust(gr2, method = "ward.D")

plot(b)
rect.hclust(b, k = 4)
