library(cluster)

concap <- read.csv(file = "C:/concap.csv", 
                  sep=",", dec=".", 
                  header = TRUE)

newconcap <- na.omit(concap)

gr <- dist(newconcap, method="euclidean")

a <- hclust(gr)
plot(a)

avg <- hclust(gr, method="ward")
plot(avg)

b <- cutree(a, k = 3)
plot(b)
rect.hclust(a, k = 3)

c <- cutree(a, h = 20)
plot(c)
abline(h = 20, col="red")

clusters.color = c(rgb(0, 1, 0, 1), rgb(0, 0, 1, 1),
                   rgb(1, 0, 0, 1), rgb(1, 0, 1, 1), rgb(0, 1, 1, 1))

metoda1 <- hclust(gr, method="ward.D")
plot(metoda1)
grupy1 <- cutree(metoda1, k = 5)
plot(grupy1, main="WardD method")
rect.hclust(metoda1, k = 5, border = clusters.color)

metoda2 <- hclust(gr, method="ward.D2")
plot(metoda2)
grupy2 <- cutree(metoda2, k = 5)
plot(grupy2, main="WardD2 method")
rect.hclust(metoda2, k = 5, border = clusters.color)

metoda3 <- hclust(gr, method="single")
plot(metoda3)
grupy3 <- cutree(metoda3, k = 5)
plot(grupy3, main="Single method")
rect.hclust(metoda3, k = 5, border = clusters.color)

metoda4 <- hclust(gr, method="complete")
plot(metoda4)
grupy4 <- cutree(metoda4, k = 5)
plot(grupy4, main="Complete method")
rect.hclust(metoda4, k = 5, border = clusters.color)

metoda5 <- hclust(gr, method="average")
plot(metoda5)
grupy5 <- cutree(metoda5, k = 5)
plot(grupy5, main="Average method")
rect.hclust(metoda5, k = 5, border = clusters.color)

metoda6 <- hclust(gr, method="mcquitty")
plot(metoda6)
grupy6 <- cutree(metoda6, k = 5)
plot(grupy6, main="Mcquitty method")
rect.hclust(metoda6, k = 5, border = clusters.color)

metoda7 <- hclust(gr, method="median")
plot(metoda7)
grupy7 <- cutree(metoda7, k = 5)
plot(grupy7, main="Median method")
rect.hclust(metoda7, k = 5, border = clusters.color)

metoda8 <- hclust(gr, method="centroid")
plot(metoda8)
grupy8 <- cutree(metoda8, k = 5)
plot(grupy8, main="Centroid method")
rect.hclust(metoda8, k = 5, border = clusters.color)

kmethod <- kmeans(dist(newconcap),5)
plot(newconcap, pch = 19, col = kmethod$cluster,
     main = "K-means clustering")
