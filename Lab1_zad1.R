data(iris)
dane_4d <- iris[,1:4]
dane_2d <- iris[,1:2]
dane_3d <- iris[,1:3]
klaster1 <- kmeans(dane_4d, 3)
klaster2 <- kmeans(dane_2d, 3)
klaster3 <- kmeans(dane_3d, 3)
klaster1
klaster2
klaster3
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)
legend(7,4.3,unique(iris$Species),col=1:length(iris$Species),pch=1)

