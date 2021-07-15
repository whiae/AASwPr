library(datasets)
library(dbscan)
library(factoextra)

#CZESC1
data(iris)
summary(iris)
iris

df <- data.frame(iris[1:4])
df.scaled <- scale(df)
df.scaled <- as.data.frame(df.scaled)
is.data.frame(df.scaled)

kNNdist(df.scaled, k=5)
kNNdistplot(df.scaled, k=5)
abline(h=1)

a.res <- dbscan(df.scaled, eps = 1, 5)
a.res

fviz_cluster(a.res, df, geom = "point", ellipse = FALSE)

vector <- a.res[["cluster"]]

df2 <- data.frame(iris[1:4], vector)
head(df2)

kNNdistplot(df.scaled, k=5)
abline(h=1.8)
abline(h=0.5)
abline(h=0.4)

b.res <- dbscan(df.scaled, eps = 1.8, 5)
b.res
fviz_cluster(b.res, df, geom = "point", ellipse = TRUE)

c.res <- dbscan(df.scaled, eps = 0.5, 5)
c.res
fviz_cluster(c.res, df, geom = "point", ellipse = TRUE)

d.res <- dbscan(df.scaled, eps = 0.4, 5)
d.res
fviz_cluster(d.res, df, geom = "point", ellipse = TRUE)

#CZESC2
unbalance <- read.table("C:/unbalance.txt", header = TRUE)
head(unbalance)
s1 <- read.table("C:/s1.txt", header = TRUE)
head(s1)

unbalance.scaled <- scale(unbalance)
unbalance.scaled <- as.data.frame(unbalance.scaled)

s1.scaled <- scale(s1)
s1.scaled <- as.data.frame(s1.scaled)


kNNdistplot(unbalance.scaled, k=5)
abline(h=0.3)

kNNdistplot(s1.scaled, k=5)
abline(h=0.1)

#----Unbalance----
#dbscan
unbalance.res <- dbscan(unbalance.scaled, eps = 0.3, 5)
unbalance.res
fviz_cluster(unbalance.res, unbalance, geom = "point", ellipse = TRUE)

#optics
unbalance.res2 <- optics(unbalance.scaled, eps = 0.3, 5)
unbalance.res2
plot(unbalance.res2)

res <- extractDBSCAN(unbalance.res2, eps_cl = 0.3)
plot(res)

#kmeans
dist_un <- get_dist(unbalance.scaled, method = "euclidean")
fviz_nbclust(unbalance.scaled, kmeans, method = "silhouette")

km_un <- kmeans(dist_un, 5)
fviz_cluster(km_un, unbalance)

#----s1----
#dbscan
s1.res <- dbscan(s1.scaled, eps = 0.1, 5)
s1.res
fviz_cluster(s1.res, s1, geom = "point", ellipse = TRUE)

#optics
s1.res2 <- optics(s1.scaled, eps = 0.1, 5)
s1.res2
plot(s1.res2)

res2 <- extractDBSCAN(s1.res2, eps_cl = 0.1)
plot(res2)

#kmeans
dist_s1 <- get_dist(s1.scaled, method = "euclidean")
fviz_nbclust(s1.scaled, kmeans, method = "silhouette")

km_s1 <- kmeans(dist_s1, 10)
fviz_cluster(km_s1, s1)

#----ZB_6----
zb6 <- read.table("C:/zb_6.txt", sep=",", header = TRUE)

zb6.scaled <- scale(zb6)
zb6.scaled <- as.data.frame(zb6.scaled)
head(zb6.scaled)

kNNdistplot(zb6.scaled, k=5)
abline(h=0.4)

zb6.res <- dbscan(zb6.scaled, eps = 0.4, 5)
zb6.res

fviz_cluster(zb6.res, zb6, geom = "point", ellipse = FALSE)

v_zb6 <- zb6.res[["cluster"]]
v_zb6
