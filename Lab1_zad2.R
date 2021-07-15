mouse <- read.csv("C:/mouse.txt", header=T, sep=' ', dec='.')
mouse
dane_mouse <- mouse[,1:2]
klaster_mouse <- kmeans(dane_mouse, 3)
klaster_mouse

plot(mouse$X, mouse$Y)
plot(klaster_mouse$X, klaster_mouse$Y)

sum(mouse$descr == "Head")
sum(mouse$descr == "left")
sum(mouse$descr == "right")
summary(mouse$descr)
