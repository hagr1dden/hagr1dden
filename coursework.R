df.coord <- data.table::fread("/home/evgeny/files/coord_moved.csv",  dec = ",")
coords_1 <- subset(df.coord[,.(V6,V7)])
coor_1 <- coords_1[complete.cases(coords_1)]
library(factoextra)
lm1 = kmeans(coor_1, 50, nstart = 4, iter.max = 10)
plot(coor_1, col = (lm1$cluster+1), pch = 20, cex = 0.2)
vizio <- fviz_cluster(lm1, data = coor_1, geom = "point", pch = '.')
c_clusters <-data.frame(lm1$centers, lm1$size)
mean_count <- mean(lm1$size)
filter_clusters <- subset(c_clusters, lm1$size > mean_count)
dot_max <- filter_clusters[order(filter_clusters$V6),]
points(dot_max, col = 'red', pch = 19)
plot(lm1$centers)
points(dot_max$V6, dot_max$V7,type = "s")
