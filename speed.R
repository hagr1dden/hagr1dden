library(trackeR)
library(data.table)

#testing

runDF <- readTCX(file = '/home/evgeny/My_Route(1).tcx', timezone = "GMT")
runTr0 <- trackeRdata(runDF)
bluefunc <- colorRampPalette(c("lightblue", "darkblue"))
#plot(runTr0, session = 1)
#plotRoute(runTr0, zoom = 12)
runSummary <- summary(runTr0)
print(runSummary)
plot(runSummary, group = c("total", "moving"), what = c("avgSpeed", "distance", "duration"))
#dev.copy(png, "graph.png")

#parts_of_cluster

runDATA <- setDT(runDF)
runDATA <- runDATA[ ! duplicated(runDATA$time, fromLast = TRUE), ]

set.seed(123456789)
lm1 = kmeans(runDATA[,c("latitude", "longitude")], 20, nstart = 40, iter.max = 10)
plot(runDATA[,c("latitude", "longitude")], col = (lm1$cluster+1), pch = 20, cex = 0.2)

color.gradient <- function(x, colors=c("red","yellow"), colsteps=50) {
  return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
#speed in clusters
calculate_speed <- function(data, clus){
  
  mass_speed <- c()
  for(i in 1:nrow(clus$centers)) {
    
    art <- trackeRdata(subset(data,clus$cluster == i))
    center <- clus$centers[i,]
    common_art <- summary(art)
    #print(common$avgSpeed)
    points(x = center[1], y = center[2], pch = 20, cex = .9)
    text(x = center[1], y = center[2], pos = 2, cex = 0.5,round(common_art$avgSpeedMoving, 5))
    mass_speed[i] <- common_art$avgSpeedMoving
  }
  points(clus$centers, col = color.gradient(mass_speed), cex = 4, pch = 20)
  plot(mass_speed, type = "h",lwd = 10, col = color.gradient(mass_speed))
  return(mass_speed)
}

calculate_dist <- function(data, clus){
  
  mass_dist <- c()
  for(i in 1:nrow(clus$centers)) {
    
    art <- subset(data[,c("latitude", "longitude")], clus$cluster == i)
    distantion <- dist(art)
    mean_cluster <- mean(distantion)
    mass_dist[i] <- mean_cluster
    #  print(mass_dist[i])
  }
  mean_common <- mean(mass_dist)
  print(mean_common)
  for(i in 1:length(mass_dist)){
    if(mass_dist[i] < mean_common)
    {
      art <- subset(data[,c("latitude", "longitude")], clus$cluster == i)
      points(art$latitude, art$longitude, col = "pink", cex = 2, pch = 20)
      # points(clus$centers[i,1], clus$centers[i,2], col = "red", pch = 20, cex = 2)
    }
  }
  
  
  return(mass_dist)
  
}

calculate_dist(runDATA, lm1)
calculate_speed(runDATA, lm1)

partcluster <- subset(runDATA, lm1$cluster == 20)
plotRoute(trackeRdata(partcluster), zoom = 13)

l1 <- subset(runDATA[,c("latitude", "longitude")],lm1$cluster == 2)
m <- mean(dist(l1))
mx <- data.frame(subset(lm1$centers, lm1$withinss < m))
