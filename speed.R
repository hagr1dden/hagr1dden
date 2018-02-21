library(trackeR)
library(data.table)

#speed

runDF <- readTCX(file = '/home/evgeny/My_Route.tcx', timezone = "GMT")
runTr0 <- trackeRdata(runDF)

  plot(runTr0, session = 1)
  plotRoute(runTr0, zoom = 12)
  
runSummary <- summary(runTr0)
print(runSummary)
plot(runSummary, group = c("total", "moving"), what = c("avgSpeed", "distance", "duration"))
dev.copy(png, "graph.png")
  
#parts_of_cluster

runDATA <- setDT(runDF)

  set.seed(123456789)
  lm1 = kmeans(runDATA[,c("latitude", "longitude")], 25, nstart = 4, iter.max = 10)
  plot(runDATA[,c("latitude", "longitude")], col = (lm1$cluster+1), pch = 20, cex = 0.2)
  
#speed in clusters
calculate_speed <- function(data, clus){
  
  mass_speed <- c()
  for(i in 1:nrow(clus$centers)) {

   art <- trackeRdata(subset(data, clus$cluster == i))
   center <- lm1$centers[i,]
   points(x = center[1], y = center[2], col = "black", pch = 20, cex = .9)
   text(x = center[1], y = center[2], pos = 2, cex = 0.5,round(common$avgSpeed, 5))
   common <- summary(art)
   #print(common$avgSpeed)
   mass_speed[i] <- common$avgSpeed
  }
  return(mass_speed)
}

calculate_speed(runDATA, lm1)

partcluster <- subset(runDATA, lm1$cluster == 1)
plotRoute(trackeRdata(partcluster), zoom = 13)
geom_point(lm1, aes(x = lm1$centers[,1], y = lm1$centers[,2]), size = 3)

dev.off()
