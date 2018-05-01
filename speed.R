library(trackeR)
library(data.table)
library(isofor)
library(ggmap)
library(e1071)

#testing

runDF <- readTCX(file = '/home/evgeny/My_Route(1).tcx', timezone = "GMT")
#runTr0 <- trackeRdata(runDF)
#runSummary <- summary(runTr0)
#plotRoute(runTr0, speed = FALSE)
#print(runSummary)
#plot(runSummary, group = c("total", "moving"), what = c("avgSpeed", "distance", "duration"))

#parts_of_cluster

runDATA <- setDT(runDF)
runDATA <- runDATA[ ! duplicated(runDATA$time, fromLast = TRUE), ]

set.seed(123456789)
lm1 = kmeans(runDATA[,c("latitude", "longitude")], 30, nstart = 40, iter.max = 10)
plot(runDATA[,c("latitude", "longitude")], col = (lm1$cluster+1), pch = 20, cex = 0.2)
#change color 

color.gradient <- function(x, colors=c("yellow","red"), colsteps=30) {
  return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

#speed in clusters
calculate_speed <- function(data, clus){
  plot(runDATA[,c("latitude", "longitude")], col = (clus$cluster+1), pch = 20, cex = 0.2)
  mass_speed <- c()
  for(i in 1:nrow(clus$center)) {
    
    art <- trackeRdata(subset(data,clus$cluster == i))
    center <- clus$centers[i,]
    common_art <- summary(art)
    mass_speed[i] <- common_art$avgSpeedMoving
    text(x = center[1], y = center[2], pos = 2, cex = 0.6,round(mass_speed[i], 5))
    text(x = center[1], y = center[2], pos = 3, cex = 0.8, i)
  }
 
  points(clus$centers, col = color.gradient(mass_speed), cex = 4, pch = 20)
  plot(mass_speed, type = "h",lwd = 10, col = color.gradient(mass_speed))
  return(mass_speed)
}

calculate_dist <- function(data, clus){
  plot(runDATA[,c("latitude", "longitude")], col = (clus$cluster+1), pch = 20, cex = 0.2)
  mass_dist <- c()
  for(i in 1:nrow(clus$centers)) {
    
    art <- subset(data[,c("latitude", "longitude")], clus$cluster == i)
    distantion <- dist(art)
    center <- clus$centers[i,]
    mean_cluster <- mean(distantion)
    mass_dist[i] <- mean_cluster
    text(x = center[1], y = center[2], pos = 3, cex = 0.8,round(i, 5))
  }
  mean_common <- mean(mass_dist)
  points(clus$centers, col = color.gradient(mass_dist), cex = 4, pch = 20)
  plot(mass_dist, type = "h",lwd = 10, col = color.gradient(mass_dist))
  return(mass_dist)
  
}

detected_anomaly <- function(all_coords, data, clus){
  
  anomaly_data <- c()
  
  mean_cen <- apply(clus$centers,2,mean)
  content <- get_googlemap(center = c(mean_cen[2],mean_cen[1]), zoom = 11)
  colnames(data) <- c("x", "y", "dist", "speed")
  x <- subset(data, select = c("dist","speed"))
  
  mod = iForest(x, 100, 30)
  p = predict(mod, x)
  
  col = ifelse(p > quantile(p, 0.85), "red", "blue")
  ol = ifelse(p > quantile(p, 0.85), 1, 2)
  
  
  plot(x, col = col, pch = ol)
  text(x, pos = 3, cex = 0.8, rownames(x))  

  for( i in 1:length(p)){
    if(p[i] > quantile(p, 0.85))
    {
      tra <- subset(all_coords, clus$cluster == i)
      anomaly_data <- rbind(anomaly_data, tra)
    }
  }
  ggmap(content) + geom_point(aes(x = longitude, y = latitude), data = all_coords, color = "blue", size = 0.8) + geom_point(aes(x = longitude, y = latitude), data = anomaly_data, color = "red", size = 0.8)
}

classif_bayes <- function(data){
  
  colnames(data) <- c("x", "y", "dist", "speed")
  cor(data$dist, data$speed, method = c("pearson"))
  
  classes_speed <- ifelse(data$speed > mean(data$speed)* 1.1 | data$dist > mean(data$dist) * 1.1, "not_interesting",
                          ifelse(data$speed < mean(data$speed) * 0.9 | data$dist < mean(data$dist)*0.9, "interesting", "casual"))
  
  data_new <- cbind(new_dt, classes_speed)
  model2 <- naiveBayes(data[,3], data_new[,5])
  pr2train <- predict(model2, data[,3])
  t2train = table(pr2train, data_new[,5], dnn=list('Предсказано        ', 'На самом деле'))
  
  return(data_new)
}

classif_bayes(new_dt)
x <- subset(runDATA, lm1$cluster == 16)
y <- trackeRdata(x)
calculate_speed(runDATA,lm1)
new_dt <- data.table(lm1$centers, calculate_dist(runDATA, lm1), calculate_speed(runDATA,lm1))
detected_anomaly(runDATA,new_dt, lm1)
mean_cen <- apply(lm1$centers,2,mean)
