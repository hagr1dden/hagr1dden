library(isofor)
data(iris)
df <- iris

df <- subset(df ,  Species=='setosa')  #choose only one of the classes
colnames(new_dt) <- c("x", "y", "dist", "speed")
y <- rownames(new_dt) #make x variables
x <- subset(new_dt, select = c("V2","V3")) #make y variable(dependent)

mod = iForest(new_dt, 100, 30)
p = predict(mod, new_dt)
col = ifelse(p > quantile(p, 0.85), "red", "blue")
ol = ifelse(p > quantile(p, 0.85), 1, 2)

plot(x, col = col, pch = ol)
text(x, pos = 3, cex = 0.8, rownames(x))                             


