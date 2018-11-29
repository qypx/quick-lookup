##动态聚类k-means 
newiris <- iris
newiris$Species <- NULL
(kc <- kmeans(newiris, 3)) # 显示K-均值聚类结果

table(iris$Species, kc$cluster)#将聚类结果与species进行比较
#值得注意的是多次运行得到的k-means聚类结果可能不同，因为初始的簇中心是随机选择的

plot(newiris[c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col = iris$Species)
newiris$species=kc$cluster

library(fpc)
plotcluster(na.omit(newiris),kc$cluster)# 生成聚类图







##层次聚类(系统聚类) hclust
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])

# 修剪树使之分为3类
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)


##层次聚类
data(iris)
iris.hc <- hclust( dist(iris[,1:4]))
# plot( iris.hc, hang = -1)
plot( iris.hc, labels =FALSE, hang = -1)
re <- rect.hclust(iris.hc, k = 3)
iris.id <- cutree(iris.hc, 3)
table(iris.id, iris$Species)




