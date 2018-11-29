# 线性判别
library(MASS)
data(iris)
iris.lda=lda(Species~.,data=iris)
table(iris$Species,predict(iris.lda,iris)$class)

table<-table(iris$Species,predict(iris.lda,iris)$class)
sum(diag(prop.table(table)))###判对率


# 二次判别
iris.qda=qda(Species~.,data=iris,cv=T)
table(iris$Species,predict(iris.qda,iris)$class)

table<-table(iris$Species,predict(iris.qda,iris)$class)
sum(diag(prop.table(table)))###判对率




# 贝叶斯判别
# 贝叶斯判别式假定对研究对象已有一定的认识 这种认识常用先验概率来描述，
# 当取得样本后就可以用样本来修正已经有的先验概率分布得出后验概率分布，
# 然后通过后验概率分布进行各种统计推。
# 实际上就是使平均误判损失（误判概率与误判损失的结合）ECM达到极小的过程。
library(MASS)
data(iris)
iris.Beyes=lda(Species~.,data=iris,prior=c(1,1,1)/3)
table(iris$Species,predict(iris.Beyes,iris)$class)

table<-table(iris$Species,predict(iris.Beyes,iris)$class)
sum(diag(prop.table(table)))###判对率



# 上面是先验概率相等的情形，下面介绍先验概率不等的情形
iris.Beyes1=lda(Species~.,data=iris,prior=c(7,8,15)/30)
table(iris$Species,predict(iris.Beyes1,iris)$class)

table<-table(iris$Species,predict(iris.Beyes1,iris)$class)
sum(diag(prop.table(table)))###判对率




# 利用决策树分类
library(tree)
set.seed(2)
data(iris)
train=sample(1:nrow(iris),100)
iris.test=iris[-train,]
tree.Species=tree(Species~.,iris,subset=train)
tree.pred=predict(tree.Species,iris.test,type='class')
table(tree.pred,iris.test$Species)

table<-table(tree.pred,iris.test$Species)
sum(diag(prop.table(table)))###判对率



# 用神经网络分类
library(nnet)
set.seed(2)
data(iris)
iris.nnet <-nnet(Species ~ ., linout = F,size = 10, decay = 0.01, maxit = 1000,trace = F,data = iris)
#对分类数据预测需要加上type参数
pre.forest=predict(iris.nnet, iris,type='class')
table(pre.forest,iris$Species) 

table<-table(pre.forest,iris$Species)
sum(diag(prop.table(table)))###判对率



# 利用支持向量机分类
library(e1071)
set.seed(2)
data(iris)
iris.svm <-svm(Species ~ .,data = iris)
pre.forest=predict(iris.svm, iris,type='class')
table(pre.forest,iris$Species) 

table<-table(pre.forest,iris$Species)
sum(diag(prop.table(table)))###判对率





# 基于距离的分类算法
# K—最临近方法（k Nearest Neighbors,简称KNN）
library(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size =round(m/3), replace = FALSE, prob= rep(1/m, m)) ##随机选出训练集合
iris.train <- iris[-val,]
iris.test <- iris[val,]
iris.kknn <- kknn(Species~.,iris.train, iris.test, distance = 5, kernel= "triangular")
fit <- fitted(iris.kknn)
table(iris.test$Species, fit) 

table<-table(iris.test$Species, fit)
sum(diag(prop.table(table)))




# 利用logistic回归分类
library(nnet)
data(iris)
set.seed(2)
train=sample(1:nrow(iris),100)
iris.train=iris[train,]
iris.test=iris[-train,]
iris.logistic<- multinom(Species~., data = iris.train)

summary(iris.logistic)

iris.pre<-predict(iris.logistic,iris.test,type="class")
table(iris.pre,iris.test$Species)

table<-table(iris.pre,iris.test$Species)
sum(diag(prop.table(table)))
