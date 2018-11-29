###分类树###
library(RSADBE)
data(CART_Dummy)
CART_Dummy$Y=as.factor(CART_Dummy$Y)
attach(CART_Dummy)
plot(c(0,12),c(0,10),type="n",xlab="X1",ylab="X2")
points(X1[Y==1],X2[Y==1],pch=19,col="green")
points(X1[Y==0],X2[Y==0],pch=15,col="red")
abline(v=6,lwd=2)
segments(x0=c(0,0,6,6),y0=c(3.75,6.25,2.25,5),x1=c(6,6,12,12),y1=c(3.75,6.25,2.25,5),lwd=2)
title(main="Looks a Solvable Problem Under Partitions")

library(rpart)
CART_Dummy_rpart = rpart(Y~X1+X2,data=CART_Dummy)
plot(CART_Dummy_rpart)
text(CART_Dummy_rpart)
summary(CART_Dummy_rpart)

library(rpart.plot)
rpart.plot(CART_Dummy_rpart)

library(rattle)
asRules(CART_Dummy_rpart)
plot(c(0,12),c(0,10),type="n",xlab="X1",ylab="X2")
points(X1[Y==1],X2[Y==1],pch=19,col="green")
points(X1[Y==0],X2[Y==0],pch=15,col="red")
abline(h=4.875,lwd=2)
segments(x0=4.5,y0=4.875,x1=4.5,y1=10,lwd=2)
abline(h=1.75,lwd=2)
segments(x0=3.5,y0=1.75,x1=3.5,y1=4.875,lwd=2)
title(main="Classification Tree on the Data Display")


##回归树##
library(MASS)
data(cpus)
hist(cpus$perf)##高度偏态分布##
###因此，我们将构造对数变换的回归树###
cpus.ltrpart = rpart(log10(perf)~syct+mmin+mmax+cach+chmin+chmax,data=cpus)
plot(cpus.ltrpart)
text(cpus.ltrpart)


##分类树##
library(rpart)
data("kyphosis")
ky_rpart = rpart(Kyphosis~Age+Number+Start,data=kyphosis,parms=list(split="information"))
plot(ky_rpart);text(ky_rpart)

library(rpart.plot)
rpart.plot(ky_rpart)



###使用函数rpart对德国信用数据集构造分类树，同时运用训练+验证+测试的方法，并得到受试工作者特征曲线###
###首先我们将德国信用数据分为三部分，即训练集、验证集合测试集，使用训练部分的数据构造分类树，###
###然后将它运用到验证部分，可视化相应的受试工作者特征曲线，如果我们感觉两条曲线是相似的，那我们将###
###该数据运用到测试部分，并采取必要措施限制客户的贷款###
##训练+验证+测试##
library(RSADBE)
data(GC)###德国信用数据###
set.seed(1234567)
data_part_label = c("Train","Validate","Test")
indv_label=sample(data_part_label,size=nrow(GC),replace = T,prob=c(0.6,0.2,0.2))
GC_Train=GC[indv_label=="Train",]
GC_Validate=GC[indv_label=="Validate",]
GC_Test=GC[indv_label=="Test",]
##构造分类树，并可视化该树##
GC_rpart = rpart(good_bad~.,data = GC_Train)
plot(GC_rpart);text(GC_rpart)
library(rpart.plot)
rpart.plot(GC_rpart)
##树的规律##
library(rattle)
asRules(GC_rpart)
##使用上述验证部分给出的树，并绘制受试工作者特征曲线##
Pred_Train_Class=predict(GC_rpart,type='class')
Pred_Train_Prob=predict(GC_rpart,type='prob')
Train_Pred=prediction(Pred_Train_Prob[,2],GC_Train$good_bad)
Perf_Train=performance(Train_Pred,"tpr","fpr")
plot(Perf_Train,col="blue",lty=2)

Pred_Validate_Class=predict(GC_rpart,newdata=GC_Validate[,-21],type='class')
Pred_Validate_Prob=predict(GC_rpart,newdata = GC_Validate[,-21],type='prob')
Validate_Pred=prediction(Pred_Validate_Prob[,2],GC_Validate$good_bad)
Perf_Validate=performance(Validate_Pred,"tpr","fpr")
plot(Perf_Validate,col="black",lty=2,add=T)
##接下来对测试部分进行预测##
Pred_Test_Class=predict(GC_rpart,newdata=GC_Test[,-21],type='class')
Pred_Test_Prob=predict(GC_rpart,newdata = GC_Test[,-21],type='prob')
Test_Pred=prediction(Pred_Test_Prob[,2],GC_Test$good_bad)
Perf_Test=performance(Test_Pred,"tpr","fpr")
plot(Perf_Test,col="red",lty=2,add=T)

legend(0.5,0.5,c("Train Curve","Validate Curve","Test Curve"),col=c("black","black","red"),pch="-")
###受试工作者特征曲线表面该树在验证部分效果不是很好###


###函数rpart中的参数minsplit和cp可以改善分类与回归树###
##指定minsplit=30,并使用新的分类树重新绘制受试工作者特征曲线图##
GC_rpart_minsplit = rpart(good_bad~.,data=GC_Train,minsplit=30)
GC_rpart_minsplit = prune(GC_rpart,cp=0.05)

Pred_Train_Class = predict(GC_rpart_minsplit,type='class')
Pred_Train_Prob=predict(GC_rpart_minsplit,type='prob')
Train_Pred=prediction(Pred_Train_Prob[,2],GC_Train$good_bad)
Perf_Train=performance(Train_Pred,"tpr","fpr")
plot(Perf_Train,col="blue",lty=2)

Pred_Validate_Class = predict(GC_rpart_minsplit,newdata=GC_Validate[,-21],type='class')
Pred_Validate_Prob=predict(GC_rpart_minsplit,newdata = GC_Validate[,-21],type='prob')
Validate_Pred=prediction(Pred_Validate_Prob[,2],GC_Validate$good_bad)
Perf_Validate=performance(Validate_Pred,"tpr","fpr")
plot(Perf_Validate,col="black",lty=2,add=T)

Pred_Test_Class = predict(GC_rpart_minsplit,newdata=GC_Test[,-21],type='class')
Pred_Test_Prob = predict(GC_rpart_minsplit,newdata = GC_Test[,-21],type='prob')
Test_Pred = prediction(Pred_Test_Prob[,2],GC_Test$good_bad)
Perf_Test = performance(Test_Pred,"tpr","fpr")
plot(Perf_Test,col="red",lty=2,add=T)

legend(0.5,0.5,c("Train Curve","Validate Curve","Test Curve"),col=c("black","black","red"),pch="-")
title(main="Improving a Classification Tree with minsplit")

##对于修剪因子cp=0.02,重复受试工作者特征曲线(ROC曲线)图练习：##
GC_rpart_prune = prune(GC_rpart,cp=0.02)

Pred_Train_Class = predict(GC_rpart_prune,type='class')
Pred_Train_Prob=predict(GC_rpart_prune,type='prob')
Train_Pred=prediction(Pred_Train_Prob[,2],GC_Train$good_bad)
Perf_Train=performance(Train_Pred,"tpr","fpr")
plot(Perf_Train,col="blue",lty=2)

Pred_Validate_Class = predict(GC_rpart_prune,newdata=GC_Validate[,-21],type='class')
Pred_Validate_Prob=predict(GC_rpart_prune,newdata = GC_Validate[,-21],type='prob')
Validate_Pred=prediction(Pred_Validate_Prob[,2],GC_Validate$good_bad)
Perf_Validate=performance(Validate_Pred,"tpr","fpr")
plot(Perf_Validate,col="black",lty=2,add=T)

Pred_Test_Class = predict(GC_rpart_prune,newdata=GC_Test[,-21],type='class')
Pred_Test_Prob = predict(GC_rpart_prune,newdata = GC_Test[,-21],type='prob')
Test_Pred = prediction(Pred_Test_Prob[,2],GC_Test$good_bad)
Perf_Test = performance(Test_Pred,"tpr","fpr")
plot(Perf_Test,col="red",lty=2,add=T)

legend(0.5,0.5,c("Train Curve","Validate Curve","Test Curve"),col=c("black","black","red"),pch="-")
title(main="Improving a Classification Tree with Pruning")
###cp=0.02是根据复杂性参数和相对误差选定的，运行 plotcp(GC_rpart)，结果图证明选择cp=0.02是合理的###

###使用minsplit和cp选项，我们已经设法得到了一个缩减的规律集，在这个意义下，拟合的模型没有了过度拟合的问题###
###受试工作者特征曲线表明，验证部分的结果有了可观的改进。###
###另外，验证和测试部分得到了类似的受试工作者特征曲线，因此，对GC_rpart使用GC_rpart_prune或者GC_rpart_minsplit是合理的###



###bagging算法###
library(ipred)
library(RSADBE)
data(GC)###德国信用数据###
###B=200时，使用bagging算法拟合数据GC###
GC_bagging = bagging(good_bad~.,data=GC,coob=F,nbagg=200,keepX=T)
###B个树的展示结果###
for(i in 1:200){
  plot(GC_bagging$mtrees[[i]]$btree);
  text(GC_bagging$mtrees[[i]]$btree,pretty=1,use.n=T)
}
###用predict预测所有观测值的类概率###
GCB_Margin = round(predict(GC_bagging,type="prob")*200,0)
head(GCB_Margin)##得到首次的6个预测类
###得到整个bagging算法的最低收益值###
mean(pmax(GCB_Margin[,1],GCB_Margin[,2]) pmin(GCB_Margin[,1],GCB_Margin[,2]))/200##有问题？##
###建立out-of-bag bagging 模型###
GC_bagging_oob = bagging(good_bad~.,data=GC,coob=T,nbagg=200,keppX=T)
GC_bagging_oob$err##误差率##




###建立随机森林树来研究德国信用数据问题###
library(randomForest)
library(RSADBE)
data(GC)
GC_RF = randomForest(good_bad~.,data=GC,keep.forest = T,ntree = 500) 
###作out-of-bag误差率树的数目图：###
plot(1:500,GC_RF$err.rate[,1],"l",xlab="No.of.Trees",ylab="OOB Error Rate")
##对不同的树选择不同的协变量(特征)。那么我们关注的是哪个变量是显著的。使用varImpPlot可以得到显著的变量##
varImpPlot(GC_RF)##这样我们就可以看到那些关联性较高的变量##




###低出生体重数据的随机森林###
###我们将用到线性回归模型、logistic回归模型，以及分类树与回归树(CART)##
#1.向R中读入数据集：data(lowbwt)#
data(lowbwt)
#2.使用diag.panel,lower.panel和upper.panel可视化数据集：#
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(lowbwt,diag.panel = panel.hist,lower.panel = panel.smooth,upper.panel = panel.cor)
###因为相关性很弱，因此回归模型不适合。让我们进行检查###
#3.构造（子）数据集，用于回归与分类树问题#
LOW=lowbwt[,-10]
BWT=lowbwt[,-1]
#4.首先检查线性回归模型是否合适#
BWT_lm = lm(BWT~.,data=BWT)
summary(BWT_lm)
##由于R^2的值较小，因此我们不能使用该模型，下面检验logistic回归模型##
#5.拟合logistic回归模型#
LOW_glm = glm(LOW~.,data=LOW)
summary(LOW_glm)
#6.logistic回归模型的Hosmer-Lemeshow拟合优度检验：#
hosmerlem <- function(y,yhat,g=10){
  cutyhat = cut(yhat,breaks = quantile(yhat,probs = seq(0,1,1/g)),include.lowest = T)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P= 1- pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}
hosmerlem(LOW_glm$y,fitted(LOW_glm))
##得到的p值为0.7813，这表明在拟合值和观测值之间没有显著差异。因此logsitic回归模型是一个很好的拟合#
##我们仍将继续，对该问题使用分类与回归树模型进行拟合##
##注意到回归系数的估计值不大，因此我们不需要检验岭回归问题##
#7.使用函数rpart拟合分类树#
LOW_rpart = rpart(LOW~.,data=LOW)
plot(LOW_rpart)
text(LOW_rpart,pretty=1)
#8.使用asRules(LOW_rpart)得到分类树的规律：#
asRules(LOW_rpart)
##你可以看到，这些规则对于做手术的医生而言非常重要##
#9.使用函数bagging,得到bagging方法的误差率：##
LOW_bagging = bagging(LOW~.,data=LOW,coob=T,nbagg=50,keepX=T)
LOW_bagging$err#误差率约为0.45#
#10.随机森林树#
library(randomForest)
LOW_RF = randomForest(LOW~.,data=LOW,keep.forest=T,ntree=50)##有问题？##
# LOW_RF$err.rate
##重复上述步骤，构造150个树，并检查误差率是否减少
# LOW_RF = randomForest(LOW~.,data=LOW,keep.forest=T,ntree=150)
# plot(1:150,LOW_RF$err.rate[,1],"l",xlab="No.of.Trees",ylab="OOB Error Rate")







