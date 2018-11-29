###ROC曲线
##分类器算法最后都会有一个预测精度，而预测精度都会写一个混淆矩阵，所有的训练数据都会落入这个矩阵中，
##而对角线上的数字代表了预测正确的数目，即True Positive+True Nagetive
##为了更好的衡量ROC所表达结果的好坏，Area Under Curve（AUC）被提了出来，
##简单来说就是曲线右下角部分占正方形格子的面积比例。该比例代表着分类器预测精度。

##法一 ROC包
##ROCR包，它不仅可以用来画图，还能计算ROC曲线下面积AUC，以评价分类器的综合性能，
##该数值取0-1之间，越大越好。
library(ROCR)  
pred <- prediction(pre,newdata$y)  
performance(pred,'auc')@y.values #AUC值  
perf <- performance(pred,'tpr','fpr')  
plot(perf) 
#注意：其中pre是分类器预测的模型，而newdata$y是实际值。


##法二 pROC包
##ROCR包画图函数功能比较单一，笔者比较偏好使用功能更强大的pROC包。
##它可以方便比较两个分类器，还能自动标注出最优的临界点，图看起来也比较漂亮
library(pROC)  
modelroc <- roc(newdata$y,pre)  
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),  
     grid.col=c("green", "red"), max.auc.polygon=TRUE,  
     auc.polygon.col="skyblue", print.thres=TRUE)  


##EXAMPLE
library("pROC")
data(aSAH)  
# Build a ROC object and compute the AUC  
roc(aSAH$outcome, aSAH$s100b)  
plot(roc(aSAH$outcome, aSAH$s100b))

# Build a ROC object and compute the AUC, draw ROC, print AUC and the best THRESHOLDS  
roc(aSAH$outcome, aSAH$s100b, plot=TRUE, print.thres=TRUE, print.auc=TRUE)
