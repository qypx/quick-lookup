#### 分类 ####
set.seed(1)
data(PimaIndiansDiabetes2, package = 'mlbench')
data <- PimaIndiansDiabetes2
library(caret)
# 标准化
preProcValues <- preProcess(data[,-9], method = c("center","scale"))
scaledata <- predict(preProcValues, data[,-9])
# YeoJohnson转换（使数据接近正态分布，并减弱异常值的影响）
preProcbox <- preProcess(scaledata, method = c("YeoJohnson"))
boxdata <- predict(preProcbox, scaledata)
# 缺失值插补（装袋算法）
preProcimp <- preProcess(boxdata, method = "bagImpute")
procdata <- predict(preProcimp,boxdata)
procdata$class <- data[,9]

###########################################################################################################################
##### 决策树 ######
# 决策树模型不需要对数据与任何的先验假设,计算速度较快,结果容易解释,稳健性强,对噪声数据和缺失数据不敏感

# 分类回归树方法(CART)是众多树模型算法中的一种,它先从n个自变量中寻找最佳分割变量和最佳分割点,将数据划分为两组.
# 针对分组后数据将上述步骤重复下去,直到满足某种停止条件.

# 建立树模型可分为分类树和回归树两种.
# 分类树用于因变量为分类数据的情况,树的末端为因变量的分类值
# 回归树则可以用于因变量为连续变量的情况,树的末端可以给出相应类别中的因变量描述或预测


# 建立树模式大致分为三个步骤:

# 第一步是对所有自变量和所有分割点进行评估,最佳的选择是使分割后组内的数据纯度更高,即组内数据的目标变量变异更小.
# 这种纯度可通过Gini值或熵Entropy来度量.

# 第二步是对树进行修剪或成为剪枝.之所以要修剪是因为若不加任何限制,模型会产生"过度拟合"问题.
# 通常使用CP参数来对树的复杂度进行控制,使预测误差和树规模都尽可能小.
# CP参数类似于岭回归中的惩罚系数,数字越小模型越偏向于过度拟合.
# 通常做法是先建立一个划分较细较为复杂的树模型,再根据交叉检验方法来估计不同"剪枝"条件下各模型的误差,选择误差最小的树模型.

# 第三步是输出最终结果,进行预测和解释.


# 在R中通常可以用rpart包来实现CART算法,其中重要的参数是cp,它由control进行控制.

library(caret)
library(rpart)
rpartModel <- rpart(class~.,data = procdata, control = rpart.control(cp = 0)) 
# cp（模型复杂度）参数设置为0是为了让模型变得复杂，以方便后面演示剪枝处理

# 不同的模型复杂度下会有不同的预测误差,模型复杂度可以由CP参数代表,预测误差是由xerror表示,即交叉检验的模型预测误差.
# 我们可以寻找最小xerror值点所对应的CP值,并由此CP值决定树的大小.
# 根据上面的输出自动求出最小的CP值,再用prune函数对树模型进行修剪.

cptable <- as.data.frame(rpartModel$cptable)
cptable$errsd <- cptable$xerror + cptable$xstd
cpvalue <- cptable[which.min(cptable$errsd), "CP"] # 选择误差最小的树模型

# 剪枝
pruneModel <- prune(rpartModel, cpvalue)

# 使用rpart.plot包来画出树结构图
library(rpart.plot)
rpart.plot(pruneModel)

# rpart模型运行快速,不怕缺失和冗余变量,解释性强,但缺点在于:它是矩形的判别边界,使得精确度不高,对回归问题不太合适.
# 在处理回归问题时,建议使用模型树(model tree)方法,即先将数据切分,再对各组数据进行线性回归.
# party包中的mob函数和RWeka包中的M5P函数可以建立模型树.
# 另一个缺点在于,单个决策树不太稳定,数据微小的变化会造成模型结构变化.树模型还会有变量选择偏向,即会选择那些有取值较多的变量.
# 一种改善的做法是使用条件推断树,即party包中的ctree函数,还可以采用集成学习方法,如随机森林算法.


# 效果的衡量:以决策树为例

# 统计模型可以直接从各项检验的结果判断模型的好坏.但是数据挖掘和机器学习这一类算法模型通常解释性没有这么强,
# 而算法模型通常都是为了预测,那么一个很现实的解决办法就是通过比较真实值和预测值之间的差异来衡量模型的效果.
# 也就是构建一个表格来评价二元分类器的预测效果.所以的训练数据都会落入这个两行两列的表格中,
# 对角线上的数字代表了预测正确的数目,同时可以相应算出TPR(真正率获称为灵敏度)和TNR(真负率或称为特异度),这个表格又称之为混淆矩阵.

pre <- predict(pruneModel, procdata, type = 'class')
(preTable <- table(pre, procdata$class))
# pre   neg pos
#   neg 444  53
#   pos  56 215
# 纵轴是预测值,横轴是真实值.
(accuracy <- sum(diag(preTable))/sum(preTable))# 准确率
# 0.8580729
preTable[1,1]/sum(preTable[,1]) # 灵敏度(TPR, 在真实为阴性条件下预测正确的比率)
# 0.888
preTable[2,2]/sum(preTable[,2]) # 特异度(TNR, 在真实为阳性条件下预测正确的比率)
# 0.8022388

# 本着宁可错杀不可放过的思路，在建模函数中增加成本矩阵的参数设置，将未诊断出有病的成本增加到5倍
rpartModel <- rpart(class~., data = procdata, control = rpart.control(cp = 0.01), 
                    parms = list(loss = matrix(c(0,5,1,0),2)))
pre <- predict(rpartModel, procdata, type = 'class')
(preTable <- table(pre, procdata$class))
# pre   neg pos
#   neg 269   8
#   pos 231 260
(accuracy <- sum(diag(preTable))/sum(preTable))
# 0.6888021
preTable[1,1]/sum(preTable[,1]) # 灵敏度
# 0.538
preTable[2,2]/sum(preTable[,2]) # 特异度
# 0.9701493
# 这样模型特异度增加到0.97,但牺牲了灵敏度,且总体准确率也下降了.

# 需要注意的是上面的预测仍然是针对训练集进行的，往往会高估模型的准确性,或者说形成了过度拟合的情况.
# 处理过度拟合的的问题通常有以下几个思路:
# 其一是保留数据,例如多重交叉检验;
# 其二是用正则化方法对模型的复杂度进行约束,例如岭回归(ridge regression)和套索方法(LASSO).

# 在机器学习的实践中,衡量模型效果最常用的方法是多重交叉检验.
# 以十重交叉检验为例,将所有数据随机分为十组,第一次训练对象是1~9组,检验对象是第10组,第二次训练是2~10组,检验对象第1组,然后依次轮换.
# 若还需要调校参数,一种典型的做法是先将数据划分为训练集和检验集.
# 训练集中用多重交叉检验来选择调校模型,参数确定后使用整体训练集得到最终模型,再用检验集来观察判断最终模型的效果.

# 十重交叉验证，使用准确率为度量指标
num <- sample(1:10, nrow(procdata), replace = T)
res <- array(0, dim = c(2,2,10))
n <- ncol(procdata)
for(i in 1:10){
  train <- procdata[num!=i,]
  test <- procdata[num==i,]
  model <- rpart(class~., data = train, control = rpart.control(cp = 0.1))
  pre <- predict(model, test[,-n], type = 'class')
  res[,,i] <- as.matrix(table(pre, test[,n]))
}
table <- apply(res,MARGIN = c(1,2),sum)
#      [,1] [,2]
# [1,]  426  119
# [2,]   74  149
sum(diag(table))/sum(table)
# 0.7486979
# 经过多次的验证后,可以认为消除了单次建模的偶然性,那么模型的准确率实际上应该是0.75,可见和之前的0.86有较大的差距.

# 也可使用caret包中的train函数来建模并自动实施10重交叉验证.
# 给定一个CP参数,进行一次10重交叉检验会得到一个模型的结果,我们输入10各不同的CP参数,分别进行交叉检验可以得到10个对应的结果.
# 这样可以观察不同参数对模型的结果影响,进而确定最优的参数.
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# 使用trainControl设置检验的控制参数，确定为10重交叉验证，反复进行3次.目的是为了减少模型评价d不稳定性,这样得到30次检验结果.
tunedf <- data.frame(.cp=seq(0.001,0.1,length=10))
# 在参数调校中，确定CP参数从0.001开始到0.1结束
treemodel <- train(x = procdata[,-9], y = procdata[,9], method = 'rpart',
                   trControl = fitControl, tuneGrid = tunedf)
# 训练时使用模型为rpart建模函数，用10个不同的参数来进行交叉验证
plot(treemodel)
# 由图可以看到CP参数在0.045附近可以得到最优的预测准确率，我们就可以用这个参数对整个训练集或对未来的新数据进行预测
# caret包中的predict.train函数会自动选择最优参数，并对整个训练集进行一次预测
densityplot(treemodel)
# 准确率在0.77左右
###########################################################################################################################



###########################################################################################################################
###### 贝叶斯分类 #######
# 使用klaR中的NaiveBayes函数，该函数可以输入先验概率，另外在正态分布基础上增加了核平滑密度函数
# 为使用该函数建模并通过图形查看变量glucose的影响
library(klaR)
nbModel <- NaiveBayes(class~., data = procdata, usekernel = F, fL = 1) 
# usekernel确定是否使用核密度平滑，否则使用正态分布
# fL设置平滑系数，这是为了防止某个后验概率计算为0的结果
# (fL指定是否进行拉普拉斯修正，默认情况下不对数据进行修正，当数据量较小时，可以设置该参数为1，即进行拉普拉斯修正。)
plot(nbModel, vars = "glucose", legendplot = T)
# 葡萄糖浓度越高，患病概率越大

#可以直接使用caret包来实施朴素贝叶斯分类，并进行多重交叉检验
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
nbModel <- train(x = procdata[,-9], y = procdata[,9], method = "nb", trControl = fitControl,
                 tuneGrid = data.frame(.fL = 1, .usekernel = T))
# densityplot(nbModel)
# 上面我们进行的是三次10重交叉检验，从图中看到其准确率在0.75左右，也可使用nbmodels$resample调出具体结果数据
#######################################################################################################


#########################################################################################################
##### 最近邻分类(KNN) #######
# KNN算法的优势在于算法简单,稳健性强,可构成非线性的判别边界,模型参数简单,只有距离测度和k参数.
# 其弱点在于计算量较大,对异常点和不平衡数据都较为敏感

# class包中的knn函数可以实行基本的KNN算法,其参数即是近邻个数k
# 下面使用caret包来调校参数,找出最优的k值
library(caret)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunedf <- data.frame(.k = seq(3,20,by=2))
knnmodel <- train(x = procdata[,-9], y = procdata[,9], method = "knn", trControl = fitControl,
                  tuneGrid = tunedf)
plot(knnmodel)
# 由图可见，K取17时模型预测准确率最高

# 用createDataPartition将数据进行划分，分成75%的训练样本和25%检验样本
inTrain = createDataPartition(procdata[,9], p = 3/4, list = F)
trainx = procdata[inTrain,-9]
testx = procdata[-inTrain,-9]
trainy = procdata[inTrain,9]
testy = procdata[-inTrain,9]
library(class)
knnModel <- knn(trainx, testx, trainy, k = 7)
(preTable <- table(knnModel, testy))
(accuracy <- sum(diag(preTable))/sum(preTable))

# 对于KNN算法,还有一个kknn包值得关注,它对于基本的knn函数有很大程度的扩展.可以结合核函数,利用距离进行加权计算
###########################################################################################################

#############################################################################################################
####### 神经网络分类 ########
# BP神经网络的学习规则是使用最速下降法，通过反向传播来不断调整网络的权值和阈值,使网络的误差平方和最小.

# nnet包可以实现BP神经网络分类suanfa,其中的重要参数有
# size:隐层神经元个数,数字越大模型越复杂
# decay: 学习速率,是为了避免过度拟合问题,这个值一般在0到0.1之间
# linout:隐层到输出层的函数形式,若是回归问题则设置为TRUE,表示线性输出,若是分类问题则设置为FALSE,表示非线性输出

# caret包中的avNNet函数对nnet包有所改进,它使用同一个BP神经网络模型，而使用不同的初始随机种子,最后预测时经行综合预测,
# 这样可以一定程度上避免模型训练时陷入局部最优解

library(caret)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunedf <- expand.grid(.decay = 0.1, .size = 5:10, .bag = T)
nnetmodel <- train(class~., data = procdata, method = "avNNet", trControl = fitControl,
                   trace = F, linout = F, tuneGrid = tunedf)
# 上面我们控制decay参数为常值,这是为了避免调校参数过多,计算时间过长
# 上面的结果显示BP神经网络的参数在隐层神经元为个时,准确率最高

# BP神经网络的特点是可以拟合任何一种函数,容易陷入局部极值,导致过度拟合,且计算量大.
# 一种解决方法是在数据预处理时使用PCA,在train函数的method中使用pacNNet即可直接实施基于PCA的神经网络.
# 但它可以处理冗余变量,因为冗余变量的权重在学习训练中会变得很小
################################################################################################################


################################################################################################################
###### 支持向量机分类########
# 理解SVM有四个关键概念:分离超平面、最大边缘超平面、软边缘、核函数
# 分离超平面:线性的决策边界
# 最大边缘超平面:尽量和两边保持距离,以留足余量,减小泛化误差,保证稳健性.在数学上是一个二次规划问题
# 软边缘可以破例允许个别样本跑到其他类别的地盘上去,但要使用参数来权衡两段,这种参数就是对错误分类的惩罚程度C.
# SVM提出一种思想,就是将原始数据映射到高维空间去,高维空间中的数据变得稀疏,有利于"分清敌我",映射的方法就是使用"核函数"
# 选择合适的核函数以及软边缘参数C就是训练SVM的重要因素.一般来讲,核函数越复杂模型越偏向于拟合过度.参数C可看作
# 是LASSO算法中的lambda的倒数,C越大模型越偏向于拟合过度,反之则拟合不足.实践中仍然是使用交叉检验来确定参数.

# 常用的核函数
# Linear:线性核函数,使用它的话就称为线性向量机,效果基本等价于Logistic回归.但它可处理变量极多的情况,例如文本挖掘.
# polynomial:多项式核函数,适用于图像处理问题.
# Radial basis:高斯核函数,最流行易用的选择.参数包括sigma,其值若设置过小,会有过度拟合出现,但这个参数也可自动计算出最优值
# sigmoid:反曲核函数,多用于神经网络的激活函数.

# R语言可用e1071包中的svm函数建模,而另一个kernlab包中则包括了更多的核方法函数
# 我们主要使用其中的ksvm函数,来说明参数C的作用和核函数的选择.
# 我们使用人为构造的一个线性不可分的数据集LMdata作为例子,专门用来测试SVM算法

# 首先使用线性核函数,其参数C取为0.1
data(LMdata, package = "rinds")
library(kernlab)
model1 <- ksvm(y~., data = LMdata$SVM, kernel = "vanilladot", C=0.1)
plot(model1, data = LMdata$SVM)
# 图为各样本的判别值等高线图
# 可以清楚的看到决策边界为线性,中间的决策边缘显示为白色区域,有相当多的样本落入此区域

# 为了更好的拟合,加大C的取值
model2 <- ksvm(y~., data = LMdata$SVM, kernel = "vanilladot", C=10)
plot(model2, data = LMdata$SVM)
# 加大C后决策边缘缩窄,也使误差减少,但仍有个别样本未被正确分类

# 换用高斯核函数
model3 <- ksvm(y~., data = LMdata$SVM, kernel = "rbfdot", C=1)
plot(model3, data = LMdata$SVM)
# 这样得到了非线性决策边界,所有的样本都得到了正确的分类

# 实际的应用中,为了寻找最优参数我们用caret包来配合建模
# 我们仍使用多重交叉检验来评价模型
library(caret)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
tunedf <- data.frame(.C=seq(0,1,length = 11))
svmmodel <- train(class~., data = procdata, method = "svmRadialCost", trControl = fitControl, tuneGrid = tunedf)
plot(svmmodel)

# SVM的特点在于它可以发现全局最优解,这不同于决策树或神经网络模型.
# 它可以用参数来控制过度拟合问题,并通过选择核函数来处理不同的问题
# 当数据变量较多时,可以先尝试使用线性核,例如在生物信息和文本挖掘方面,当变量较少时,可以考虑优先使用高斯核.
####################################################################################################################


###################################################################################################################
###### 随机森林 ##########

# 集成学习是试图通过连续调用单个学习算法,获得不同的模型,
# 然后根据规则组合这些模型来解决同一个问题,可以显著的提高学习系统的泛化能力.
# 组合多个模型预测结果主要采用加权平均或投票的方法

# 随机森林计算步骤:
# 从原始训练样本中随即又放回地抽出N个样本,从解释变量中随机抽取M各变量,
# 依据上述得到的子集实施CART方法(无需剪枝),从而形成一个单独的决策树,
# 重复上面步骤X次,就构建了有X棵树的随机森林模型.
# 在对新数据进行预测分类时,由X棵树分别预测,以投票方式综合最终结果

# R语言中的randomForest包可以实施随机森林算法
# 其重要参数有两个,mtry表示在抽取变量时的抽取数目M,ntree表示迭代次数,即森林中决策树的数目;
# 一般缺省的mtry是全部变量数的开方数,ntree是500.

library(caret)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
rfmodel <- train(class~., data = procdata, method = "rf", trControl = fitControl, tuneLength = 5)
plot(rfmodel)
# mtry最佳值为5

# 随机森林还可用来判断变量的重要程度
# 由于决策树是根据不同变量来分割数据,所以一棵树中能进行正确划分的变量就是最重要的变量
# 随机森林可以根据置换划分变量对分类误差的影响来判断哪些变量是比较重要的.
# randomForest包中的importance函数能返回各变量的重要程度
# varImpplot函数可以用图形方式加以展现
# partialPlot函数则能呈现变量的偏效应
# rfcv函数用来得到最优的变量数目

varImpPlot(rfmodel$finalModel)
partialPlot(rfmodel$finalModel, procdata[,-9], "mass", which.class = "pos")
# 从以上两个图可以观察到,glucose是影响糖尿病发生最重要的变量,而随着体重mass增加,患病风险也在增加

# 随机森林的特点是准确率很高、不会形成过拟合;速度快,能够处理大量数据,方便并行处理;能处理很高维度的数据,不用做特征选择.