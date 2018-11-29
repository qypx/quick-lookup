setwd("E:/新建文件夹 (2)/商务统计/data/第4讲：定序回归/")
dat=read.csv("贷款逾期.csv")
attach(dat)
dat[住房贷款月供!=0,]$住房贷款月供<-1
dat[历史逾期次数!=0,]$历史逾期次数<-1
dat[信用卡开户数==0,]$信用卡开户数<-"1张"
dat[信用卡开户数==1|信用卡开户数==2,]$信用卡开户数<-"2-3张"
dat[信用卡开户数!=0 & 信用卡开户数!=1 & 信用卡开户数!=2 ,]$信用卡开户数<-"3张以上"
detach(dat)
dat$住房贷款月供=as.factor(dat$住房贷款月供)
dat$历史逾期=as.factor(dat$历史逾期次数)
dat$信用卡开户数=as.factor(dat$信用卡开户数)

dat$信用卡额度=(dat$信用卡额度)/10000
dat=dat[,-5]



library(caret)
folds=createFolds(y=dat$逾期状态,k=10)

Z=10;D=6#Z为折数，D为因变量的位置（第几个变量）
ERROR=c()
for(i in 1:Z){
  m=folds[[i]]
  probit=polr(as.factor(逾期状态)~性别+使用率+信用卡额度+住房贷款月供+信用卡开户数+历史逾期,
              method="probit",Hess=T,  data=dat[-m,])#定序回归
  ERROR[i]=sum(dat[m,D]!=predict(probit,dat[m,]))/length(m)

}
mean(ERROR)#平均错判率





