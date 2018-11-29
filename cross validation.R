Z=10
dat[,7]
j
rep(1:Z,ceiling(j/Z))
w=dat
w[,7]=factor(w[,7])
D=7

Fold=function(Z=10,w,D,seed=7777){
  n=nrow(w)
  d=1:n
  dd=list()
  e=levels(w[,D]);T=length(e)#因变量T类
  set.seed(seed)
  for(i in 1:T){
    d0=d[w[,D]==e[i]];j=length(d0)
    ZT=rep(1:Z,ceiling(j/Z))[1:j]
    id=cbind(sample(ZT,length(ZT)),d0)
    dd[[i]]=id
  }
  mm=list()
  for(i in 1:Z){u=NULL;
  for( j in 1:T) u=c(u,dd[[j]][dd[[j]][,1]==i,2])
  mm[[i]]=u
  }
  return(mm)
}
  
mm=Fold(Z,w,D)
ERROR=c()
for(i in 1:Z){
  m=mm[[i]]
  probit=polr(as.factor(逾期状态)~性别+使用率+信用卡额度+factor(住房贷款月供)+信用卡开户数+factor(历史逾期次数),
              method="probit",Hess=T,  data=dat[-m,])
  ERROR[i]=sum(dat[m,D]!=predict(probit,dat[m,]))/length(m)
  
}
ERROR
mean(ERROR)
