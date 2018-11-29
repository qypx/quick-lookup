x=rnorm(15)
y=x+rnorm(15)##rnorm(15)相当于是误差项##
l=lm(y~x)
new=data.frame(x=seq(-3,3,0.5))
pi=predict(l,new,interval="prediction")##预测区间##
ci=predict(l,new,interval="confidence")##估计区间##
library(graphics)
matplot(x=new$x,y=cbind(ci,pi[,-1]),lty=c(1,5,5,6,6),col=c(1,2,2,4,4),type="l",
xlab="newx",ylab="predicted y")

 