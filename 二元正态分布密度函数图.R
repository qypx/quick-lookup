###二元正态分布密度函数图
sigma1=matrix(c(1,0,0,1),2);sigma1
#mu=0
f1=function(x,y){
  (1/(2*pi*sigma1[1,1]*sigma1[2,2]*sqrt(1-(pho1)^2)))*exp(-(1/(2*(1-pho1^2)))*(((x/sigma1[1,1])^2)-2*pho1*x/sigma1[1,1]*y/sigma1[2,2]+(y/sigma1[2,2])^2))  
}
x<-y<-seq(-4,4,length=20)
z1=outer(x,y,f1)


persp(x,y,z1,theta=45,phi=15,col='lightblue')#密度函数图