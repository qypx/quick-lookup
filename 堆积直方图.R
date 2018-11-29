
年份<-c(rep(2010,58),rep(2011,63),rep(2012,70),rep(2013,75))
票房组别<-rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(3,13,42,2,14,47,3,16,51,7,24,44))
x3=cbind(年份,票房组别)
y=table(x3[,2],x3[,1])


barplot(y,beside = T,col=c("lightgreen","grey","pink"),xlab="年份",ylab="频数")
legend("topright",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)
 

x1=rep(c("1~13","14~26","27~39","40~53"),c(62,54,85,65))
x2=rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(2,19,41,2,10,42,4,19,62,7,19,39))

x=cbind(x1,x2)
b=table(x[,2],x[,1])
barplot(b,beside = F,col=c("lightgreen","grey","pink"),xlab="放映周数",ylab="频数")
legend("topright",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)
barplot(b)


x1=rep(c("C","G","L","S"),c(57,59,68,82))
x2=rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(4,11,42,2,13,44,0,10,58,9,33,40))

x=cbind(x1,x2)
b=table(x[,2],x[,1])
barplot(b,beside = T,col=c("lightgreen","grey","pink"),xlab="宣发公司",ylab="频数")
legend("topright",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)


###
x1=rep(c("076~095","096~115","116~135","136~156"),c(78,128,54,6))
x2=rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(0,7,71,3,34,91,10,20,24,2,2,2))

x=cbind(x1,x2)
b=table(x[,2],x[,1])
barplot(b,beside = T,col=c("lightgreen","grey","pink"),xlab="放映时长",ylab="频数")
legend("topright",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)

###
x1=rep(c("0","1"),c(25,241))
x2=rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(6,10,9,9,57,175))

x=cbind(x1,x2)
b=table(x[,2],x[,1])
barplot(b,beside = T,col=c("lightgreen","grey","pink"),xlab="是否改编作品",ylab="频数")
legend("topleft",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)

###
x1=rep(c("0","1"),c(242,24))
x2=rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(13,60,169,2,7,15))

x=cbind(x1,x2)
b=table(x[,2],x[,1])
barplot(b,beside = T,col=c("lightgreen","grey","pink"),xlab="是否真人真事",ylab="频数")
legend("topright",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)

###
x1=rep(c("0","1"),c(240,26))
x2=rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(14,54,172,1,13,12))

x=cbind(x1,x2)
b=table(x[,2],x[,1])
barplot(b,beside = T,col=c("lightgreen","grey","pink"),xlab="是否翻拍作品",ylab="频数")
legend("topright",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)



###
x1=rep(c("0","1"),c(223,43))
x2=rep(c("a高票房组","b中票房组","c低票房组","a高票房组","b中票房组","c低票房组"),c(10,55,158,5,12,26))

x=cbind(x1,x2)
b=table(x[,2],x[,1])
barplot(b,beside = T,col=c("lightgreen","grey","pink"),xlab="是否作品续集",ylab="频数")
legend("topright",legend=c("高票房组","中票房组","低票房组"),fill=c("lightgreen","grey","pink"),cex=0.5)

