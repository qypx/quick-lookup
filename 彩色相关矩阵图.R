###²ÊÉ«Ïà¹Ø¾ØÕóÍ¼
data(mtcars)
corr <- cor(mtcars)

library(corrplot)
# require(corrplot)
corrplot(corr)
corrplot(corr,method="shade",shade.col=NA,tl.col="black",tl.srt=45)

# corrplot(corr,method="circle")
# corrplot(corr,method="square")
# corrplot(corr,method="ellipse")
# corrplot(corr,method="number")
# corrplot(corr,method="shade")
# corrplot(corr,method="color")
# corrplot(corr,method="pie")


