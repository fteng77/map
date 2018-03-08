rm(list=ls())
gc()
library(tidyverse)
data("diamonds")
set.seed(1410)
#summary(sample(nrow(diamonds),100))
dsmall<-diamonds[sample(nrow(diamonds),100),]

###2.1 qplot基本用法
qplot(carat,price,data=diamonds)
qplot(log(carat),log(price),data=diamonds)
qplot(carat,x*y*z,data=diamonds)

###2.2 qplot的颜色大小形状
qplot(carat,price,data=dsmall,color=color)
qplot(carat,price,data=dsmall,shape=cut)
qplot(carat,price,data=dsmall,size=x*y*z)
qplot(log(carat),log(price),data=dsmall,size=I(2))
#qplot(log(carat),log(price),data=dsmall,size=x*y*z,color=color)+geom_smooth(method="lm")
qplot(carat,price,data=diamonds,alpha=0.1)
qplot(carat,price,data=diamonds,alpha=I(1/10))
qplot(carat,price,data=diamonds,alpha=I(1/100))

###2.3 qplot的几何对象（geom）
qplot(carat,price,data=dsmall,geom=c("point","smooth","line"))
qplot(carat,price,data=dsmall,geom=c("smooth","point"))

#加载mgcv,geom
library(mgcv)
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="gam",fomula=y~s(x))
qplot(carat,price,data=dsmall,geom=c("point","smooth"),method="lm",fomula=y~ploy(x,2))

#箱线图和扰动点图
qplot(color,price/carat,data=diamonds,geom="jitter")
qplot(color,price/carat,data=diamonds,geom="jitter",alpha=I(1/10))
qplot(color,price/carat,data=diamonds,geom="boxplot")
qplot(color,price/carat,data=diamonds,geom="jitter",colour=cut,size=I(1),fill=I("red"),alpha=I(1/5))
qplot(color,price/carat,data=diamonds,geom="boxplot",fill=I("blue"),colour=I("red"))

#分面
qplot(carat,data=diamonds,facets=color~.,geom="histogram",binwidth=0.1,xlim=c(0,3),fill=I("red"))
qplot(carat,data=dsmall,facets=color~cut,geom="histogram",binwidth=0.5,main="ggplot2",xlab="x",ylab="y")

