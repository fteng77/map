rm(list=ls())
gc()
library("sp")
library("maptools")
library(maps)
library(mapproj)
library(plyr)
library(tidyverse)

####1. 中国地图，参考https://zhuanlan.zhihu.com/p/26708368
china_map <- readShapePoly("D:/dizhen/bou2_4p.shp")       # 读取地图空间数据
china<- fortify(china_map)
china.plot<-ggplot()+
  geom_polygon(data=china, aes(x=long, y=lat, group=group),fill="grey95", colour="grey60",size=0.25)+
  coord_map("polyconic")

setwd("D:/dizhen")
dizhen<-read.csv("dizhen.all.new2.csv",header=T,stringsAsFactors = F)
dizhen<-dizhen[,-1]
dizhen3<-filter(dizhen,zhenji>=3)
dizhen4<-filter(dizhen,zhenji>=4)
dizhen5<-filter(dizhen,zhenji>=5)
zhenji.hist<-hist(dizhen$zhenji)

china.plot+geom_point(data=dizhen4,aes(x=jingdu,y=weidu,size=zhenji,color=zhenji,fill=I("blue")),alpha=I(1/12),shape=I(21))
#reference: geom_point(data=china_data, aes(x = jd,y = wd),size=4,fill="black",  colour="white")+ #散点图

####2. 分省地图，参考http://blog.sina.com.cn/s/blog_6bc5205e0102vmgq.html
CHN_adm2 <- readShapePoly("D:/dizhen/shengditu/CHN_adm2.shp")        #读取市级中国地图
CHN <- fortify(CHN_adm2)     #转化为数据框     
x <- CHN_adm2@data          #读取行政信息
xs <- data.frame(x,id=seq(0:344)-1)          #总共345行
china_map_data <- join(CHN, xs, type = "full")          #合并形状数据与行政提示：Joining by: id
zhejiang <-subset(china_map_data,NAME_1=="Zhejiang")          #取出zhejiang的子集
ningbo<-subset(china_map_data,NAME_2=="Ningbo")
weifang<-subset(china_map_data,NAME_2=="Weifang")

zhejiang.plot<-ggplot()+
  geom_polygon(data=zhejiang, aes(x=long, y=lat, group=group),fill="grey95", colour="grey60",size=0.25)+
  coord_map("polyconic")
ningbo.plot<-ggplot()+
  geom_polygon(data=ningbo, aes(x=long, y=lat, group=group),fill="grey95", colour="grey60",size=0.25)+
  coord_map("polyconic")
weifang.plot<-ggplot()+
  geom_polygon(data=weifang, aes(x=long, y=lat, group=group),fill="grey95", colour="grey60",size=0.25)+
  coord_map("polyconic")

zhejiang.web<-"https://raw.githubusercontent.com/fteng77/dizhen/master/zj.dizhen.fujin.csv"
zhejiang.dizhen<-read.csv(zhejiang.web,header=T)
zhejiang.plot+geom_point(data=zhejiang.dizhen,aes(x=jingdu,y=weidu,size=zhenji),fill="red",alpha=0.6,shape=21,colour="red")

ningbo.dizhen<-filter(zhejiang.dizhen,shi=="宁波市")
ningbo.plot+geom_point(data=ningbo.dizhen,aes(x=jingdu,y=weidu,size=zhenji),fill="red",alpha=0.8,shape=21,colour="red")

weifang<-subset(china_map_data,NAME_2=="Weifang")
weifang.plot<-ggplot()+
  geom_polygon(data=weifang, aes(x=long, y=lat, group=group),fill="grey95", colour="grey60",size=0.25)+
  coord_map("polyconic")
weifang.dizhen<-filter(dizhen,shi.1=="潍坊市")
weifang.plot+geom_point(data=weifang.dizhen,aes(x=jingdu,y=weidu,size=zhenji),fill="red",alpha=0.8,shape=21,colour="red")

ggsave("D:/dizhen/ningbo.png",width=5,height = 5)
