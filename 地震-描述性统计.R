rm(list=ls())
gc()
library("sp")
library("maptools")
library(maps)
library(mapproj)
library(plyr)
library(tidyverse)

##0.建立函数multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#as.data.frame(table(dizhen$year))

##1. 数据分析
#1.0整理数据
setwd("D:/dizhen")
dizhen<-read.csv("dizhen.all.new2.csv",header=T,stringsAsFactors = F)
dizhen<-dizhen[,-1] #删掉第一列
dizhen<-mutate(dizhen,earthquake.mag=zhenji)  #为了显得高大上，把zhenji改为earthquake.mag
#dizhen<-filter(dizhen,year>=1970&year<2018)
qplot(factor(year),zhenji,data=dizhen,geom="boxplot",color="black")+theme(axis.text.x = element_text(size=7,angle=45))+theme(legend.position = "none")

# dizhen3<-filter(dizhen,zhenji>=3) #三级以上的地震
# dizhen4<-filter(dizhen,zhenji>=4) #四级以上的地震
# dizhen5<-filter(dizhen,zhenji>=5) #五级以上的地震

dizhen.nosheng<-filter(dizhen,is.na(as.numeric(sheng))==F) #提取没有省名的记录
max(dizhen.nosheng$zhenji)
nrow(dizhen.nosheng)
nrow(dizhen.nosheng)/nrow(dizhen)

#给没有省名的数据画地图
china_map <- readShapePoly("D:/dizhen/bou2_4p.shp")       # 读取地图空间数据
china<- fortify(china_map)
china.plot<-ggplot()+
  geom_polygon(data=china, aes(x=long, y=lat, group=group),fill="grey95", colour="grey60",size=0.25)+
  coord_map("polyconic")+
  labs(xlab="jingdu",ylab="weidu")
nosheng.plot<-china.plot+
  geom_point(data=dizhen.nosheng,aes(x=jingdu,y=weidu,size=earthquake.mag),alpha=I(1/5),color="red",fill="red",shape=I(21))+
  theme(legend.position = "bottom")
nosheng.plot
#结论：由于地图软件的限制，未出现省名的地点都集中于临海海域和边境地区，且只占总数只有千分之一，故这部分数据保留，但在省级分析中不再涉及。


#1.1 中国地震总体情况：每年平均发生次数（按年份、震级、按省）
dizhen.year<-filter(dizhen,is.na(year)==F&year!=2018)
qplot(as.factor(year),zhenji,data=dizhen.year,geom="boxplot",xlab="年份",ylab="发生次数")
#qplot(as.factor(year),zhenji,data=dizhen.year,geom="jitter",alpha=I(1/10),color=I("red"),xlab="年份",ylab="发生次数")
qplot(as.factor(year),data=dizhen.year,geom="bar",xlab="年份",ylab="发生次数")+labs(title="zheji")+theme(axis.text.x = element_text(size=7,angle=45))  
qplot(as.factor(year),data=dizhen.year,geom="bar",xlab="年份",ylab="发生次数")+theme(axis.text.x = element_text(size=7,angle=45))  

dizhen.year.3<-filter(dizhen.year,zhenji>=3)
dizhen.year.4<-filter(dizhen.year,zhenji>=4)
dizhen.year.5<-filter(dizhen.year,zhenji>=5)
dizhen.year.6<-filter(dizhen.year,zhenji>=6)

(x1<-qplot(factor(year),data=dizhen.year.3,geom="bar",xlab="年份",ylab="震级超过3级的发生次数")+theme(axis.text.x = element_text(size=7,angle=45)) )
(x2<-qplot(factor(year),data=dizhen.year.4,geom="bar",xlab="年份",ylab="震级超过4级的发生次数")+theme(axis.text.x = element_text(size=7,angle=45)) )
(x3<-qplot(factor(year),data=dizhen.year.5,geom="bar",xlab="年份",ylab="震级超过5级的发生次数")+theme(axis.text.x = element_text(size=7,angle=45)) )
(x4<-qplot(factor(year),data=dizhen.year.6,geom="bar",xlab="年份",ylab="震级超过6级的发生次数")+theme(axis.text.x = element_text(size=7,angle=45)) )

qplot(zhenji,data=dizhen.year.4,geom="histogram",xlab="")+ facet_wrap( ~ factor(year), ncol=10)#分页，参考：http://www.mamicode.com/info-detail-1272791.html



#1.2 浙江

#1.3 浙江各地市

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