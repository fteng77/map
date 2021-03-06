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

#as.data.frame(table(dizhen$year))##提取table的数字部分

##1. 数据分析
#1.0整理数据
setwd("D:/dizhen")
dizhen<-read.csv("dizhen.all.new2.csv",header=T,stringsAsFactors = F)
dizhen<-dizhen[,-1] #删掉第一列
dizhen<-mutate(dizhen,earthquake.mag=zhenji)  #为了显得高大上，把zhenji改为earthquake.mag
#dizhen<-filter(dizhen,year>=1970&year<2018)
qplot(factor(year),zhenji,data=dizhen,geom="boxplot",color="black")+theme(axis.text.x = element_text(size=7,angle=45))+theme(legend.position = "none")


qplot(factor(year),zhenji,data=dizhen1,geom="boxplot",color="black",xlab="年份",ylab="震级")+theme(axis.text.x = element_text(size=8,angle=45))+theme(legend.position = "none")
summary(dizhen1)
  
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
  labs(xlab="",ylab="")
nosheng.plot<-china.plot+
  geom_point(data=dizhen.nosheng,aes(x=jingdu,y=weidu,size=earthquake.mag),alpha=I(1/5),color="red",fill="red",shape=I(21))+  theme(legend.position = "bottom")+labs(xlab="",ylab="")
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
x0<-qplot(as.factor(year),data=dizhen.year,geom="bar",xlab="",ylab="次数(大于2级)")+theme(axis.text.x = element_blank(),axis.text.y=element_text(size=7))
x1<-qplot(factor(year),data=dizhen.year.3,geom="bar",xlab="",ylab="次数(大于3级)")+theme(axis.text.x = element_blank(),axis.text.y=element_text(size=7))
x2<-qplot(factor(year),data=dizhen.year.4,geom="bar",xlab="",ylab="次数(大于4级)")+theme(axis.text.x = element_blank(),axis.text.y=element_text(size=7))
x3<-qplot(factor(year),data=dizhen.year.5,geom="bar",xlab="年份",ylab="次数(大于5级)")+theme(axis.text.x = element_text(size=7,angle=45) ,axis.text.y=element_text(size=7))
x4<-qplot(factor(year),data=dizhen.year.6,geom="bar",xlab="年份",ylab="次数(大于6级)")+theme(axis.text.x = element_text(size=7,angle=45) ,axis.text.y=element_text(size=7))

multiplot(x0,x1,x2,x3,ncol=1)


#1.2  
dizhen1<-filter(dizhen,year>=1970&year<2018)
dizhen1.3<-filter(dizhen1,zhenji>=3)
qplot(zhenji,data=dizhen1,geom="histogram",xlab="",bins=10,binwidth=0.5)+  facet_wrap( ~ factor(year), ncol=10)#分页，参考：http://www.mamicode.com/info-detail-1272791.html
qplot(zhenji,data=dizhen1.3,geom="histogram",xlab="",bins=10,binwidth=0.5)+  facet_wrap( ~ factor(year), ncol=10)


#1.3 画图
####1. 中国地图，参考https://zhuanlan.zhihu.com/p/26708368
china_map <- readShapePoly("bou2_4p.shp")       # 读取地图空间数据
china<- fortify(china_map)
china.plot<-ggplot()+
  geom_polygon(data=china, aes(x=long, y=lat, group=group),fill="grey95", colour="grey60",size=0.25)+
  coord_map("polyconic")
china.plot+geom_point(data=dizhen,aes(x=jingdu,y=weidu,size=earthquake.mag),alpha=I(1/20),color="red",fill="red",shape=I(21))+  theme(legend.position = "bottom")+labs(xlab="",ylab="")

#1.4
dizhen.sheng<-filter(dizhen,is.na(as.numeric(sheng))==T)
lev<-c("北京市", "天津市", "上海市", "重庆市", "河北省", "山西省", "辽宁省", "吉林省", "黑龙江省", "江苏省", "浙江省", "安徽省", "福建省", "江西省", "山东省", "河南省", "湖北省", "湖南省", "广东省", "海南省", "四川省", "贵州省", "云南省", "陕西省", "甘肃省", "青海省", "台湾省", "内蒙古自治区", "广西壮族自治区", "西藏自治区", "宁夏回族自治区", "新疆维吾尔自治区", "香港特別行政區", "澳门特别行政区")

sheng.freq<-as.data.frame(table(factor(dizhen.sheng$sheng,levels=lev )))
colnames(sheng.freq)<-c("sheng","freq")
qplot(sheng,freq,data=sheng.freq,geom=c("point","area"))+theme(axis.text.x = element_text(size=8,angle=45) ,axis.text.y=element_text(size=10))+labs(xlab="cishu",ylab="sh")

##1.5 地震次数是否存在acf
acf(as.data.frame(table(factor(dizhen.year.3$year[dizhen.year.3$year>1969&dizhen.year.3$year<2018])))[,2],main="")
dizhen3<-as.data.frame(table(factor(dizhen.year.3$year[dizhen.year.3$year>1969&dizhen.year.3$year<2018])))
colnames(dizhen3)<-c("year","freq")
write.csv(dizhen3,"dizhen.year.csv")

barplot(table(factor(dizhen1.3$sheng,levels=lev)),legend.text = F, xlab="",ylab="")

#按照省份进行统计
qplot(factor(sheng),data=dizhen.sheng,geom="bar",xlab="省份",ylab="发生次数")+theme(axis.text.x = element_text(size=7,angle=90))  
ggplot(data=dizhen.sheng, aes(x=reorder(factor(sheng),rep(-1,length(factor(sheng))),sum)))+geom_bar()+theme(axis.text.x = element_text(size=8,angle=50))  +scale_x_discrete(name='')+ scale_y_discrete(name='次数')


# dizhen.sheng.2<-filter(dizhen.sheng,zhenji>=2&year>1969&year<2018)
# dizhen.sheng.4<-filter(dizhen.sheng.2,zhenji>=4)
# ggplot(data=dizhen.sheng.2, aes(x=reorder(factor(sheng),rep(-1,length(factor(sheng))),sum)))+geom_bar()+theme(axis.text.x = element_text(size=7,angle=50))  +scale_x_discrete(name='')+ scale_y_discrete(name='次数')
# dizhen.table.2<-data.frame(sort(table(factor(dizhen.sheng.2$sheng)),decreasing = T))
# dizhen.table.4<-data.frame(sort(table(factor(dizhen.sheng.4$sheng)),decreasing = T))
# dizhen.table<-merge(x=dizhen.table.2,y=dizhen.table.4,by="Var1",all.x=T)
# colnames(dizhen.table)<-c("sheng","2ji","4ji")

dizhenYear<-filter(dizhen,year>=1970&year<2018)
dizhenYear3<-filter(dizhenYear,zhenji>=3)
dizhenYear4<-filter(dizhenYear,zhenji>=4)
dizhenYear5<-filter(dizhenYear,zhenji>=5)
dizhenYear6<-filter(dizhenYear,zhenji>=6)
dizhenYear7<-filter(dizhenYear,zhenji>=7)

dizhenYearTable<-data.frame(table(factor(dizhenYear$year)))
dizhenYearTable3<-data.frame(table(factor(dizhenYear3$year)))
dizhenYearTable4<-data.frame(table(factor(dizhenYear4$year)))
dizhenYearTable5<-data.frame(table(factor(dizhenYear5$year)))
dizhenYearTable6<-data.frame(table(factor(dizhenYear6$year)))
dizhenYearTable7<-data.frame(table(factor(dizhenYear7$year)))

dizhenYearTableAll<-cbind(dizhenYearTable,dizhenYearTable3,dizhenYearTable4,dizhenYearTable5,dizhenYearTable6)
dizhenYearTableAll<-dizhenYearTableAll[,c(-3,-5,-7,-9)]
colnames(dizhenYearTableAll)<-c("year","x2","x3","x4","x5","x6")

write.csv(dizhenYearTableAll,"dizhenyartableall.csv")




# dizhenYearTableAll<-data.frame(dizhenYearTableAll,r23=dizhenYearTableAll$x3/dizhenYearTableAll$x2)
# dizhenYearTableAll<-data.frame(dizhenYearTableAll,r24=dizhenYearTableAll$x4/dizhenYearTableAll$x2)
# dizhenYearTableAll<-data.frame(dizhenYearTableAll,r34=dizhenYearTableAll$x4/dizhenYearTableAll$x3)
# summary(dizhenYearTableAll[,c(5,6,7)])
# write.csv(dizhenYearTableAll,"dizhenYear.csv")
# hist(dizhenYearTableAll$r23)
# hist(dizhenYearTableAll$r34)
# r23<-dizhenYearTableAll$r23
# r34<-dizhenYearTableAll$r34
# acf(dizhenYearTableAll$x3)
# acf(dizhenYearTableAll$x4)


# ks.test(r23,"punif")
# ks.test(r23,"pnorm")
# ks.test(r34,"pnorm")
# ks.test(dizhenYearTableAll$x2,"ppois",lambda=mean(dizhenYearTableAll$x2))
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