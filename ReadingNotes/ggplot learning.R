##ggplot学习

library(ggplot2)
data(mpg)
p <- ggplot(data=mpg,mapping=aes(x=cty,y=hwy))
p +geom_point()

p <- ggplot(data=mpg,mapping=aes(x=cty,y=hwy,colour=factor(year)))
p +geom_point()
p+geom_point()+stat_smooth(method=loess)

p <- ggplot(data=mpg,mapping=aes(x=cty,y=hwy))
p+geom_point(aes(colour=factor(year)))+stat_smooth()

p+geom_point(aes(colour=factor(year)))+stat_smooth()+
  scale_color_manual(values=c("lightblue","pink"))

p+geom_point(aes(colour=factor(year),size=displ))+stat_smooth()+
  scale_color_manual(values=c("lightblue","pink"))

p+geom_point(aes(colour=factor(year),size=displ),alpha=0.5,position = "jitter")+stat_smooth()+
  scale_color_manual(values=c("blue","pink"))+
  scale_size_continuous(range=c(1,4))

p+geom_point(aes(colour=factor(class),size=displ),alpha=0.6,position = "jitter")+stat_smooth()+
  scale_size_continuous(range=c(1,5))+
  facet_wrap(~year,ncol=1)+
  labs(title="main title",x="xlab",y="ylab")+
  guides(color=guide_legend(title="type",override.aes=list(size=10)),
         size=guide_legend(title="displ"))

##直方图

p <- ggplot(mpg,aes(x=hwy))
p+geom_histogram()

p+geom_histogram(aes(fill=factor(year),y=..density..),alpha=0.3,color="black")+
  facet_grid(~year)+
  stat_density(geom="line",position="identity",size=1.2,aes(color=factor(year)))


##条形图
p <- ggplot(mpg,aes(x=class))
p+geom_bar(aes(fill=factor(year)),alpha=0.5,position = "identity")


#饼图
p <- ggplot(mpg,aes(x=factor(1),fill=factor(class)))+
    geom_bar(width=1)
p+coord_polar(theta="y")

##箱线图
p <- ggplot(mpg,aes(class,hwy,fill=class))
p+geom_boxplot()

p+geom_violin(alpha=0.7,width=0.9)+
  geom_jitter(shape=3)


##