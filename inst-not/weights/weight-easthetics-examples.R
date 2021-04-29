library(ggplot2)
library(ggpmisc)

stuff<-structure(list(var = 2:31, mean = c(17026.5, 11028.6842105263,
                                           13113.1111111111, 11087.3679395386, 9863.8664212548, 10060.676012167,
                                           9378.01091924399, 9790.67922990444, 8569.95788246269, 8839.68511390887,
                                           7656.50625471556, 7370.78564257028, 7939.13425925926, 7541.83192090395,
                                           8845.67474747475, 8023.03099415205, 6373.05976190476, 6337.93259803922,
                                           6824.79901960784, 7450.80769230769, 6651.81884057971, 5548.59722222222,
                                           7802.78205128205, 3627.07407407407, 2471, 2248.33333333333, 1368.7,
                                           2104.25, 742, 2097.5), n = c(2L, 19L, 150L, 419L, 1562L, 3178L,
                                                                        3880L, 2965L, 4288L, 2780L, 2209L, 664L, 54L, 59L, 66L, 57L,
                                                                        50L, 34L, 34L, 26L, 23L, 18L, 13L, 9L, 7L, 7L, 5L, 2L, 4L, 2L
                                           )), .Names = c("var", "mean", "n"), row.names = c(NA, -30L), class = "data.frame")


ggplot(data=stuff, aes(x=var, y=mean))+
  geom_point()+
  geom_smooth(method='lm', formula = y~x, alpha = 0.3)+
  geom_smooth(aes(weight=n), method='lm', formula = y~x, colour = "red", alpha = 0.3)

ggplot(data=stuff, aes(x=var, y=mean))+
  geom_point()+
  stat_smooth(method='lm', formula = y~x, alpha = 0.3)+
  stat_smooth(aes(weight=n), method='lm', formula = y~x, colour = "red", alpha = 0.3)

ggplot(data=stuff, aes(x=var, y=mean, weight=n))+
  geom_point()+
  stat_smooth(method='lm', formula = y~x, alpha = 0.3)+
  stat_smooth(aes(weight=NULL), method='lm', formula = y~x, colour = "red", alpha = 0.3)

ggplot(data=stuff, aes(x=var, y=mean))+
  geom_point()+
  geom_smooth(method='lm', formula = y~x)+
  stat_poly_eq(aes(label=paste(..eq.label.., stat(rr.label), sep="~~~")),
               formula=y~x, label.x.npc=0.8, label.y.npc=0.8,
               coef.digits=3, parse=TRUE)

ggplot(data=stuff, aes(x=var, y=mean, weight=n))+
  geom_point()+
  geom_smooth(method='lm', formula = y~x)+
  stat_poly_eq(aes(label=paste(..eq.label.., stat(rr.label), sep="~~~")),
               formula=y~x, label.x.npc=0.8, label.y.npc=0.8,
               coef.digits=3, parse=TRUE)

ggplot(data=stuff, aes(x=var, y=mean, weight=n))+
  geom_point()+
  geom_smooth(aes(weight=NULL), method='lm', formula = y~x)+
  stat_poly_eq(aes(weight=NULL, label=paste(..eq.label.., stat(rr.label), sep="~~~")),
               formula=y~x, label.x.npc=0.8, label.y.npc=0.8,
               coef.digits=3, parse=TRUE)

ggplot(data=stuff, aes(x=var, y=mean))+
  geom_point()+
  geom_smooth(method='lm', formula = y~x, colour = "blue")+
  geom_smooth(aes(weight = n), method='lm', formula = y~x, colour = "red")+
  stat_poly_eq(aes(label=paste(..eq.label.., stat(rr.label), sep="~~~")),
               formula=y~x, label.x.npc=0.8, label.y.npc=0.8,
               coef.digits=3, parse=TRUE, vjust = 1.5, colour = "blue") +
  stat_poly_eq(aes(weight = n, label=paste(..eq.label.., stat(rr.label), sep="~~~")),
               formula=y~x, label.x.npc=0.8, label.y.npc=0.8,
               coef.digits=3, parse=TRUE, vjust = -0.5, colour = "red")
