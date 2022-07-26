library("ggplot2")
library(pwr)

#power analysis
pwr.f2.test(u=5, power=.80, sig.level=.05, f2=0.15)

#sensitivity plot
N<-seq(25,2852,by=25)

f2=seq(0.001,0.5, by=0.001)
flip = data.frame(f2=f2)
for (i in seq(seq(0,500))){
  f2=flip$f2[i]
  n80 = pwr.f2.test(u=5, power=.80, sig.level=.05, f2=f2)
  flip$n80[i] = n80$v + 5 +1
  n95 = pwr.f2.test(u=5, power=.95, sig.level=.05, f2=f2)
  flip$n95[i] = n95$v + 5 +1

}

ggplot(flip, 
       aes(f2))+
  geom_line(aes(y=n80, colour = ".80"))+
  geom_line(aes(y=n95, colour = ".95"))+
  theme(panel.background = element_rect(fill = "white"))+
  ylim(0,2500)+xlim(0,0.3)+
  labs(x="Effect size f",y="Total sample size", colour="Power")