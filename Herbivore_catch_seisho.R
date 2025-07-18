library(ggplot2)
library(ggbreak)

herbivore<-read.csv("Herbivore_setnet1.csv")
Budai<-subset(herbivore, Species=="Calotomus japonicus")


herbivore$Species <- factor(herbivore$Species, 
                           levels = c("Calotomus japonicus", "Siganus fuscescens",
                                      "Prionurus scalprum", "Girella punctata") )
herbivore_2sp<-subset(herbivore,Species=="Calotomus japonicus" | Species=="Siganus fuscescens")
herbivore_aigo<-subset(herbivore, Species=="Siganus fuscescens")


### Set-net catch of 4 herbivore species in 西湘 region ###
pl_herbivore<-
  ggplot(data=herbivore, aes(x=Year, y=Catch, color=Species))+
  geom_line()+
  geom_point()+
  xlab("Year")+ 
  ylab(expression(atop("Catch (kg)")))+ 
  theme_classic(base_size = 22, base_family = "sans")
pl_herbivore

ggsave("Catch_herbivore_2000_2020.png", width = 8, height = 4, dpi = 300)


pl_herbivore1<-pl_herbivore+scale_y_break(c(1000,3000,7000,8000),scales=c(0.5, 3,5,5))
pl_herbivore1

### Set-net catch of Aigo and budaiin 西湘 region ###
pl_herbivore_2sp<-
  ggplot(data=herbivore_2sp, aes(x=Year, y=Catch, color=Species))+
  geom_line()+
  geom_point()+
  scale_color_manual(labels = c(expression(italic("Calotomus japonicus")),
                                expression(italic("Siganus fuscescens"))),
                     values = c("#005aff","#ff0000"))+
  xlab("Year")+ 
  ylab(expression(atop("Catch (kg)")))+ 
  theme_classic(base_size = 22, base_family = "sans")

pl_herbivore_2sp

pl_herbivore_2sp_c<-pl_herbivore_2sp+scale_y_break(c(500, 1000 ),scales=c(0.5, 3))
pl_herbivore_2sp_c

##Change pointsを追加###
pl_herbivore_2sp_1<-pl_herbivore_2sp+scale_y_continuous(breaks=seq(0,3000,length=7),limits=c(0,3000))+
  geom_vline(xintercept = 2012, linetype = 2, color = "red", linewidth = 0.5)+
  geom_vline(xintercept = 2015, linetype = 2, color = "red", linewidth = 0.5)+
  geom_vline(xintercept = 2002, linetype = 2, color = "blue", linewidth = 0.5)+
  geom_vline(xintercept = 2005, linetype = 2, color = "blue", linewidth = 0.5)+
  geom_vline(xintercept = 2013, linetype = 2, color = "blue", linewidth = 0.5)
pl_herbivore_2sp_1

ggsave("Catch_aigo_budai_2000_2020.png", width = 8, height = 4, dpi = 300)

### Set-net catch of Aigo and budaiin 西湘 region ###
### Aigo 


