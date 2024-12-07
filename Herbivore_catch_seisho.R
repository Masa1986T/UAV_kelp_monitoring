library(ggplot2)
library(ggbreak)

herbivore<-read.csv("Herbivore_setnet1.csv")

herbivore_2sp<-subset(herbivore,Species=="Calotomus japonicus" | Species=="Siganus fuscescens")


### Set-net catch of Siganaus fuscensus and Calatomus japonices Seisho region ###
pl_herbivore_2sp<-
  ggplot(data=herbivore_2sp, aes(x=Year, y=Catch, color=Species))+
  geom_line()+
  geom_point()+
  scale_color_manual(labels = c(expression(italic("Calotomus japonicus")),
                                expression(italic("Siganus fuscescens"))),
                     values = c("#005aff","#ff0000"))+
  xlab("Year")+ 
  ylab(expression(atop("Catch (kg)")))+ 
  theme_classic(base_size = 15, base_family = "sans")+
  theme(legend.position="top",legend.title.position="top",legend.text=element_text(size=14))

pl_herbivore_2sp


pl_herbivore_2sp_c<-
  pl_herbivore_2sp+scale_y_break(c(500, 1000),scales=c(0.5, 3))
pl_herbivore_2sp_c

ggsave("Figure3_Catch_aigo_budai_2000_2020.pdf", width = 5, height = 4, dpi = 300)

