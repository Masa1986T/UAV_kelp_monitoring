######################data###############################
rm(list=ls())

Iwa_moba<-read.csv("Iwa_algae_data_2019-2020.csv")
Iwa_moba$Species <- factor(Iwa_moba$Species, 
                           levels = c("Ecklonia", "Sargassum","Small algae",
                                      "Geniculate CA","Nongeniculate CA") )
Iwa_moba$Species


library(ggplot2)
### Figure for seaweed cover
plot_seaweed<-ggplot(data=Iwa_moba, aes(x=Transect, y=Cover,fill=Species))+
  xlab("Transect")+ 
  ylab(expression(atop("Cover (%)")))+ 
  geom_boxplot()+ 
  labs(color='Zone')+
  facet_grid(~Year)+
  theme_classic(base_size = 22, base_family = "sans")+
  theme(axis.text = element_text(color="black",size=22))+
  theme(strip.text = element_text(size = 22))+
  theme(legend.text =  element_text(size = 20))
plot_seaweed

ggsave("Figure3_Iwa_algalcommunity1.pdf", width = 12, height = 4)



###PERMANOVA ####
library(vegan)
Iwa_moba_com<-read.csv("Iwa_algal_community_multivariate.csv",header=T,row.names=1)
envi_moba<-read.csv("envi_iwa_site_multivariate.csv")

adonis2(Iwa_moba_com~Year+Line, data=envi_moba, method="bray", permutation = 9999)


