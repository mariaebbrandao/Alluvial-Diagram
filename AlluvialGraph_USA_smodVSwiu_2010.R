# set workspace
setwd("C:/Users/mb084896/Desktop/AlluvialGraph_Smod_vs_WIU_2010/SMOD_WIU_CENSUS/Output")
install.packages("ggalluvial")
library("tidyverse")
library("dplyr")
library("ggalluvial")
library(scales)
library("grid")

# "SparseRural","Rural","Village","Suburbs","SemiDenseTown","DenseTown","Cities"
# "Cities","DenseTown","SemiDenseTown","Suburbs","Village","Rural","SparseRural"
# "chartreuse4","chartreuse2","chartreuse","red","firebrick","firebrick2", "darkred"
#"darkred","firebrick2","firebrick","red","chartreuse","chartreuse2","chartreuse4"

table<-read_csv("P:/NFS_SEES/UrbanAnalysis/DegreeOfUrbanization/Result_tables/WIU_vs_SMOD_version_2/Mexico_smodVSwiu_2010_v2_update.csv")%>%
  filter(threshold=="50pct") %>%
  arrange(factor(SMODclass,levels=c("SparseRural","Rural","Village","Suburbs","SemiDenseTown","DenseTown","Cities"))) %>%
  mutate(CensusClass=case_when(WIUclass=="UA"~"Urban",
                               WIUclass=="UPO"~"Urban",
                               WIUclass=="BULO"~"Rural",
                               WIUclass=="RE"~"Rural")) %>% 
  
rename(CENSUSclass = CensusClass, Threshold = threshold) %>% mutate(POP=POP/1000) %>% 
ggplot(
       aes(axis1 =factor(SMODclass,levels=c("SparseRural","Rural","Village","Suburbs","SemiDenseTown","DenseTown","Cities")),
           axis2 =factor(WIUclass,levels=c("RE","BULO","UPO","UA")), axis3 = factor(CENSUSclass,levels=c("Urban","Rural")),
           y = POP)) + 
  scale_x_discrete(labels = c("SMOD", "WIU", "Census"), limits = c("SMODclass", "WIUclass", "CENSUSclass"), expand = c(.0, .05)) + 
  scale_fill_manual(labels = c("Cities","DenseTown","SemiDenseTown","Suburbs","Village","Rural","SparseRural"), values = c("darkred","firebrick2","firebrick","orange","chartreuse","chartreuse2","chartreuse4"))+
  labs(fill = "SMOD")+ 
  xlab("") + ylab("Population by 1000") + 
  geom_alluvium(aes(fill = SMODclass)) +scale_y_continuous(labels=scales::comma)+
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +  
  theme(axis.title= element_text(face="bold", colour="black", size=12),axis.text.x  = element_text(angle=1800, vjust=0.5, size=12))+
  ggtitle("POP by 50%",
          "Mexico SMOD VS. SWIU 2010") +
  labs(caption = "UA: Urban Agreement  UPO: Urban People Only  BULO: Built Up Land Only  RE: Rural" )+theme(plot.caption = element_text(hjust=0.5))
  

table


