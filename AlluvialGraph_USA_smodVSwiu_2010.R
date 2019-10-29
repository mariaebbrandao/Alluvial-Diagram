# set workspace
setwd("C:/Users/mb084896/Desktop/AlluvialGraph_USA_smodVSwiu_2010/SMOD_WIU_CENSUS/Output")
install.packages("ggalluvial")
library("tidyverse")
library("dplyr")
library("ggalluvial")
library(scales)


table<-read_csv("P:/NFS_SEES/UrbanAnalysis/DegreeOfUrbanization/Result_tables/WIU_vs_SMOD_version_2/Mexico_smodVSwiu_2010_v2_update.csv")%>%
  filter(threshold=="50pct") %>%
  arrange(factor(SMODclass,levels=c("Cities","DenseTown","SemiDenseTown","Suburbs","Village","Rural","SparseRural"))) %>%
  mutate(CensusClass=case_when(WIUclass=="UA"~"Urban",
                               WIUclass=="UPO"~"Urban",
                               WIUclass=="BULO"~"Rural",
                               WIUclass=="RE"~"Rural")) %>% 

rename(CENSUSclass = CensusClass, Threshold = threshold) %>% mutate(POP=POP/1000) %>% 
ggplot(
       aes(axis1 =factor(SMODclass,levels=c("SparseRural","Rural","Village","Suburbs","SemiDenseTown","DenseTown","Cities")),
           axis2 =factor( WIUclass,levels=c("RE","BULO","UPO","UA")), axis3 = factor(CENSUSclass,levels=c("Urban","Rural")),
           y = POP)) +
  scale_x_discrete(limits = c("SMODclass", "WIUclass", "CENSUSclass"), expand = c(.0, .05)) +
  scale_fill_manual(breaks=c("Cities","DenseTown","SemiDenseTown","Suburbs","Village","Rural","SparseRural"),values = c("darkred","firebrick1","firebrick","red","chartreuse","chartreuse2","chartreuse4"))+
  xlab("Data Sets") +
  geom_alluvium(aes(fill = SMODclass)) +scale_y_continuous(labels=scales::comma)+
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("POP by 1%",
          "USA SMOD VS. SWIU 2010")

table
