# set workspace
setwd("C:/Users/mb084896/Desktop/LECZ Alluvial Graph/SMOD_WIU_CENSUS/Output")
install.packages("ggalluvial")
library("tidyverse")
library("dplyr")
library("ggalluvial")


PCT<-read_csv("USA_smodVSwiu_2010_v2_update_test_2.csv" )

# rename column 

PCT_rename <- PCT %>% rename(CENSUSclass = CensusClasses, Threshold = threshold)


# subset rows and columns by PCT and AREA

OnePctArea.subset <- subset(PCT_rename, Threshold < "25pct", select = SMODclass:AREA) 
TwentyfivePctArea.subset <- subset(PCT_rename, Threshold >="25pct" & Threshold <"50pct", select =c (SMODclass:AREA))
FiftyPctArea.subset <- subset(PCT_rename, Threshold >="50pct", select =c (SMODclass:AREA))

# subset rows and columns by PCT and POP
OnePctPOP.subset <- subset(PCT_rename, Threshold < "25pct", select =c (SMODclass:Threshold, POP))
TwentyfivePctPOP.subset <- subset(PCT_rename, Threshold >="25pct" & Threshold <"50pct", select =c (SMODclass:Threshold, POP))
FiftyPctPOP.subset <- subset(PCT_rename, Threshold >="50pct", select =c (SMODclass:Threshold, POP))



# creating alluvial diagram One Pct and POP

ggplot(data = OnePctPOP.subset,
       aes(axis1 = SMODclass, axis2 = WIUclass, axis3 = CENSUSclass,
           y = POP)) +
  scale_x_discrete(limits = c("SMODclass", "WIUclass", "CENSUSclass"), expand = c(.1, .05)) +
  xlab("Data Sets") +
  geom_alluvium(aes(fill = SMODclass)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("POP by 1%",
          "USA SMOD VS. SWIU 2010")

