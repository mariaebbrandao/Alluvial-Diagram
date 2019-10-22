# set workspace
setwd("C:/Users/mb084896/Desktop/Bnagladesh_lecz/Output")
install.packages("ggalluvial")
library("tidyverse")
library("dplyr")
library("ggalluvial")

alluvialdata<-read_csv("Bangladesh_merit_srtm_groupedelevation.csv" )

# testing data set

is_alluvia_form(as.data.frame(alluvialdata), axes = 1:3, silent = TRUE)

# creating alluvial diagram

ggplot(as.data.frame(alluvialdata),
       aes(y =Count, axis1 =Source, axis2 =ElevationZone)) +
  geom_alluvium(aes(fill = Source), width = 1/10) +
  geom_stratum(width = 1/10, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Source", "ElevationZone"), expand = c(.04, .04)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Comparison of Elevation Zones Between Merit and Srtm")

