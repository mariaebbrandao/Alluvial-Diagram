
# set workspace
setwd("C:/Users/mb084896/Desktop/Bnagladesh_lecz/Output")
library("tidyverse")
library("dplyr")

srtm<-read_csv("bgd_srtm_point_unique_id.csv" )
merit<-read_csv("bgd_merit_point_unique_id.csv")

# rename columns

srtm_changeName <- srtm %>% rename(ELEVATION_srtm = GRID_CODE)
merit_changeName <- merit %>% rename(Unique_ID=Unique_id,ELEVATION_merit= GRID_CODE)

# remove unecessary columns

ELEVATION_merit<-subset(merit_changeName,select = c(ELEVATION_merit))
ELEVATION_srtm<-subset(srtm_changeName,select = c(ELEVATION_srtm))

# aggregating data into ranges

merit_groupedelevation<-ELEVATION_merit %>%
  group_by(ELEVATION_merit) %>%
  summarize(Count=n())%>%mutate(Source="MERIT","")%>%select(Source,ElevationZone=ELEVATION_merit,Count)

srtm_groupedelevation<-ELEVATION_srtm %>%
  group_by(ELEVATION_srtm) %>%
  summarize(Count=n())%>%mutate(Source="SRTM")%>%select(Source,ElevationZone=ELEVATION_srtm,Count)

# append two data frames on top of each other
# exporting a dataset from R to CSV

merit_srtm_groupedelevation<- bind_rows(merit_groupedelevation,srtm_groupedelevation)%>%write_csv("Bangladesh_merit_srtm_groupedelevation.csv")



