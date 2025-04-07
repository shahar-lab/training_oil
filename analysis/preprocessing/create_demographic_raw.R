#demographic data
rm(list = ls())
library(tidyverse)
load("data/empirical_data/data_raw/RL_raw.rdata")

demographic=read.csv(file="data/empirical_data/data_collected/demographic/prolific_export_67a3a875762e7abf2c410a8c.csv")
demographic=demographic%>%
                  group_by(Participant.id)%>%filter(Status=="APPROVED")%>%
                  slice_min(Started.at)%>%ungroup()%>%
                  rename(prolific_id=Participant.id)%>%
                  select(prolific_id,Sex,Age)

age=mean(as.numeric(demographic$Age))
sd_age=sd(as.numeric(demographic$Age))
range_age=range(as.numeric(demographic$Age))
sex=table(demographic$Sex)


save(demographic,file="data/empirical_data/data_raw/demographic.rdata")
write.csv(demographic,file="data/empirical_data/data_raw/demographic.csv")
