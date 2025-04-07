#demographic data
rm(list = ls())
library(tidyverse)
load("data/data_raw/RL_raw.rdata")
#abstract
data_abstract=read.csv(file="data/data_collected/abstract/RL/df_raw_abstract.csv")
abstract_demographic=read.csv(file="data/data_collected/abstract/Demographic/demographic_data_from_prolific.csv")
abstract_demographic=abstract_demographic%>%filter(participant_id%in%data_abstract$prolific_id)%>%
  rename(prolific_id=participant_id)%>%
  select(prolific_id,status,Sex,age,Nationality,Country.of.Birth,Current.Country.of.Residence,Employment.Status,Student.Status,First.Language,
         Medication.use,Mental.health.illness.condition...ongoing,Mild.cognitive.impairment.Dementia,Vision)
abstract_demographic <- abstract_demographic %>%
  left_join(df_raw %>% select(prolific_id, subject) %>% distinct(prolific_id, .keep_all = TRUE), by = "prolific_id")


#story
data_story=read.csv(file="data/data_raw/story/RL_raw.csv")
story_demographic1=read.csv(file="data/data_collected/story/Demographic/prolific_export_66a0f20f0492b5d6da4d2ba0.csv")
story_demographic2=read.csv(file="data/data_collected/story/Demographic/prolific_export_66a791dc84b6d669652babcf.csv")
story_demographic3=read.csv(file="data/data_collected/story/Demographic/prolific_export_66977697565173b8edbd608c.csv")
story_demographic4=read.csv(file="data/data_collected/story/Demographic/prolific_export_669519374b5bd0caf94e8351.csv")
story_demographic5=read.csv(file="data/data_collected/story/Demographic/prolific_export_6686c7607967cb709a2d2a4b.csv")
story_demographic=rbind(story_demographic1,story_demographic2,story_demographic3,story_demographic4,story_demographic5)%>%
                  filter(Participant.id%in%data_story$subject_id)%>%
                  group_by(Participant.id)%>%
                  slice_min(Started.at)%>%ungroup()%>%
                  rename(prolific_id=Participant.id)%>%
                  select(prolific_id,Status,Completed.at,Sex,Age,Nationality,Ethnicity.simplified,Country.of.birth,Country.of.residence,Employment.status,Student.status,Language,
                         Medication.use,Autism.spectrum.disorder,Mental.health.diagnosis,Mental.health.illness.condition...ongoing,Mild.cognitive.impairment.dementia,Colourblindness)
story_demographic <- story_demographic %>%
  left_join(df_raw %>% select(prolific_id, subject) %>% distinct(prolific_id, .keep_all = TRUE), by = "prolific_id")%>%
  select(subject,everything())

age=mean(abstract_demographic$age)
sd_age=sd(abstract_demographic$age)
range_age=range(abstract_demographic$age)
sex=table(abstract_demographic$Sex)

#13 missing demographic data as they didn't complete the second session and returned the data.
#We approved their first submission.
#story_demographic=story_demographic%>%filter(Age!="CONSENT_REVOKED")
# age_story=mean(as.numeric(story_demographic$Age))
# sd_age_story=as.numeric(sd(story_demographic$Age))
# range_age_story=range(as.numeric(story_demographic$Age))
# sex_story=table(story_demographic$Sex)

#combine demographic
abstract_demographic=abstract_demographic%>%rename(Age=age)%>%mutate(sample="abstract")%>%select(sample,subject,Age,Sex)
story_demographic=story_demographic%>%mutate(sample="story")%>%select(sample,subject,Age,Sex)
demographic=rbind(abstract_demographic,story_demographic)%>%arrange(subject)

save(demographic,file="data/data_raw/demographic.rdata")
write.csv(demographic,file="data/data_raw/demographic.csv")
