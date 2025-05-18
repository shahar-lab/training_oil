rm(list=ls())
source('./functions/my_starter.R')
load("data/empirical_data/data_raw/WM_raw.rdata")
     
# exclude subjects --------------------------------------------------------

remove_subjects <- WM_raw %>%
  group_by(subject_id) %>%
  summarise(
    mean_exclude_4 = mean(exclude_4, na.rm = TRUE),
    mean_exclude_chance = mean(exclude_chance, na.rm = TRUE),
    mean_exclude_inattention = mean(exclude_inattention, na.rm = TRUE)
  )
WM=WM_raw%>%filter(exclude_4==FALSE,exclude_chance==FALSE,exclude_inattention==FALSE)
save(WM,file="data/empirical_data/data_filtered/WM.rdata")

write.csv(WM%>%select(-subject_id),file="data/empirical_data/data_filtered/WM.csv")

after_remove_4=remove_subjects%>%filter(mean_exclude_4==0)
after_remove_chance=after_remove_4%>%filter(mean_exclude_chance==0)
after_remove_attention=after_remove_chance%>%filter(mean_exclude_inattention==0)
#stats exclusion
WM_raw%>%group_by(subject_id)%>%summarise(mean(exclude_4),mean(exclude_chance),mean(exclude_inattention))%>%
  summarise(
    remove_exclude_4 = sum(`mean(exclude_4)` > 0, na.rm = TRUE),
    remove_exclude_chance = sum(`mean(exclude_chance)` > 0, na.rm = TRUE),
    remove_inattention = sum(`mean(exclude_inattention)` > 0, na.rm = TRUE)
  )
