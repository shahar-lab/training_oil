rm(list=ls())
library(tidyverse)
#add second session data
load(file="data/empirical_data/data_filtered/RL.rdata")
load(file="data/empirical_data/data_filtered/WM.rdata") #you need to have a filtered WM data for that part.

WM <- WM %>%
  group_by(subject_id) %>%
  summarise(capacity4=4*(mean(mean_accuracy4)*2-1),capacity8=8*(mean(mean_accuracy8)*2-1),
            avg_capacity = (capacity4+capacity8)/2)

# merge wm capacity with the decision-making data frame
df <- df %>%
  left_join(WM, by = "subject_id")

save(df,file="data/empirical_data/data_analysis/df_all.rdata")

write.csv(df,file="data/empirical_data/data_analysis/df_all.csv")
