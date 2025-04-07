rm(list = ls())
load(file="data/empirical_data/data_raw/df_raw.rdata")
library(tidyverse)
#filter
filter_subject_id_data <- function(subject_id, data) {
  df <- data %>%
    filter(subject_id == !!subject_id) %>%
    filter(rt > 300, rt < 4000,!is.na(ch_key),!is.na(ch_card),!is.na(reward))
  
  df=df%>%group_by(session) %>%
    mutate(
      trials_in_session = n(),  # Count trials in each session
      exclude_trial_session1 = if_else(session == 1 & trials_in_session / 200 < 0.8 , TRUE, FALSE),
      exclude_trial_session2 = if_else(session == 2 & trials_in_session / 200 < 0.8 , TRUE, FALSE),
      exclude_trial_session3 = if_else(session == 3 & trials_in_session / 200 < 0.8 , TRUE, FALSE),
      exclude_inattention1 = if_else(session == 1 & number_inattention > 1, TRUE, FALSE),
      exclude_inattention2 = if_else(session == 2 & number_inattention > 1, TRUE, FALSE),
      exclude_inattention3 = if_else(session == 3 & number_inattention > 1, TRUE, FALSE)
    )%>%ungroup()%>%
    group_by(subject_id) %>%  # Group by subject to check missing sessions
    mutate(
      missing_sessions = paste0(setdiff(c(1, 2, 3), unique(session)), collapse = ", "),  # List missing sessions
      missing_session_flag = if_else(missing_sessions != "", TRUE, FALSE)  # Flag if any session is missing
    ) %>%
    ungroup()%>%
    mutate(exclude_key_rep = if_else(mean(stay_key,na.rm=T) > 0.7|mean(stay_key,na.rm=T)<0.3, TRUE, FALSE))
  
  return(df)
}

df <- unique(df_raw$subject_id) %>%
  lapply(function(subject_id) filter_subject_id_data(subject_id, df_raw)) %>%
  bind_rows()

df%>%group_by(subject)%>%summarise(mean(exclude_key_rep))%>%
  summarise(
    remove_key_rep = sum(`mean(exclude_key_rep)` > 0, na.rm = TRUE)
  )
#count removed trials per sample
ntrials_before=df_raw%>%summarise(n())
ntrials_after=df%>%summarise(n())

#fix this
filter=df%>%group_by(subject_id,subject)%>%summarise(missing_sessions=unique(missing_sessions),
                                                     exclude_trial_session1=max(exclude_trial_session1),
                                                     exclude_trial_session2=max(exclude_trial_session2),
                                                     exclude_trial_session3=max(exclude_trial_session3),
                                                     exclude_key_rep=max(exclude_key_rep),
                                                     exclude_inattention1=max(exclude_inattention1),
                                                     exclude_inattention2=max(exclude_inattention2),
                                                     exclude_inattention3=max(exclude_inattention3),
                                                     sum_motivation=mean(sum_motivation))
df=df%>%filter(exclude_trial_session1==FALSE,exclude_trial_session2==FALSE,exclude_trial_session3==FALSE,exclude_key_rep==FALSE,
               exclude_inattention1==FALSE,exclude_inattention2==FALSE,exclude_inattention3==FALSE) #exclude subjects
df=df%>%group_by(subject)%>%mutate(missing_sessions = paste0(setdiff(c(1, 2, 3), unique(session)), collapse = ", "),
                                   exclude_missing_sessions=if_else(nchar(missing_sessions) >1, TRUE, FALSE))%>%ungroup()

df=df%>%filter(exclude_missing_sessions==FALSE)

save(df,file="data/empirical_data/data_filtered/RL.rdata")
write.csv(df%>%select(-subject_id),file="data/empirical_data/data_filtered/RL.csv")

