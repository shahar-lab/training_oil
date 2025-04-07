
library(tidyverse)
rm(list = ls())

# preprocess --------------------------------------------------------------

process_RL <- function(file_path) {
  df <- read.csv(file_path)
  
  #get attention by mouse movement
  if(any(colnames(df)=="event")){
    number_inattention = df %>%
      filter(trial_index >= 15) %>%
      mutate(prev_event = lag(event)) %>%
      filter(event == "blur" & prev_event != "blur") %>%
      summarise(number_inattention = n()) %>%
      pull(number_inattention)
  }else{
    number_inattention=0 
  }
  
  #get rt
  rt=as.numeric(df%>%filter(phase=="exp",trial_name=="shapes")%>%pull(rt))
  df=df%>%filter(trial_name=="reward")%>%
    select(subject_id,block,trial_num,left_shape,right_shape,shape_selected,key_selected,reward,prob1,prob2,prob3,prob4)%>%
    rename(trial=trial_num,ch_card=shape_selected,ch_key=key_selected,card_left=left_shape,card_right=right_shape)%>%
    mutate(block=block+1,rt=rt,ch_key=as.integer(ch_key),
           unch_card=if_else(ch_card==card_right,card_left,card_right),
           prob_ch=case_when(ch_card==0~prob1,
                             ch_card==1~prob2,
                             ch_card==2~prob3,
                             ch_card==3~prob4),
           prob_unch=case_when(unch_card==0~prob1,
                               unch_card==1~prob2,
                               unch_card==2~prob3,
                               unch_card==3~prob4),
           
           #lines below are only for story
           bonus=if_else(reward==1,0.00166,0),
           number_inattention=number_inattention,
           block=factor(block),trial=factor(trial+1),
           reward=factor(reward),reward_oneback=lag(reward),
           ch_card=ch_card+1,unch_card=unch_card+1,
           card_left=card_left+1,card_right=card_right+1,
           reoffer_ch=lag(ch_card)==card_right|lag(ch_card)==card_left,
           reoffer_unch=lag(unch_card)==card_right|lag(unch_card)==card_left,
           stay_key=ch_key==lag(ch_key),
           stay_card=ch_card==lag(ch_card),
           stay_unch_card=lag(unch_card)==ch_card,
           accuracy=(prob_ch>prob_unch)*1,
           delta_exp_value=abs(prob_ch-prob_unch),
           prob_left = case_when(
             card_left == 1 ~ prob1,
             card_left == 2 ~ prob2,
             card_left == 3 ~ prob3,
             card_left == 4 ~ prob4,
             TRUE ~ NA_real_  # Default value 
           ),prob_right = case_when(
             card_right == 1 ~ prob1,
             card_right == 2 ~ prob2,
             card_right == 3 ~ prob3,
             card_right == 4 ~ prob4,
             TRUE ~ NA_real_  # Default value 
           ),better_loc=if_else(prob_left>prob_right,0,1),
           prev_loc_better=if_else(lag(ch_key)==better_loc,1,0),
           selected_offer=ch_key)
  df=df%>%mutate(prob1=round(prob1,3),prob2=round(prob2,3),prob3=round(prob3,3),prob4=round(prob4,3))
  df = df %>%
    group_by(block) %>%
    mutate(
      counterbalance_reward = factor(case_when(
        first(prob1[trial == 1]) == 0.157 ~ "Condition 1",
        first(prob1[trial == 1]) == 0.250 ~ "Condition 2",
        first(prob1[trial == 1]) == 0.367 ~ "Condition 3",
        first(prob1[trial == 1]) == 0.846 ~ "Condition 4",
        TRUE ~ NA_character_  # Handle cases where no match occurs
      )))%>%ungroup()

  return (df)
}
# Get all CSV files in the directory
files1 <- list.files("data/empirical_data/data_collected/session1", pattern = "\\.csv$", full.names = TRUE)
files2 <- list.files("data/empirical_data/data_collected/session2", pattern = "\\.csv$", full.names = TRUE)
files3 = list.files("data/empirical_data/data_collected/session3", pattern = "\\.csv$", full.names = TRUE)
# Process each file and combine results into a dataframe
df_raw1 <- do.call(rbind, lapply(files1, process_RL))
df_raw2 <- do.call(rbind, lapply(files2, process_RL))
df_raw3 <- do.call(rbind, lapply(files3, process_RL))
df_raw1$session=1
df_raw2$session=2
df_raw3$session=3
#save df_raw by session folder
save(df_raw1,file="data/empirical_data/data_raw/session1/df_raw1.rdata")
save(df_raw2,file="data/empirical_data/data_raw/session2/df_raw2.rdata")
save(df_raw3,file="data/empirical_data/data_raw/session3/df_raw3.rdata")

#bind all files sorted by subject

df_raw = rbind(df_raw1,df_raw2,df_raw3)
df_raw = df_raw %>% arrange(subject_id,session,block,trial)%>%mutate(subject=as.integer(as.factor(subject_id)))%>%
  select(subject_id,session,block,trial,everything())

save(df_raw,file="data/empirical_data/data_raw/df_raw.rdata")

write.csv(df_raw%>%select(-subject_id),file="data/empirical_data/data_raw/df_raw.csv")

