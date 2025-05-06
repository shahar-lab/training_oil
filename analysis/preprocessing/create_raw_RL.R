
rm(list = ls())
source('./functions/my_starter.R')

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
  rt=as.numeric(df%>%filter(phase=="exp",trial_name=="cards1")%>%pull(rt))
  
  # Get the self-report values
  report <- df %>%
    filter(trial_type == "html-slider-response")
  
  motivation <- report %>%
    filter(grepl("out of 9", stimulus))%>%
    pull(response) %>%
    as.numeric()
  
  responses <- report %>%
    filter(grepl("out of 5", stimulus))%>%
    pull(response) %>%
    as.numeric()
  
  # Step 2: Define descriptive column names
  motivation_names <- c(
    "enjoyed",
    "interesting",
    "no_boring",
    "no_attention",
    "effort",
    "no_try_hard",
    "tense",
    "no_nervous",
    "self_value"
  )
  
  # Flip the values for the 'no_' columns by subtracting from the maximum score
  flipped_motivation <- motivation
  flipped_motivation[grepl("^no_", motivation_names)] <- 100 - flipped_motivation[grepl("^no_", motivation_names)]
  
  # Calculate the average
  mean_motivation <- mean(flipped_motivation)
  
  # Convert to a 1-row dataframe with 9 columns
  motivation_df <- as.data.frame(t(motivation))
  colnames(motivation_df) <- motivation_names
  
  #get text responses
  text=df %>%
    filter(trial_type == "survey-text") %>%
    pull(response) %>%
    gsub('^\\{\\"Q0\\":\\"|\\\"\\}$', '', .)

  text_df <- data.frame(
    text_instructions = text[1],
    text_motivation = text[2]
  )
  
  df=df%>%filter(trial_name=="reward2")%>%
    select(subject_id,block,trial_num,left_card,right_card,card_selected,key_selected,reward,counterbalance_reward,counterbalance_stimuli,prob1,prob2,prob3,prob4)%>%
    rename(trial=trial_num,ch_card=card_selected,ch_key=key_selected,card_left=left_card,card_right=right_card)%>%
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
           selected_offer=ch_key,
           )
  df=df%>%mutate(prob1=round(prob1,3),prob2=round(prob2,3),prob3=round(prob3,3),prob4=round(prob4,3))
  if(length(responses)!=0){
  df=df%>%mutate(response_clear_easy=as.numeric(responses[1]),
                 response_fair_honest=as.numeric(responses[2]),
                 response_think_locs=as.numeric(responses[3]),
                 response_tricked_manipulated=as.numeric(responses[4]),
                 response_use_locs=as.numeric(responses[5]))
  }
  # Bind to df (if you want these values repeated for each row of df)
  df <- bind_cols(df, motivation_df[rep(1, nrow(df)), ])
  df=df%>%mutate(mean_motivation=mean_motivation, end_time = file_path %>%
                   str_extract("(?<=SESSION_).*(?=\\.csv)") %>%
                   str_replace("h", ":") %>%
                   str_replace("(\\d{2})\\.(\\d{3})$", "\\1.\\2") %>%
                   parse_date_time(orders = "Ymd HMS", tz = "UTC"))
  df <- bind_cols(df, text_df[rep(1, nrow(df)), ])
  return (df)
}
# Get all CSV files in the directory
files1 <- list.files(paste0(data_folder,"/empirical_data/data_collected/session1"), pattern = "\\.csv$", full.names = TRUE)
files2 <- list.files(paste0(data_folder,"/empirical_data/data_collected/session2"), pattern = "\\.csv$", full.names = TRUE)
files3 <- list.files(paste0(data_folder,"/empirical_data/data_collected/session3"), pattern = "\\.csv$", full.names = TRUE)
# Process each file and combine results into a dataframe
df_raw1 <- do.call(rbind, lapply(files1, process_RL))
df_raw2 <- do.call(rbind, lapply(files2, process_RL))
df_raw3 <- do.call(rbind, lapply(files3, process_RL))
df_raw1$session=1
df_raw2$session=2
df_raw3$session=3
#save df_raw by session folder
save(df_raw1,file=paste0(data_folder,"/empirical_data/data_raw/session1/df_raw1.rdata"))
save(df_raw2,file=paste0(data_folder,"/empirical_data/data_raw/session2/df_raw2.rdata"))
save(df_raw3,file=paste0(data_folder,"/empirical_data/data_raw/session3/df_raw3.rdata"))

#bind all files sorted by subject

df_raw <- bind_rows(df_raw1, df_raw2, df_raw3)

df_raw = df_raw %>% arrange(subject_id,session,block,trial)%>%mutate(subject=as.integer(as.factor(subject_id)))%>%
  select(subject_id,session,block,trial,everything())

save(df_raw,file=paste0(data_folder,"/empirical_data/data_raw/RL_raw.rdata"))

write.csv(df_raw%>%select(-subject_id),file=paste0(data_folder,"/empirical_data/data_raw/RL_raw.csv"))

#filter current subjects
session3=unique(df_raw3%>%filter(end_time > ymd("2025-05-02"))%>%pull(subject_id))
write.csv(session3,file=paste0(data_folder,"/empirical_data/data_raw/session3.csv"))

session2=unique(df_raw2%>%filter(end_time > ymd("2025-05-01"))%>%pull(subject_id))
write.csv(session2,file=paste0(data_folder,"/empirical_data/data_raw/session2.csv"))
session1=unique(df_raw1%>%filter(end_time > ymd("2025-04-30"))%>%pull(subject_id))
write.csv(session1,file=paste0(data_folder,"/empirical_data/data_raw/session1.csv"))
