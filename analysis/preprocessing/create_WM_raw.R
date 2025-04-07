rm(list=ls())
library(tidyverse)
load("data/empirical_data/data_raw/RL_raw.rdata")
# session1 ----------------------------------------------------------------
process_wm <- function(file_path) {
  wm <- read.csv(file_path)
  subject_id=unique(wm$subject_id)
  #Check attention by mouse movement
  if(any(colnames(wm)=="event")){
    number_inattention = wm %>%
      filter(trial_index >= 15) %>%
      mutate(prev_event = lag(event)) %>%
      filter(event == "blur" & prev_event != "blur") %>%
      summarise(number_inattention = n()) %>%
      pull(number_inattention)
  }else{
    number_inattention=0 
  }

  #add acc
  wm=wm%>%filter(block_type=="exp",trial_name=="test_squares")%>%
    select(subject_id,trial_num,set_size,rt,accuracy,response,correct_response,mapping,condition,stimulus)
  
  wm=wm%>%mutate(mean_accuracy4=wm%>%filter(set_size==4)%>%summarise(acc4=mean(accuracy=="true",na.rm=T))%>%pull(acc4),
         mean_accuracy8=wm%>%filter(set_size==8)%>%summarise(acc8=mean(accuracy=="true",na.rm=T))%>%pull(acc8),
         mean_accuracy_combined=(mean_accuracy4+mean_accuracy8)/2,
         capacity4=4*(2*mean_accuracy4-1),
         capacity8=8*(2*mean_accuracy8-1),
         capacity_combined=mean(capacity4+capacity8),
         trial_num=trial_num+1)%>%
         rename(trial=trial_num)
  
  #check flags
  
  if(mean(wm$mean_accuracy4)<0.5){
    wm$exclude_chance = TRUE
  }
  else{
    wm$exclude_chance=FALSE
  }
if(mean(wm$mean_accuracy4)<mean(wm$mean_accuracy8)){
  wm$exclude_4 = TRUE
}
else{
  wm$exclude_4=FALSE
}
  if(number_inattention>1){
    wm$exclude_inattention = TRUE
  }
  else{
    wm$exclude_inattention=FALSE
  }
  return(wm)
  
}


# Get all CSV files in the directory
files <- list.files("data/empirical_data/data_collected/session4", pattern = "\\.csv$", full.names = TRUE)

# Process each file and combine results into a dataframe
WM_raw <- do.call(rbind, lapply(files, function(x) process_wm(x)))
save(WM_raw,file="data/empirical_data/data_raw/WM_raw.rdata")
write.csv(WM_raw%>%select(-subject_id),file="data/empirical_data/data_raw/WM_raw.csv")

