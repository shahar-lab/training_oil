rm(list=ls())
library(tidyverse)
rt_threshold=5
process_self_report <- function(file_path) {
  df <- read.csv(file_path)%>%filter(Finished==TRUE)%>%mutate(oci_score=as.numeric(SC0),
                                                              stai_score=as.numeric(SC1),
                                                              bdi_score=as.numeric(SC2),
                                                              sgs_score=as.numeric(SC3),
                                                              aq_score=as.numeric(SC4))%>%
    select(car_time_First.Click:last_col(),-c(SC0,SC1,SC2,SC3),-contains("First.Click"), -contains("Last.Click"),-contains("Click.Count"))%>%
    mutate(exclude_attention=if_else(car_attention!="Car Crush"|tsunami_attention!="Tsunami"|earthquake_attention!="Earthquake",1,0),
           exclude_rt=if_else(as.numeric(car_time_Page.Submit)<rt_threshold|
           as.numeric(tsunami_time_Page.Submit)<rt_threshold|
           as.numeric(earthquake_time_Page.Submit)<rt_threshold,1,0))
  
  return(df)
  
}


# Get all CSV files in the directory
files <- list.files("data/empirical_data/data_collected/self_report", pattern = "\\.csv$", full.names = TRUE)
# Process each file and combine results into a dataframe
self_report_raw <- bind_rows(lapply(files, function(x) process_self_report(x)))
self_report_raw <- self_report_raw %>%mutate(subject_id=PROLIFIC_PID)%>%select(-PROLIFIC_PID)%>%
select(subject_id,exclude_attention,exclude_rt,oci_score,stai_score,bdi_score,sgs_score,demographic_eng,demographic_religion,everything())

save(self_report_raw,file="data/empirical_data/data_raw/self_report/self_report_raw.rdata")
write.csv(self_report_raw%>%select(-subject_id),file="data/empirical_data/data_raw/self_report/self_report_raw.csv")
