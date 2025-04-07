rm(list = ls())
load(file="data/empirical_data/data_filtered/RL.rdata")
load(file="data/empirical_data/data_filtered/self_report.rdata")

library(dplyr)

df <- df %>%
  left_join(
    select(self_report, subject_id, oci_score, stai_score, bdi_score, sgs_score,aq_score), 
    by = "subject_id"
  )

df=df %>%mutate(subject=as.numeric(factor(subject_id)))%>%select(subject,everything())


save(df,file="data/empirical_data/data_analysis/df_all.rdata")
write.csv(df%>%select(-subject_id),file="data/empirical_data/data_analysis/df_all.csv")


