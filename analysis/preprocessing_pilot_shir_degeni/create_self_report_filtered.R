rm(list = ls())
library(dplyr)
load(file="data/empirical_data/data_raw/self_report/self_report_raw.rdata")

# filter subjects ---------------------------------------------------------

self_report=self_report_raw%>%filter(exclude_attention==0,exclude_rt==0)%>%
  select(subject_id,oci_score,stai_score,bdi_score,sgs_score,aq_score,demographic_eng,demographic_religion)

save(self_report,file="data/empirical_data/data_filtered/self_report.rdata")
write.csv(self_report%>%select(-subject_id),file="data/empirical_data/data_filtered/self_report.csv")
