rm(list = ls())
library(tidyverse)
load(file="data/data_analysis/df.rdata")
df=df%>%filter(sample=="abstract")
save(df,file="data/data_analysis/df_abstract.rdata")
load(file="data/data_filtered/df.rdata")
df=df%>%filter(sample=="story")
save(df,file="data/data_analysis/df_story.rdata")

vars= c(
  'first_trial_in_block',
  'first_trial',
  'rt',
  'choice',
  'reward',
  'card_left',
  'card_right',
  'ch_card',
  'ch_key',
  'selected_offer')

sample="all"
filepath=paste0("data/data_analysis/df_",sample,".rdata")

empirical_convert_to_stan_format(filepath,sample,vars)
