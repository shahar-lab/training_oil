rm(list=ls())
source('./functions/my_starter.R')
path = set_workingmodel()
#--------------------------------------------------------------------------------------------------------


#load parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata1.rds'))
c_unch1=colMeans(fit$draws(variables ='c_unch_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df1.RData")
subject_num1=unique(df$subject)
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata2.rds'))
c_unch2=colMeans(fit$draws(variables ='c_unch_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df2.RData")
subject_num2=unique(df$subject)
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata3.rds'))
c_unch3=colMeans(fit$draws(variables ='c_unch_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df3.RData")
subject_num3=unique(df$subject)

df1 <- data.frame(subject_id = subject_num1, estimate_s1 = c_unch1)
df2 <- data.frame(subject_id = subject_num2, estimate_s2 = c_unch2)
df3 <- data.frame(subject_id = subject_num3, estimate_s3 = c_unch3)

# Merge all sessions based on subject_id
aligned_df <- full_join(df1, df2, by = "subject_id") %>%
  full_join(df3, by = "subject_id") %>%
  arrange(subject_id)

#load parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata1.rds'))
c_1=colMeans(fit$draws(variables ='c_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df1.RData")
subject_num1=unique(df$subject)
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata2.rds'))
c_2=colMeans(fit$draws(variables ='c_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df2.RData")
subject_num2=unique(df$subject)
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata3.rds'))
c_3=colMeans(fit$draws(variables ='c_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df3.RData")
subject_num3=unique(df$subject)

df1 <- data.frame(subject_id = subject_num1, estimate_s1 = c_1)
df2 <- data.frame(subject_id = subject_num2, estimate_s2 = c_2)
df3 <- data.frame(subject_id = subject_num3, estimate_s3 = c_3)

# Merge all sessions based on subject_id
aligned_df <- full_join(df1, df2, by = "subject_id") %>%
  full_join(df3, by = "subject_id") %>%
  arrange(subject_id)



fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))
lambda=colMeans(fit$draws(variables ='lambda0_sbj' ,format='draws_matrix'))
#load parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata1.rds'))
lambda_1=colMeans(fit$draws(variables ='lambda_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df1.RData")
subject_num1=unique(df$subject)
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata2.rds'))
lambda_2=colMeans(fit$draws(variables ='lambda_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df2.RData")
subject_num2=unique(df$subject)
fit=readRDS(paste0(path$data,'/modelfit_empirical_standata3.rds'))
lambda_3=colMeans(fit$draws(variables ='lambda_sbj' ,format='draws_matrix'))
load("data/empirical_data/data_analysis/df3.RData")
subject_num3=unique(df$subject)

df1 <- data.frame(subject_id = subject_num1, estimate_s1 = lambda_1)
df2 <- data.frame(subject_id = subject_num2, estimate_s2 = lambda_2)
df3 <- data.frame(subject_id = subject_num3, estimate_s3 = lambda_3)

# Merge all sessions based on subject_id
aligned_df <- full_join(df1, df2, by = "subject_id") %>%
  full_join(df3, by = "subject_id") %>%
  arrange(subject_id)

library(ggplot2)
ggplot(aligned_df, aes(x = estimate_s1, y = estimate_s2)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Estimate S1 vs. S2")

ggplot(aligned_df, aes(x = estimate_s1, y = estimate_s3)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Estimate S1 vs. S3")

ggplot(aligned_df, aes(x = estimate_s2, y = estimate_s3)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Estimate S2 vs. S3")
