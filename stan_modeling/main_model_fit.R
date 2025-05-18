#####Setup--------------------
rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()

#####Create stan data--------------------
filepath="data\\empirical_data\\data_filtered\\RL.rdata"

load(filepath)
# 
# df=df%>%filter(session==1)
# save(df,file="data\\empirical_data\\data_analysis\\rl_session1.rdata")
# load(filepath)
# df=df%>%filter(session==2)
# save(df,file="data\\empirical_data\\data_analysis\\rl_session2.rdata")
# load(filepath)
# df=df%>%filter(session==3)
# save(df,file="data\\empirical_data\\data_analysis\\rl_session3.rdata")

filepath="data\\empirical_data\\data_analysis\\pilot_shir_degeni\\df_all.rdata"
vars= c(
  'first_trial',
  'first_trial_in_session',
  'first_trial_in_block',
  'ch_card',
  'unch_card',
  'ch_key',
  'card_left',
  'card_right',
  'reward',
  'selected_offer',
  'session')

save_path="data\\empirical_data\\data_analysis\\pilot_shir_degeni\\standata.rdata"
convert_to_stan_format(filepath,save_path,vars)
#####Load stan data--------------------
data_path = "data\\empirical_data\\data_analysis\\pilot_shir_degeni\\standata.rdata"

#####sample posterior--------------------

modelfit_compile(path, format = F)
modelfit_mcmc(
  path,
  data_path = data_path,
  save_path="degeni_confirmatory",
  mymcmc = list(
    datatype = 'empirical' ,
    samples  = 2000,
    warmup  = 2000,
    chains  = 4,
    cores   = 4
  )
)

#####examine results--------------------
mypars = c("population_scales[1]",
           "population_scales[2]")

examine_mcmc(path, mypars, datatype = 'empirical')

examine_population_parameters_recovery(path, datatype = 'empirical')

fit  = readRDS(paste0(path$data, '/modelfit_empirical_degeni_confirmatory.rds'))
####examine model
#load parameters
fit1   = readRDS(paste0(path$data, '/modelfit_empirical_session1.rds'))
fit2   = readRDS(paste0(path$data, '/modelfit_empirical_session2.rds'))
fit3   = readRDS(paste0(path$data, '/modelfit_empirical_session3.rds'))
lambda1_sbj = colMeans(fit1$draws(variables = 'lambda_sbj', format = 'draws_matrix'))
lambda2_sbj = colMeans(fit2$draws(variables = 'lambda_sbj', format = 'draws_matrix'))
lambda3_sbj = colMeans(fit3$draws(variables = 'lambda_sbj', format = 'draws_matrix'))

library(tibble)
library(ggplot2)

# Combine into a dataframe
df_lambdas <- tibble(
  subject = 1:length(lambda1_sbj),
  lambda1 = lambda1_sbj,
  lambda2 = lambda2_sbj,
  lambda3 = lambda3_sbj
)
# Calculate correlation coefficients
cor_lambda1_lambda2 <- cor(df_lambdas$lambda1, df_lambdas$lambda2)
cor_lambda1_lambda3 <- cor(df_lambdas$lambda1, df_lambdas$lambda3)
cor_lambda2_lambda3 <- cor(df_lambdas$lambda2, df_lambdas$lambda3)

print(paste("Correlation between lambda1 and lambda2:", round(cor_lambda1_lambda2,2)))
print(paste("Correlation between lambda1 and lambda3:", round(cor_lambda1_lambda3,2)))
print(paste("Correlation between lambda2 and lambda3:", round(cor_lambda2_lambda3,2)))

# Plot lambda1 vs lambda2
p1 <- ggplot(df_lambdas, aes(x = lambda1, y = lambda2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: lambda1 vs lambda2") +
  theme_minimal()+xlim(c(0.5,1))+ylim(c(0.5,1))

# Plot lambda1 vs lambda3
p2 <- ggplot(df_lambdas, aes(x = lambda1, y = lambda3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: lambda1 vs lambda3") +
  theme_minimal()+xlim(c(0.5,1))+ylim(c(0.5,1))

# Plot lambda2 vs lambda3
p3 <- ggplot(df_lambdas, aes(x = lambda2, y = lambda3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation: lambda2 vs lambda3") +
  theme_minimal()+xlim(c(0.5,1))+ylim(c(0.5,1))

# Optionally, arrange the plots
library(patchwork)
p1 + p2 + p3



lambda1 = (fit1$draws(variables = 'mu_lambda', format = 'draws_matrix'))
lambda2 = (fit2$draws(variables = 'mu_lambda', format = 'draws_matrix'))
lambda3 = (fit3$draws(variables = 'mu_lambda', format = 'draws_matrix'))

# Combine into a tidy dataframe
lambdas_df <- tibble(
  value = c(lambda1, lambda2, lambda3),
  comparison = factor(c(
    rep("session1", length(lambda1)),
    rep("session2", length(lambda2)),
    rep("session3", length(lambda3))
  ), levels = c("session1", "session2", "session3"))
)

ggplot(lambdas_df, aes(x = plogis(value), y = comparison, fill = comparison)) +
  stat_halfeye(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +  # shows where "no difference" is
  theme_bw() +
  labs(
    x = "Difference in Posterior Means",
    y = "Comparison",
    title = "Posterior Differences (mu_lambda)"
  ) +
  scale_fill_brewer(palette = "Set1")+xlim(c(0.5,1))
