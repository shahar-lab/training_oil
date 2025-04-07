rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()
library(splines)

filepath="data/empirical_data/data_analysis/df_all.rdata"
load(filepath)

vars= c(
  'first_trial',
  'first_trial_in_block',
  'ch_card',
  'unch_card',
  'ch_key',
  'card_left',
  'card_right',
  'reward',
  'selected_offer')

# save_path="data/stan_ready_data_files/standata_3.Rdata"
# convert_to_stan_format(filepath,save_path,vars)

data_path = "data/stan_ready_data_files/standata.Rdata"
# load data
load(data_path)
# Spline basis
num_knots <- 5  # Number of internal knots
# Assuming my_data has columns: subject_trial, trial (global trial index)
df <- df %>%
  group_by(subject) %>%
  mutate(trial_number = row_number()) %>%  # Reset trial index per subject
  ungroup()

# Create an empty list to store splines per subject
X_trial_list <- list()

# Loop through each subject and compute B-spline basis functions
unique_subjects <- unique(df$subject)

for (subject_num in unique_subjects) {
  subject_trials <- df %>% filter(subject == subject_num)  # Get trials for this subject
  browser()
  X_spline <- bs(subject_trials$trial_number, df = num_knots)  # Compute B-splines for this subject
  X_trial_list[[subject_num]] <- as.matrix(X_spline)  # Store it
}

# Combine all splines into a single matrix
X_trial <- do.call(rbind, X_trial_list)  # Stack subject splines into one matrix

# Ensure X_trial is ordered correctly

data_for_stan$K = ncol(X_trial)   # Number of spline basis functions
data_for_stan$X_trial = X_trial    # B-spline basis for trial

save(data_for_stan,file=data_path)

# Compile model
modelfit_compile(path, format = F)
modelfit_mcmc(
  path,
  data_path = data_path,
  save_path="standata",
  mymcmc = list(
    datatype = 'empirical' ,
    samples  = 2000,
    warmup  = 2000,
    chains  = 4,
    cores   = 4
  )
)

fit   = readRDS(paste0(path$data, '/modelfit_empirical_splines_standata.rds'))


# Load model fit (assuming `fit` is your model object)
posterior_samples <- fit$draws(format = "df")  # Convert to a dataframe

# Select key parameters
posterior_df <- posterior_samples %>%
  dplyr::select(starts_with("mu_lambda_beta"))  # Extract spline coefficients

posterior_df_long <- tidyr::pivot_longer(posterior_df, cols = everything(), names_to = "parameter", values_to = "value")

ggplot(posterior_df_long, aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Posterior Distributions of Spline Coefficients",
       x = "Parameter Value",
       y = "Density") +
  facet_wrap(~parameter, scales = "free")



# Extract posterior draws for lambda_t
lambda_posterior <- fit$draws(variables = "lambda_t", format = "df")[1:19987]
lambda_t=colMeans(lambda_posterior)

df$lambda=lambda_t
df=df%>%mutate(global_trial=as.numeric(trial)+(as.numeric(block)-1)*50+(as.numeric(session)-1)*200)
mean_lambda=df %>% group_by(global_trial) %>% summarise(mean_lambda=mean(lambda))%>%pull(mean_lambda)
plot(1:600,mean_lambda)
d=data.frame(global_trial=1:600,mean_lambda)
d%>%ggplot(aes(x=global_trial,y=mean_lambda))+geom_line()
