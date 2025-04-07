#This code plot recovered parameters against the true parameters

rm(list=ls())
source('./functions/my_starter.R')
path = set_workingmodel()
#--------------------------------------------------------------------------------------------------------


#load parameters
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))

#load data

load(file="data/empirical_data/data_analysis/df_all.rdata")

df <- df %>%
  mutate(global_trial = as.numeric(trial) + 
           50 * (as.numeric(block) - 1) + 
           200 * (as.numeric(session) - 1))

#load parameters
lambda=(fit$draws(variables ='lambda' ,format='draws_matrix'))

sample_data <- lambda[3901:4000,]
# Function to process each sample data with its corresponding global_trial dataframe
process_sample_data <- function(sample_data, global_trial_df) {
  # Pivot sample_data to long format
  sample_data_long <- as.data.frame(sample_data) %>%
    pivot_longer(
      cols = everything(),
      names_to = "trial",
      names_prefix = "lambda\\[",  # Escape only the opening bracket
      values_to = "lambda"
    ) %>%
    mutate(
      trial = as.numeric(gsub("\\]", "", trial)),  # Remove closing bracket and convert to numeric
      sample_id = rep(1:nrow(sample_data), each = ncol(sample_data))  # Adjust sample_id
    )
  
  # Assign global_trial column based on the provided global_trial dataframe
  sample_data_long$global_trial <- rep(global_trial_df$global_trial, times = nrow(sample_data))
  
  # Calculate mean lambda per global_trial and sample_id for individual sample lines
  sample_data_long_grouped <- sample_data_long %>%
    group_by(global_trial, sample_id) %>%
    summarize(mean_l = mean(lambda, na.rm = TRUE), .groups = "drop")
  
  # Calculate overall mean lambda per global_trial for the reference line
  mean_data <- sample_data_long_grouped %>%
    group_by(global_trial) %>%
    summarize(mean_l = mean(mean_l, na.rm = TRUE))
  
  return(list(sample_data_long_grouped = sample_data_long_grouped, mean_data = mean_data))
}

# Apply the function to each group with their respective global_trial dataframe
result <- process_sample_data(sample_data, df)

# Add group labels and combine the grouped data
sample_data_long_grouped <- result$sample_data_long_grouped 

# Add group labels and combine the mean data
mean_data <- result$mean_data

# Plot
# Assuming you checked `unique(combined_sample_data$group_label)` and found "Group A" and "Group B"
a <- ggplot(sample_data_long_grouped, aes(x = global_trial, y = plogis(mean_l))) +
  geom_point(alpha = 0.1, size = 0.1) +  # Lighter points for individual samples
  # Mean lines for each group with matching colors
  geom_line(data = mean_data, aes(x = global_trial, y = plogis(mean_l)), inherit.aes = FALSE, size = 1.5) +
  labs(x = "Global Trial", y = "Lambda") +
  theme_bw() 

a
ggsave("plot_trans.png", plot = a, width = 10, height = 6, dpi = 300)

lambda0_sbj=colMeans((fit$draws(variables ='lambda0_sbj' ,format='draws_matrix')))
lambda1_sbj=colMeans((fit$draws(variables ='lambda1_sbj' ,format='draws_matrix')))
a=data.frame(lambda0_sbj,oci=oci$`mean(oci_score)`,aq=aq$`mean(aq_score)`)
model_lambda=lambda0_sbj~oci+aq
model = brm(model_lambda, 
                    data = a,
                    warmup = 1000,
                    iter   = 2000,    
                    cores  = 4,
                    chains = 4,
                    backend='cmdstan')

save(model,file=paste0(path$data,"/lambda0_oci_aq.rdata"))