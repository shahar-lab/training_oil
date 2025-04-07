library(bayestestR)
library(ggplot2)
library(posterior)
library(dplyr)

# Define sample sizes
sample_sizes <- c(50,100,150, 200)

# Placeholder for results
hdi_widths_dummy1 <- c()
hdi_widths_dummy2 <- c()

# Function to extract HDI95% width for a given parameter
extract_hdi_width <- function(file_path, parameter_name) {
  load(file_path)  # Load model object
  model_name <- ls()[sapply(ls(), function(x) inherits(get(x), "brmsfit"))]
  brms_fit <- get(model_name)
  
  # Extract posterior draws
  posterior <- as_draws_df(brms_fit)
  
  # Compute HDI95% width
  hdi_vals <- hdi(posterior[[parameter_name]])  
  return(abs(hdi_vals$CI_high - hdi_vals$CI_low))
}

# Loop over sample sizes and extract HDI widths for both interaction terms
for (Nsubj in sample_sizes) {
  file_path <- paste0("data/precision/fit_precision_", Nsubj, "_.rdata")
  
  hdi_widths_dummy1 <- c(hdi_widths_dummy1, extract_hdi_width(file_path, "b_reward_onebackreward_oneback_dummy:sessionsession_dummy1"))  
  hdi_widths_dummy2 <- c(hdi_widths_dummy2, extract_hdi_width(file_path, "b_reward_onebackreward_oneback_dummy:sessionsession_dummy2"))  
}

# Create a dataframe for plotting
plot_data <- data.frame(
  Sample_Size = rep(sample_sizes, 2),
  HDI_Width = c(hdi_widths_dummy1, hdi_widths_dummy2),
  Interaction = rep(c("Dummy1", "Dummy2"), each = length(sample_sizes))
)

# Plot using ggplot2
ggplot(plot_data, aes(x = Sample_Size, y = HDI_Width, color = Interaction)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Sample Size", y = "HDI95% Width", title = "Precision of Posterior Estimates by Sample Size") +
  scale_color_manual(values = c("red", "blue")) +
  theme_bw()
