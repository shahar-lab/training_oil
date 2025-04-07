library(cmdstanr)
library(jsonlite)
library(gptoolsStan)

# Read JSON data
data_list <- fromJSON("tube-stan.json")

# Apply a training mask (similar to np.random.binomial in Python)
set.seed(42)  # Ensure reproducibility
train_mask <- rbinom(data_list$num_stations, size = 1, prob = 0.8)

# Replace passenger values with -1 where train_mask is 0
data_list$passengers <- ifelse(train_mask == 1, data_list$passengers, -1)
