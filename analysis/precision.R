rm(list = ls())

library(faux)
library(tidyr)
library(dplyr)
library(brms)
library(cmdstanr)

# Load previous results
load("data/regression/studyA/key_effect_by_sample.rdata")
summary(m_key_sample)

# Define fixed effect coefficients
beta_0 <- -0.24
beta_1 <- 0.13
beta_2 <- 0 # session2 main
beta_3 <- 0 # session3 main
beta_4 <- 0 # session2 interaction
beta_5 <- 0 # session3 interaction

# Function to simulate data
simulate_data <- function(Nsubj, Nsessions, Nblocks, Ntrials) {
  
  df_sim <- 
    add_random(subject = Nsubj) |> 
    add_within(session = factor(1:Nsessions), .by = "subject") |> 
    add_within(block = 1:Nblocks, .by = "session") |> 
    add_within(trial = 1:Ntrials, .by = "block") |> 
    add_between(reward_oneback = c("unrewarded", "rewarded"), .by = "trial") |> 
    mutate(reoffer_both_cards = rbinom(n(), 1, prob = 1/6)) |>  # ✅ Simple & direct!
    add_contrast("reward_oneback", "treatment", add_cols = TRUE, colnames = 'reward_oneback_dummy') |> 
    add_contrast("session", "treatment", add_cols = TRUE, colnames = 'session_dummy') |> 
    add_contrast("reoffer_both_cards", "treatment", add_cols = TRUE, colnames = 'reoffer_both_cards_dummy') |> 
    add_ranef("subject", u0 = 0.25, u1 = 0.27) |> 
    mutate(
      logit_stay_key = (beta_0 + u0) + (beta_1 + u1) * reward_oneback_dummy +
        beta_2 * session_dummy1 + beta_3 * session_dummy2 +
        beta_4 * reward_oneback_dummy * session_dummy1 + 
        beta_5 * reward_oneback_dummy * session_dummy2,
      p_stay_key = exp(logit_stay_key) / (1 + exp(logit_stay_key)),
      stay_key = rbinom(n(), 1, p_stay_key)
    )
  
  return(df_sim)
}





# Compile the model on minimal data
df_sim <- simulate_data(Nsubj = 5, Nsessions = 3, Nblocks = 4, Ntrials = 50)
model <- brm(
  stay_key ~ 1 + reward_oneback * session + (1 + reward_oneback || subject), 
  data = df_sim %>% filter(reoffer_both_cards_dummy == FALSE),
  chains = 0,
  family = bernoulli(link = "logit"),
  backend = 'cmdstan'
)

# Run the model for different subject sizes
for (Nsubj in c(50,100,150, 200)) {
  
  # Simulate new dataset
  df_sim <- simulate_data(Nsubj=Nsubj, Nsessions = 3, Nblocks = 4, Ntrials = 50)
  
  # Update model with new data (NO recompilation)
  model <- update(
    model,
    newdata = df_sim %>% filter(reoffer_both_cards_dummy == 1),
    iter = 4000,
    warmup = 2000,
    chains = 2,
    cores = 2
  )
  
  # Save the model
  save(model, file = paste0("data/precision/fit_precision_", Nsubj, "_.rdata"))
  # Save the simulated dataset for this Nsubj
  save(df_sim, file = paste0("data/precision/df_sim_", Nsubj, "_.rdata"))
}
