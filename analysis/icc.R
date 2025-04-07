
# Extract posterior samples
posterior_samples <- posterior_samples(m_oil, pars = c("sd_subject__Intercept", "sd_subject__reward_oneback"))

# Compute ICC for each posterior sample
posterior_samples$ICC <- posterior_samples$sd_subject__Intercept^2 /
  (posterior_samples$sd_subject__Intercept^2 + (pi^2 / 3))

posterior_samples$ICC_slope <- posterior_samples$sd_subject__reward_oneback^2 /
  (posterior_samples$sd_subject__reward_oneback^2 + (pi^2 / 3))

# Get 95% credible intervals
quantile(posterior_samples$ICC, probs = c(0.025, 0.5, 0.975))
quantile(posterior_samples$ICC_slope, probs = c(0.025, 0.5, 0.975))

save(m_oil,file="data/regression/oil.Rdata")