library(brms)
library(cmdstanr)
library(dplyr)
rm(list=ls())
load("data/empirical_data/data_analysis/df_all.rdata")

myprior = prior(normal(0, 0.2),  class = b)
df$reward_oneback=factor(df$reward_oneback)
df=na.omit(df)
# main -------------------------------------------------------
m_unch_sgs <-
  brm(
    formula=stay_unch_card~0+Intercept+reward_oneback*sgs_score+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )
save(m_unch_sgs,file="data/regression/unch_sgs.Rdata")

m_unch_ocir <-
  brm(
    formula=stay_unch_card~0+Intercept+reward_oneback*oci_score+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_unch_ocir,file="data/regression/unch_ocir.Rdata")

m_unch_bdi <-
  brm(
    formula=stay_unch_card~0+Intercept+reward_oneback*bdi_score+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_unch_bdi,file="data/regression/unch_bdi.Rdata")

m_oil <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F,session==1,block==1),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )

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

df$session=factor(df$session)
m_oil_session <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*session+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_oil_session,file="data/regression/oil_session.Rdata")

m_oil_aq <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*aq_score+reward_oneback*oci_score+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F,aq_score>0),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_oil_aq,file="data/regression/oil_aq.Rdata")

#splines

df <- df %>%
  mutate(global_trial = as.numeric(trial)+50* (as.numeric(block)-1)+(as.numeric(session)-1)*200)
s_oil <-
  brm(
    formula=stay_key~reward_oneback + s(global_trial, by = reward_oneback,bs = "cr"),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    control = list(adapt_delta = 0.95, max_treedepth = 15),
    backend = "cmdstanr"
  )
save(s_oil,file="data/regression/s_oil.Rdata")

#reliability
# Count the number of sessions per participant
participants_with_3_sessions <- df %>%
  group_by(subject_id) %>%
  summarise(n_sessions = n_distinct(session)) %>%
  filter(n_sessions == 3) %>%
  pull(subject_id)

m_unch_session1 <-
  brm(
    formula=stay_unch_card~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T,session==1,subject_id%in%participants_with_3_sessions),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )



m_unch_session2 <-
  brm(
    formula=stay_unch_card~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T,session==2,subject_id%in%participants_with_3_sessions),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )

m_unch_session3 <-
  brm(
    formula=stay_unch_card~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==T,session==3,subject_id%in%participants_with_3_sessions),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )

# Extract per-participant estimates for reward_oneback
slopes_s1 <- coef(m_unch_session1)$subject[,1,2]
slopes_s2 <- coef(m_unch_session2)$subject[,1,2]
slopes_s3 <- coef(m_unch_session3)$subject[,1,2]

m_oil_session1 <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F,session==1,subject_id%in%participants_with_3_sessions),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )



m_oil_session2 <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F,session==2,subject_id%in%participants_with_3_sessions),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )

m_oil_session3 <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F,session==3,subject_id%in%participants_with_3_sessions),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    prior=myprior,
    backend = "cmdstanr"
  )

# Extract per-participant estimates for reward_oneback
slopes_oil1 <- coef(m_oil_session1)$subject[,1,2]
slopes_oil2 <- coef(m_oil_session2)$subject[,1,2]
slopes_oil3 <- coef(m_oil_session3)$subject[,1,2]
