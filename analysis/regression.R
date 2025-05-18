
rm(list=ls())
source('./functions/my_starter.R')
load(paste0("data/empirical_data/data_filtered/RL.rdata"))

myprior = prior(normal(0, 2),  class = b)
df$reward_oneback=factor(df$reward_oneback)
#df=na.omit(df)
# main -------------------------------------------------------

# m_oil <-
#   brm(
#     formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject),
#     data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(m_oil,file=paste0(data_folder,"/regression/oil.Rdata"))

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
save(m_oil_session,file=paste0("data/regression/oil_session.Rdata"))

m_oil_motivation <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback*mean_motivation+(1+reward_oneback|subject),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )
save(m_oil_motivation,file=paste0("data/regression/oil_motivation.Rdata"))

c_eff <- conditional_effects(m_oil_motivation)

#creating plot
plot <- plot(c_eff, plot = FALSE)[[3]] +theme_bw()
  
# m_relevant <-
#   brm(
#     formula=stay_card~0+Intercept+reward_oneback+(1+reward_oneback|subject),
#     data = df%>%filter(reoffer_ch==T),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(m_relevant,file=paste0(data_folder,"/regression/relevant.Rdata"))
# 
# m_oil_capacity <-
#   brm(
#     formula=stay_key~0+Intercept+reward_oneback*avg_capacity+(1+reward_oneback|subject),
#     data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
#     family = bernoulli(link = "logit"),
#     warmup = 1000,
#     iter = 2000,
#     chains = 4,
#     cores = 4,
#     seed = 123,
#     backend = "cmdstanr"
#   )
# save(m_oil_capacity,file=paste0(data_folder,"/regression/oil_capacity.Rdata"))


#splines
df$trial=as.numeric(df$trial)
s_oil <-
  brm(
    formula=stay_key~reward_oneback + s(trial, by = reward_oneback,bs = "cr"),
    data = df%>%filter(reoffer_ch==F,reoffer_unch==F),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    # seed = 123,
    # control = list(adapt_delta = 0.95, max_treedepth = 15),
    backend = "cmdstanr"
  )
save(s_oil,file="data/regression/s_oil.Rdata")

c_eff <- conditional_effects(s_oil)

#creating plot
plot <- plot(c_eff, plot = FALSE)[[3]] +theme_bw()

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
    data = df%>%filter(session==1),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )



m_oil_session2 <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(session==2),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )

m_oil_session3 <-
  brm(
    formula=stay_key~0+Intercept+reward_oneback+(1+reward_oneback|subject_id),
    data = df%>%filter(session==3),
    family = bernoulli(link = "logit"),
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 4,
    seed = 123,
    backend = "cmdstanr"
  )

# Extract per-participant estimates for reward_oneback
slopes_oil1 <- coef(m_oil_session1)$subject[,1,2]
slopes_oil2 <- coef(m_oil_session2)$subject[,1,2]
slopes_oil3 <- coef(m_oil_session3)$subject[,1,2]
