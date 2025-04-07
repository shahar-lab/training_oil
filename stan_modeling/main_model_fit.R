#####Setup--------------------
rm(list = ls())
source('./functions/my_starter.R')

path = set_workingmodel()

#####Create stan data--------------------
filepath="data/empirical_data/data_analysis/df_all.rdata"
filepath="data/empirical_data/data_analysis/df1.rdata"
filepath="data/empirical_data/data_analysis/df2.rdata"
filepath="data/empirical_data/data_analysis/df3.rdata"
load(filepath)
df= df%>%filter(session==1)
save(df,file="data/empirical_data/data_analysis/df1.rdata")
df= df%>%filter(session==2)
save(df,file="data/empirical_data/data_analysis/df2.rdata")
df= df%>%filter(session==3)
save(df,file="data/empirical_data/data_analysis/df3.rdata")
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

save_path="data/stan_ready_data_files/standata3.Rdata"
convert_to_stan_format(filepath,save_path,vars)
#####Load stan data--------------------
data_path = "data/stan_ready_data_files/standata1.Rdata"

#####sample posterior--------------------

modelfit_compile(path, format = F)
modelfit_mcmc(
  path,
  data_path = data_path,
  save_path="standata1",
  mymcmc = list(
    datatype = 'empirical' ,
    samples  = 1000,
    warmup  = 1000,
    chains  = 4,
    cores   = 4
  )
)

data_path = "data/stan_ready_data_files/standata2.Rdata"

#####sample posterior--------------------


modelfit_mcmc(
  path,
  data_path = data_path,
  save_path="standata2",
  mymcmc = list(
    datatype = 'empirical' ,
    samples  = 1000,
    warmup  = 1000,
    chains  = 4,
    cores   = 4
  )
)

data_path = "data/stan_ready_data_files/standata3.Rdata"

#####sample posterior--------------------

modelfit_mcmc(
  path,
  data_path = data_path,
  save_path="standata3",
  mymcmc = list(
    datatype = 'empirical' ,
    samples  = 1000,
    warmup  = 1000,
    chains  = 4,
    cores   = 4
  )
)

#####examine results--------------------
mypars = c("population_scales[1]",
           "population_scales[2]")

examine_mcmc(path, mypars, datatype = 'empirical')

examine_population_parameters_recovery(path, datatype = 'empirical')


####examine model
#load parameters
fit   = readRDS(paste0(path$data, '/modelfit_empirical.rds'))
Qdiff = fit$draws(variables = 'Qdiff_external', format = 'draws_matrix')
Qval1 = fit$draws(variables = 'Qval1_external', format = 'draws_matrix')
Qval2 = fit$draws(variables = 'Qval2_external', format = 'draws_matrix')
Qval3 = fit$draws(variables = 'Qval3_external', format = 'draws_matrix')
Qval4 = fit$draws(variables = 'Qval4_external', format = 'draws_matrix')

PE    = fit$draws(variables = 'PE_external', format = 'draws_matrix')


