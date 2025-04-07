convert_to_stan_format <-function (filepath,save_path,var_toinclude){
  
  
  #load data
  load(filepath)
  
  # Number of subjects and trials per subject (based on your matrix dimensions)
  n_subjects <- length(unique(df$subject))  # Number of subjects (rows)
  n_trials_per_subject <- as.vector(table(df$subject))  # Number of trials per subject (columns)
  n_total_trials <- nrow(df)  # Total number of trials across all subjects
  
  # Create the subject index: repeat each subject ID for every trial they performed
  subject_vector <- rep(1:n_subjects, times = n_trials_per_subject)
  
  df=df%>%mutate(
    first_trial_in_block=if_else(block!=lag(block),1,0),first_trial=if_else(trial==1&block==1,1,0),
    ch_key=ch_key+1)
  df$first_trial_in_block[1]=1
  
  #empirical only
  # df=df%>%mutate(selected_offer=choice,choice=choice+1)
  # df$selected_offer=df$ch_key-1
  #df$key1=1
  #df$key2=2
  #df$rt=df$rt/1000
  #df$ch_key=df$ch_key+1
  # Prepare stan_data list
  data_for_stan <- list(
    Ndata = n_total_trials,  # Total number of trials
    Nsubjects = n_subjects,  # Number of subjects
    subject_trial = subject_vector,  # Subject IDs for each trial
    Narms=4,
    Nraffle=2,
    Ndims=2
  )
  for (var in var_toinclude) {
    data_for_stan[[var]] <-  as.vector(df[[var]])
  }
  data_for_stan$reward=as.numeric(data_for_stan$reward)
  #save(data_for_stan,file="data/franz_data/subject1_standata.Rdata")
  save(data_for_stan,file=save_path)
  #save(data_for_stan,file=paste0(path$data,'/artificial_standata.Rdata'))
  # cat(paste0('[stan_modeling]:  "artificial_standata.Rdata" was saved at "',path$data,'"'))
  
}