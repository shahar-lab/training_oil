convert_to_stan_format <-function (filepath,save_path,var_toinclude){
  
  
  #load data
  load(filepath)
  
  # Number of subjects and trials per subject (based on your matrix dimensions)
  n_subjects <- length(unique(df$subject))  # Number of subjects (rows)
  n_trials_per_subject <- as.vector(table(df$subject))  # Number of trials per subject (columns)
  n_total_trials <- nrow(df)  # Total number of trials across all subjects
  
  # Create the subject index: repeat each subject ID for every trial they performed
  subject_vector <- rep(1:n_subjects, times = n_trials_per_subject)
  
  df=df%>%mutate(first_trial=if_else(block!=lag(block)&block==1,1,0),
    first_trial_in_block=if_else(block!=lag(block),1,0),
    ch_key=ch_key+1)
  df$first_trial_in_block[1]=1
  df$first_trial[1]=1
df$reward=as.numeric(df$reward)-1
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

  save(data_for_stan,file=save_path)
  
}