data {

  //General fixed parameters for the experiment/models

  

  int<lower=1> Nsubjects;

  

  int<lower=1> Nblocks;

  

  int<lower=1> Ntrials;

  

  array[Nsubjects] int<lower=1> Ntrials_per_subject;

  

  int<lower=4> Npersons;

  

  int<lower=2> Nobjects;

  

  int<lower=2> Nraffle;
  array[Nsubjects,Ntrials] int<lower = 0> fold;
  real testfold;
  

  //Behavioral data:

  

  array[Nsubjects, Ntrials] int<lower=0> ch_person;

  array[Nsubjects, Ntrials] int<lower=0> unch_person;

  

  array[Nsubjects, Ntrials] int<lower=0> common_object;

  array[Nsubjects, Ntrials] int<lower=0> unique_ch_object;

  array[Nsubjects, Ntrials] int<lower=0> unique_unch_object;

  

  array[Nsubjects, Ntrials] int<lower=0> common_reward;

  array[Nsubjects, Ntrials] int<lower=0> unique_reward;

  

  array[Nsubjects, Ntrials] int<lower=0> person1;

  

  array[Nsubjects, Ntrials] int<lower=0> person2;

  

  array[Nsubjects, Ntrials] int<lower=0> selected_offer;

  

  array[Nsubjects, Ntrials] int<lower=0> first_trial_in_block;

}

transformed data {

  int<lower=1> Nparameters = 4;

}

parameters {

  //population level parameters 

  

  vector[Nparameters] population_locations;

  

  vector<lower=0>[Nparameters] population_scales;

  

  //individuals level

  

  vector[Nsubjects] alpha_mf_random_effect;

  

  vector[Nsubjects] alpha_mb_random_effect;

  

  vector[Nsubjects] omega_random_effect;

  

  vector[Nsubjects] beta_random_effect;

}

transformed parameters {

  vector<lower=0, upper=1>[Nsubjects] alpha_mf;

  vector<lower=0, upper=1>[Nsubjects] alpha_mb;

  vector<lower=0, upper=1>[Nsubjects] omega;

  vector[Nsubjects] beta;

  vector[Nraffle] Qmb_offered;

  vector[Nraffle] Qnet;

  

  real PE_mf;

  

  real PE_mb_common;

  real PE_mb_unique;

  

  vector[Npersons] Qmf;

  vector[Nobjects] Qmb;

  matrix[Ntrials, Nsubjects] Qnet_diff;

  //RL

  for (subject in 1 : Nsubjects) {

    //internal variabels

    

    //set indvidual parameters

    alpha_mf[subject] = inv_logit(population_locations[1]

                                  + population_scales[1]

                                    * alpha_mf_random_effect[subject]);

    alpha_mb[subject] = inv_logit(population_locations[2]

                                  + population_scales[2]

                                    * alpha_mb_random_effect[subject]);

    omega[subject] = inv_logit(population_locations[3]

                               + population_scales[3]

                                 * omega_random_effect[subject]);

    beta[subject] = (population_locations[4]

                     + population_scales[4] * beta_random_effect[subject]);

    

    //likelihood estimation

    for (trial in 1 : Ntrials_per_subject[subject]) {
      if(fold[subject,trial]!=testfold){
      //reset Qvalues (first trial only)

      if (first_trial_in_block[subject, trial] == 1) {

        Qmf = rep_vector(1, Npersons);

        Qmb = rep_vector(0.5, Nobjects);

      }

      

      //calculate probability for each action

      if (ch_person[subject, trial] == person1[subject, trial]) {

        Qmb_offered[1] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_ch_object[subject, trial]];

        Qmb_offered[2] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_unch_object[subject, trial]];

      } else {

        Qmb_offered[1] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_unch_object[subject, trial]];

        Qmb_offered[2] = Qmb[common_object[subject, trial]]

                         + Qmb[unique_ch_object[subject, trial]];

      }

      Qnet[1] = (1 - omega[subject]) * Qmf[person1[subject, trial]]

                + omega[subject] * Qmb_offered[1];

      Qnet[2] = (1 - omega[subject]) * Qmf[person2[subject, trial]]

                + omega[subject] * Qmb_offered[2];

      Qnet_diff[trial, subject] = Qnet[2] - Qnet[1];

      

      //update Qvalues

      PE_mf = common_reward[subject, trial] + unique_reward[subject, trial]

              - Qmf[ch_person[subject, trial]];

      Qmf[ch_person[subject, trial]] = Qmf[ch_person[subject, trial]]

                                       + alpha_mf[subject] * PE_mf;

      

      PE_mb_common = common_reward[subject, trial]

                     - Qmb[common_object[subject, trial]];

      Qmb[common_object[subject, trial]] = Qmb[common_object[subject, trial]]

                                           + alpha_mb[subject] * PE_mb_common;

      

      PE_mb_unique = unique_reward[subject, trial]

                     - Qmb[unique_ch_object[subject, trial]];

      Qmb[unique_ch_object[subject, trial]] = Qmb[unique_ch_object[subject, trial]]

                                              + alpha_mb[subject]

                                                * PE_mb_unique;

      
}

    }

  }

}

model {

  // population level  

  population_locations ~ normal(0, 2);

  population_scales ~ normal(0, 2);

  

  // indvidual level  

  alpha_mf_random_effect ~ std_normal();

  alpha_mb_random_effect ~ std_normal();

  omega_random_effect ~ std_normal();

  beta_random_effect ~ std_normal();

  

  for (subject in 1 : Nsubjects) {

    for (trial in 1 : Ntrials_per_subject[subject]) {
if(fold[subject,trial]!=testfold &&first_trial_in_block[subject, trial] != 1){
      target += bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject]

                                                                    * Qnet_diff[trial, subject]);
}
    }

  }

}


generated quantities {

  matrix[Ntrials,Nsubjects]  log_lik;
  vector[Nraffle] Qmb_offered_g;

  vector[Nraffle] Qnet_g;
  real PE_mf_g;

  real PE_mb_common_g;

  real PE_mb_unique_g;
  
  vector[Npersons] Qmf_g;

  vector[Nobjects] Qmb_g;
  matrix[Ntrials, Nsubjects] Qnet_diff_g;
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Likelihood function per subject per trial
log_lik=rep_matrix(0,Ntrials,Nsubjects);
    for (subject in 1:Nsubjects) {
 
        for (trial in 1:Ntrials_per_subject[subject]){
         
         if(fold[subject,trial] == testfold) {
 
      //reset Qvalues (first trial only)
      if (first_trial_in_block[subject,trial] == 1) {
        Qmf_g = rep_vector(1, Npersons);

        Qmb_g = rep_vector(0.5, Nobjects);
      }
        if (ch_person[subject, trial] == person1[subject, trial]) {

        Qmb_offered_g[1] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_ch_object[subject, trial]];

        Qmb_offered_g[2] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_unch_object[subject, trial]];

      } else {

        Qmb_offered_g[1] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_unch_object[subject, trial]];

        Qmb_offered_g[2] = Qmb_g[common_object[subject, trial]]

                         + Qmb_g[unique_ch_object[subject, trial]];

      }
      
      Qnet_g[1] = (1 - omega[subject]) * Qmf_g[person1[subject, trial]]

                + omega[subject] * Qmb_offered_g[1];

      Qnet_g[2] = (1 - omega[subject]) * Qmf_g[person2[subject, trial]]

                + omega[subject] * Qmb_offered_g[2];

      Qnet_diff_g[trial, subject] = Qnet_g[2] - Qnet_g[1];
      
      if (first_trial_in_block[subject, trial] != 1){
      log_lik[trial,subject] = bernoulli_logit_lpmf(selected_offer[subject, trial] | beta[subject] * Qnet_diff_g[trial, subject]);
}
      
           //update Qvalues

      PE_mf_g = common_reward[subject, trial] + unique_reward[subject, trial]

              - Qmf_g[ch_person[subject, trial]];

      Qmf_g[ch_person[subject, trial]] = Qmf_g[ch_person[subject, trial]]

                                       + alpha_mf[subject] * PE_mf_g;

      

      PE_mb_common_g = common_reward[subject, trial]

                     - Qmb_g[common_object[subject, trial]];

      Qmb_g[common_object[subject, trial]] = Qmb_g[common_object[subject, trial]]

                                           + alpha_mb[subject] * PE_mb_common_g;

      

      PE_mb_unique_g = unique_reward[subject, trial]

                     - Qmb_g[unique_ch_object[subject, trial]];

      Qmb_g[unique_ch_object[subject, trial]] = Qmb_g[unique_ch_object[subject, trial]]

                                              + alpha_mb[subject]

                                                * PE_mb_unique_g;
      
       
        }
        }
    }
}
