data {
  
 int<lower=1> Ndata;                // Total number of trials (for all subjects)
  int<lower=1> Nsubjects; //number of subjects

  int<lower=2> Narms; //number of overall alternatives

  int<lower=2> Nraffle; //number of cards per trial

  int<lower=2> Ndims; //number of dimensions
array [Ndata] int first_trial; // Which subject performed each trial
  array [Ndata] int<lower=1, upper=Nsubjects> subject_trial; // Which subject performed each trial

  //Behavioral data:

  //each variable being a subject x trial matrix

  //the data is padded in make_standata function so that all subjects will have the same number of trials

  array[Ndata] int<lower=0> ch_card; //index of which card was chosen coded 1 to 4

  array[Ndata] int<lower=0> ch_key; //index of which card was chosen coded 1 to 4

  array[Ndata] int<lower=0> reward; //outcome of bandit arm pull

  array[Ndata] int<lower=0> card_left; //offered card in left bandit

  array[Ndata] int<lower=0> card_right; //offered card in right bandit

  array [Ndata] int <lower=0,upper=1> first_trial_in_block; // binary indicator

  array[Ndata] int<lower=0> selected_offer;

}

transformed data{
  real eps=1e-8;
}


parameters {
    // Group-level (population) parameters
  real mu_alpha;        // Mean learning rate across subjects
  real mu_beta;          // Mean non-decision time across subjects
  real mu_lambda0;        // Mean drift rate across subjects for cards
  real mu_lambda1;        // Mean drift rate across subjects for keys
  
  // Group-level standard deviations (for subject-level variability)
  real<lower=eps> sigma_alpha;          // Variability in non-decision time
  real<lower=eps> sigma_beta;          // Variability in non-decision time
  real<lower=eps> sigma_lambda0;        // Variability in drift rate for cards
  real<lower=eps> sigma_lambda1;        // Variability in drift rate for keys
  
// Non-centered parameters (random effects in standard normal space)
  vector[Nsubjects] alpha_raw;
  vector[Nsubjects] beta_raw;
  vector[Nsubjects] lambda0_raw;
  vector[Nsubjects] lambda1_raw;

}


transformed parameters {
  vector<lower=0,upper=1>[Nsubjects] alpha_sbj; // learning rate
  vector[Nsubjects] beta_sbj; // Threshold
  vector[Nsubjects] lambda0_sbj; // Scaling of sigma of CDF
  vector[Nsubjects] lambda1_sbj; // Scale of gamma distribution
  
  for (subject in 1:Nsubjects){
 alpha_sbj[subject] = inv_logit(mu_alpha +sigma_alpha* alpha_raw[subject]);
 beta_sbj[subject] = (mu_beta +sigma_beta* beta_raw[subject]);
 lambda0_sbj[subject] = (mu_lambda0 + sigma_lambda0 * lambda0_raw[subject]);
 lambda1_sbj[subject] = (mu_lambda1 +sigma_lambda1 * lambda1_raw[subject]);
  }
  vector [Ndata]alpha_t;
	vector [Ndata]beta_t;					  // trial-by-trial threshold
  vector [Ndata]lambda0_t;
	vector [Ndata]lambda1_t;
	
  vector[Ndims]  weights;
  real PE_card;
  real PE_key;
  vector [Ndata] Qnet_diff;
  vector [Narms] Q_cards;
  vector [Nraffle] Q_keys;
  vector [Nraffle] Qnet;
  vector [Ndims] prior_relevant;
  vector [Ndims] irrelevant_weights;
  vector [Ndata]lambda;
  vector [Ndata] transformed_lambda;
  vector [Ndata] diff_reliability;
  prior_relevant[1]=1;
  prior_relevant[2]=0;
  irrelevant_weights[1]=0;
  irrelevant_weights[2]=1;

    for (t in 1:Ndata) {
    alpha_t[t] = alpha_sbj[subject_trial[t]];
		beta_t[t] = beta_sbj[subject_trial[t]];
		lambda0_t[t]=lambda0_sbj[subject_trial[t]];
		lambda1_t[t]=lambda1_sbj[subject_trial[t]];
      if(first_trial[t]==1){
        lambda[t]=lambda0_t[t];
      }
  if (first_trial_in_block[t] == 1) {
      Q_cards=rep_vector(0.5, Narms);
      Q_keys=rep_vector(0.5, Nraffle);
    }
    transformed_lambda[t]=inv_logit(lambda[t]);
          weights[1]= transformed_lambda[t] * prior_relevant[1] + (1-transformed_lambda[t]) * irrelevant_weights[1];
          weights[2]= transformed_lambda[t] * prior_relevant[2] + (1-transformed_lambda[t]) * irrelevant_weights[2];
          
          Qnet[1]=weights[1]*Q_cards[card_left[t]]+weights[2]*Q_keys[1]; //We compound the value of the card appearing on the left and the value of the left key.

          Qnet[2]=weights[1]*Q_cards[card_right[t]]+weights[2]*Q_keys[2];
        
        //likelihood function
        Qnet_diff[t]  = Qnet[2]-Qnet[1]; //higher values of Qdiff mean higher chance to choose right option.
      
 //calculating PEs
 PE_card =reward[t] - Q_cards[ch_card[t]];
 PE_key  =reward[t] - Q_keys[ch_key[t]];
 
//Update values ch_key=1 means choosing right
 Q_cards[ch_card[t]] += alpha_t[t] * PE_card; //update card_value according to reward
 Q_keys[ch_key[t]]   += alpha_t[t] * PE_key; //update key value according to reward
 

//updating weights
diff_reliability[t]=abs(PE_key)-abs(PE_card);
if(t!=Ndata){
lambda[t+1]= lambda[t]+lambda1_t[t] * diff_reliability[t];
}
}
}

model {
  
  // Priors for group-level parameters
  mu_alpha ~ normal(0, 3);
  mu_beta ~ normal(0, 3);
  mu_lambda0 ~ normal(0, 3);
  mu_lambda1 ~ normal(0, 3);
  
  // Priors for group-level standard deviations
  sigma_alpha ~ normal(0, 1);
  sigma_beta ~ normal(0, 1);
  sigma_lambda0 ~ normal(0, 1);
  sigma_lambda1 ~ normal(0, 1);
  
  // Priors for subject-specific effect
  alpha_raw~normal(0,1);
  beta_raw~normal(0,1);
  lambda0_raw~normal(0,1);
  lambda1_raw~normal(0,1);
for (trial in 1:Ndata){
  target+= bernoulli_logit_lpmf(selected_offer[trial] | beta_t[trial] * Qnet_diff[trial]);
    }
}

