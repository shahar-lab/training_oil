data {
  
 int<lower=1> Ndata;                // Total number of trials (for all subjects)
  int<lower=1> Nsubjects; //number of subjects

  int<lower=2> Narms; //number of overall alternatives

  int<lower=2> Nraffle; //number of cards per trial

  int<lower=2> Ndims; //number of dimensions
  
  array [Ndata] int first_trial; // Which subject performed each trial
  array [Ndata] int first_trial_in_session;
  array [Ndata] int<lower=1, upper=Nsubjects> subject_trial; // Which subject performed each trial

  //Behavioral data:

  //each variable being a subject x trial matrix

  //the data is padded in make_standata function so that all subjects will have the same number of trials

  array[Ndata] int<lower=0> ch_card; //index of which card was chosen coded 1 to 4

  array[Ndata] int<lower=0> ch_key; //index of which card was chosen coded 1 to 4

  array[Ndata] int<lower=0> reward; //outcome of bandit arm pull

  array[Ndata] int<lower=0> card_left; //offered card in left bandit

  array[Ndata] int<lower=0> card_right; //offered card in right bandit


  array[Ndata] int<lower=0> selected_offer;
  array[Ndata] int<lower=0> session;

}



parameters {
    // Group-level (population) parameters
  real mu_alpha;        // Mean learning rate across subjects
  real mu_beta;          // Mean non-decision time across subjects
  real mu_lambda1;        // Mean drift rate across subjects for cards
  real mu_lambda2;
  real mu_lambda3;
  // Group-level standard deviations (for subject-level variability)
  real<lower=0> sigma_alpha;          // Variability in non-decision time
  real<lower=0> sigma_beta;          // Variability in non-decision time
  real<lower=0> sigma_lambda1;        // Variability in drift rate for cards
  real<lower=0> sigma_lambda2;
  real<lower=0> sigma_lambda3;
// Non-centered parameters (random effects in standard normal space)
  vector[Nsubjects] alpha_raw;
  vector[Nsubjects] beta_raw;
  vector[Nsubjects] lambda1_raw;
  vector[Nsubjects] lambda2_raw;
  vector[Nsubjects] lambda3_raw;

}


transformed parameters {
  vector<lower=0,upper=1>[Nsubjects] alpha_sbj; // learning rate
  vector[Nsubjects] beta_sbj; // Threshold
  vector<lower=0,upper=1>[Nsubjects] lambda1_sbj; // Scaling of sigma of CDF
  vector<lower=0,upper=1>[Nsubjects] lambda2_sbj;
  vector<lower=0,upper=1>[Nsubjects] lambda3_sbj;
  
 alpha_sbj = inv_logit(mu_alpha +sigma_alpha* alpha_raw);
 beta_sbj = (mu_beta +sigma_beta* beta_raw);
 lambda1_sbj = inv_logit(mu_lambda1 + sigma_lambda1 * lambda1_raw);
 lambda2_sbj = inv_logit(mu_lambda2 + sigma_lambda2 * lambda2_raw);
 lambda3_sbj = inv_logit(mu_lambda3 + sigma_lambda3 * lambda3_raw);

  real alpha_t;
	real beta_t;					  // trial-by-trial threshold
  real lambda_t;
	
  real PE_card;
  real PE_key;
  vector [Ndata] Qnet_diff;
  vector [Narms] Q_cards;
  vector [Nraffle] Q_keys;
  vector [Nraffle] Qnet;

    for (t in 1:Ndata) {
    alpha_t = alpha_sbj[subject_trial[t]];
		beta_t = beta_sbj[subject_trial[t]];
		
		if(session[t]==1){
		lambda_t=lambda1_sbj[subject_trial[t]];
		}
		else if (session[t]==2){
		lambda_t=lambda2_sbj[subject_trial[t]]; 
		}
		
		else {
		lambda_t=lambda3_sbj[subject_trial[t]];
		}
		
  if (first_trial_in_session[t] == 1) {
      Q_cards=rep_vector(0.5, Narms);
      Q_keys=rep_vector(0.5, Nraffle);
    }

          Qnet[1]=lambda_t*Q_cards[card_left[t]]+(1-lambda_t)*Q_keys[1]; //We compound the value of the card appearing on the left and the value of the left key.

          Qnet[2]=lambda_t*Q_cards[card_right[t]]+(1-lambda_t)*Q_keys[2];
        
        //likelihood function
        Qnet_diff[t]  = beta_t*(Qnet[2]-Qnet[1]); //higher values of Qdiff mean higher chance to choose right option.

 //calculating PEs
 PE_card =reward[t] - Q_cards[ch_card[t]];
 PE_key  =reward[t] - Q_keys[ch_key[t]];
 
//Update values ch_key=1 means choosing right
 Q_cards[ch_card[t]] += alpha_t * PE_card; //update card_value according to reward
 Q_keys[ch_key[t]]   += alpha_t * PE_key; //update key value according to reward
 

}
}

model {
  
  // Priors for group-level parameters
  mu_alpha ~ normal(0, 3);
  mu_beta ~ normal(0, 3);
  mu_lambda1 ~ normal(0, 3);
  mu_lambda2 ~ normal(0, 3);
  mu_lambda3 ~ normal(0, 3);
  
  
  // Priors for group-level standard deviations
  sigma_alpha ~ normal(0, 2);
  sigma_beta ~ normal(0, 2);
  sigma_lambda1 ~ normal(0, 2);
  sigma_lambda2 ~ normal(0, 2);
  sigma_lambda3 ~ normal(0, 2);
  
  // Priors for subject-specific effect
  alpha_raw~normal(0,1);
  beta_raw~normal(0,1);
  lambda1_raw~normal(0,1);
  lambda2_raw~normal(0,1);
  lambda3_raw~normal(0,1);
  

  target+= bernoulli_logit_lpmf(selected_offer | Qnet_diff);
 
}

