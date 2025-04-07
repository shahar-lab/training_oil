data {

  //General fixed parameters for the experiment/models

  int<lower=1> Ndata;

  int<lower=1> Nsubjects;


  array [Ndata] int<lower=1, upper=Nsubjects> subject_trial; // Which subject performed each trial

  
  int<lower=4> Narms;


  int<lower=2> Nraffle;

  

  //Behavioral data:

  

  array[Ndata] int<lower=0> ch_card;

  

  array[Ndata] int<lower=0> unch_card;

  

  array[Ndata] int reward;

  

  array[Ndata] int<lower=0> card_left;

  

  array[Ndata] int<lower=0> card_right;

  

  array[Ndata] int<lower=0> selected_offer;

  

  array[Ndata] int<lower=0> first_trial_in_block;

}
transformed data {

  real eps=1e-8;
}
parameters {

  //population level parameters 
  // Group-level (population) parameters
  real <lower=eps, upper=1-eps>mu_f;        // Mean forgetting across subjects
  real <lower=eps, upper=1-eps>mu_f_pr;          // Mean forgetting_pers across subjects
  
  real mu_c;        // Mean updating across subjects
  real mu_c_unch;          // Mean updating_unch across subjects
  real mu_pr;                //mean perseveration
  
  // Group-level standard deviations (for subject-level variability)
  real<lower=eps,upper=300> precision_f;       // Variability in threshold
  real<lower=eps,upper=300> precision_f_pr;          // Variability in non-decision time
  
  real<lower=eps> sigma_c;          // Variability in mf_updating
  real<lower=eps> sigma_c_unch;        // Variability mb_unch_updating
  real<lower=eps> sigma_pr;            // Variability pers
//individual level

  //standard
  vector[Nsubjects] c_sbj;
  
  vector[Nsubjects] c_unch_sbj;
  
  vector[Nsubjects] pr_sbj;
  
  //transformed
  vector<lower=eps, upper=1-eps>[Nsubjects] f_sbj;
  
  vector<lower=eps, upper=1-eps>[Nsubjects] f_pr_sbj;

}


transformed parameters {
  
  vector [Ndata]c_t;
	vector [Ndata]c_unch_t;
	vector [Ndata]pr_t;	  
  vector [Ndata]f_t;
	vector [Ndata]f_pr_t;
  
  vector[Narms] Qvalues;
  
  vector[Nraffle] Qnet;

  vector[Narms] pers;
  
  real reward_conv;
  
  vector[Ndata] Qnet_diff;

    //trial by trial loop
for (trial in 1 : Ndata){

  c_t[trial]=c_sbj[subject_trial[trial]];
	c_unch_t[trial]=c_unch_sbj[subject_trial[trial]];
	pr_t[trial]=pr_sbj[subject_trial[trial]];
	f_t[trial]=f_sbj[subject_trial[trial]];			
	f_pr_t[trial]=f_pr_sbj[subject_trial[trial]];
      //reset Qvalues (first trial only)

      if (first_trial_in_block[trial] == 1) {

        Qvalues = rep_vector(0, Narms);

        pers = rep_vector(0,Narms);

      }

      //calculate probability for each action

      Qnet[1] = Qvalues[card_left[trial]]+pers[card_left[trial]];

      Qnet[2] = Qvalues[card_right[trial]]+pers[card_right[trial]];
    
      Qnet_diff[trial] = Qnet[2] - Qnet[1]; // this is the value based upon we will calculate the likelihood .

      //update Qvalues
      if(reward[trial]==1){
        reward_conv=1;
      }
      else{
        reward_conv=-1;
      }
      //Qmf, forgetting and then updating
      Qvalues= (1-f_t[trial])*Qvalues;
      Qvalues[ch_card[trial]] = Qvalues[ch_card[trial]]
                                          + c_t[trial]*(reward_conv);

     
      Qvalues[unch_card[trial]] = Qvalues[unch_card[trial]]

                                         +c_unch_t[trial]*(reward_conv);

      //perseveration, forgetting and then updating
      pers=(1-f_pr_t[trial])*pers;
      pers[ch_card[trial]]=pers[ch_card[trial]]+pr_t[trial];
    }

}

model {

  // population level  

  // Priors for normal group-level parameters
  mu_c ~ normal(0, 1);
  mu_c_unch ~ normal(0, 1);
  
  // Priors for normal group-level standard deviations
  sigma_c ~ lognormal(0, 1);
  sigma_c_unch ~ lognormal(0, 1);
  
  // Priors for beta group-level parameters
  f_sbj  ~ beta(1.5,1.5);
  f_pr_sbj  ~ beta(1.5,1.5);
  
  // Priors for beta group-level precision
  precision_f     ~ gamma(2,0.05);
  precision_f_pr     ~ gamma(2,0.05);
  
  for (subject in 1 : Nsubjects) {
    //update parameter values
    
    //normal
    target+= normal_lpdf(c_sbj[subject]|mu_c, sigma_c);
    target+= normal_lpdf(c_unch_sbj[subject]|mu_c_unch , sigma_c_unch);
    target+= normal_lpdf(pr_sbj[subject]|mu_pr, sigma_pr);
    
    //transformed
    target+= beta_proportion_lpdf(f_sbj[subject]|mu_f,precision_f);
    target+= beta_proportion_lpdf(f_pr_sbj[subject]|mu_f_pr, precision_f_pr);
  }
    //update likelihood
    for (trial in 1 : Ndata) {

      target += bernoulli_logit_lpmf(selected_offer[trial] | Qnet_diff[trial]);

    }

}
