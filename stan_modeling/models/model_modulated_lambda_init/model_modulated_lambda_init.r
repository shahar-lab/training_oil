#### simulate Rescorla-Wagner block for participant ----
sim.block = function(subject,parameters,cfg){ 
  print(paste('subject',subject))
  
  #pre-allocation
  
  #set parameters
  alpha = parameters['alpha']
  beta  = parameters['beta']
  lambda0 = parameters['lambda0']
  lambda1 = parameters['lambda1']
  #set initial var
  Narms              = cfg$Narms
  Nraffle            = cfg$Nraffle
  Nblocks            = cfg$Nblocks
  Ntrials   = cfg$Ntrials
  Ndims              = cfg$Ndims
  #expvalues          = cfg$rndwlk
  #rownames(expvalues)=c('ev1','ev2','ev3','ev4')
  df                 =data.frame()
  prior_relevant=c(1,0)
  weight_irrelevant=c(0,1)
  lambda = lambda0
  for (block in 1:Nblocks){

    expvalues=t(data.frame(a=rep(0.8,Ntrials),b=rep(0.9,Ntrials),c=rep(0.8,Ntrials),d=rep(0.9,Ntrials)))

    Q_cards= rep(0.5, Narms)
    Q_keys = rep(0.5, Nraffle)

    for (trial in 1:Ntrials){
      #computer offer
      pair  = sample(1:2,1)
      if(pair==1){
        options=sample(1:2,2)
      }
      else{
        options=sample(3:4,2)
      }
      #value of offered cards
      Q_cards_offered = Q_cards[options] #use their Q values

      
      #updating weights
      transformed_lambda=inv_logit_scaled(lambda)
      weights= transformed_lambda* prior_relevant + (1 - transformed_lambda) * weight_irrelevant
      Qnet = weights[1]*Q_cards_offered + weights[2]*Q_keys
      
      p= exp(beta * Qnet) / sum(exp(beta * Qnet)) #get prob for each action
      #players choice
      ch_card = sample(options, 1, prob = p) #chose a card according to probs
      unch_card=options[which(options != ch_card)]
      ch_key = which(options == ch_card) #get key of chosen card 1 =left
      unch_key = which(options!=ch_card)
      #outcome 
      reward = sample(0:1, 1, prob = c(1 - expvalues[ch_card, trial], expvalues[ch_card, trial])) #reward according to card
      
      #calc PE
      PE_keys= reward-Q_keys[ch_key]
      PE_cards=reward-Q_cards[ch_card]

      # diff_reliability [-1, 1]
      diff_reliability=abs(PE_keys)-abs(PE_cards)
      
      lambda=lambda + lambda1 * diff_reliability
  
      #save trial's data
      
      #create data for current trials
      dfnew=data.frame(
        subject,
        block,
        trial,
        card_right = options[2],
        card_left = options[1],
        ch_card,
        ch_key,
        selected_offer=ch_key-1,
        reward,
        first_trial_in_block=if_else(trial==1,1,0),
        first_trial=if_else(trial==1&block==1,1,0),
        Q_ch_card = Q_cards[ch_card], #to get previous trial
        Q_unch_card = Q_cards[options[which(options != ch_card)]],
        Q_right_card = Q_cards[options[2]],
        Q_left_card = Q_cards[options[1]],
        Q_left_key = Q_keys[1],
        Q_right_key = Q_keys[2],
        exp_val_right=expvalues[options[2], trial],
        exp_val_left=expvalues[options[1], trial],
        exp_val_ch = expvalues[ch_card, trial],
        exp_val_unch = expvalues[options[which(options != ch_card)], trial],
        Q_ch_key = Q_keys[ch_key],
        Q_unch_key = Q_keys[which(options != ch_card)],
        PE_cards,
        PE_keys,
        alpha,
        beta,
        lambda1 = lambda1,
        lambda0 = lambda0,
        lambda=lambda,
        diff=PE_keys-PE_cards,
        weight_card=weights[1],
        weight_key=weights[2],
        diff_reliability
      )
      df=rbind(df,dfnew)
      #updating Qvalues
      
      Q_cards[ch_card] = Q_cards[ch_card]  + alpha * PE_cards
      #Q_cards[unch_card]=Q_cards[unch_card]+alpha *(1-reward-Q_cards[unch_card])

      Q_keys[ch_key] = Q_keys[ch_key] +alpha * PE_keys
     # Q_keys[unch_key]=Q_keys[unch_key]+alpha*(1-reward-Q_keys[unch_key])

    }
  }     
  
  return (df)
}