// Y-RL Model ----------------------------------
// For mapping of variable names see README file

// Functions used in the model
functions {
  
  // Function to get sum while ignoring nan
  real nansum(vector x){
    vector[4] y;
    for (i in 1:num_elements(x)){y[i] = is_nan(x[i]) ? 0 : x[i];}
    return sum(y);
  } 
  
  // Function for initiating weights in first trial of each stage
  // This ensures that att1 (f1) always represents the weight of the correct feauture in a given trial
  vector initiate_att(vector att_updated, vector att_switch, int stage) {
    
    // Define output vector
    vector[4] att;
      
    // Reversal stages
    if (stage == 2 || stage == 5 || stage == 7 || stage == 9){
      // Switch attentions of the previously correct and previously incorrect feature, the remaining features remain
      att[1] = att_updated[2];
      att[2] = att_updated[1];
      att[3] = att_updated[3];
      att[4] = att_updated[4];
    }
    
    // Stage 3: Features of the second dimension are introduced 
    else if (stage == 3){
      att[1] = (1-att_switch[1])*0.25 + att_switch[1]*att_updated[1];
      att[2] = (1-att_switch[1])*0.25 + att_switch[1]*att_updated[2];
      att[3] = (1-att_switch[1])*0.25;
      att[4] = (1-att_switch[1])*0.25;
    }
    
    // Stage 4: Overlapping features
    else if (stage == 4){
      att[1] = (1-att_switch[2])*0.25 + att_switch[2]*att_updated[1];
      att[2] = (1-att_switch[2])*0.25 + att_switch[2]*att_updated[2];
      att[3] = (1-att_switch[2])*0.25 + att_switch[2]*att_updated[3];
      att[4] = (1-att_switch[2])*0.25 + att_switch[2]*att_updated[4];
    }
    
    // Stage 6: ID, averaging within dimensions
    else if (stage == 6){
      att[1] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[1]+att_updated[2])/2;
      att[2] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[1]+att_updated[2])/2;
      att[3] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[3]+att_updated[4])/2;
      att[4] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[3]+att_updated[4])/2;
    }
    
    // Stage 8: ED, averaging within dimensions and reversing the dimensions
    else if (stage == 8){
      att[1] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[3]+att_updated[4])/2;
      att[2] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[3]+att_updated[4])/2;
      att[3] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[1]+att_updated[2])/2;
      att[4] = (1-att_switch[2])*0.25 + att_switch[2]*(att_updated[1]+att_updated[2])/2;
    }
    
    // Return attention vector
    return att;
  }
  
  // Function to update attention after choice
  vector update_att(int stage, int stimulus, int choice, vector att, real f, real alpha_pos, real alpha_neg) {
    
    // Define values
    vector[4] weights;
    vector[4] att_updated;
    
    // Reset weights to make sure summation makes sense
    weights = [0,0,0,0]';
    
    // If stage 1 or 2 (i.e. only one dimensions)
    if (stage <= 2){
      
      // Get the weights
      weights[1] = att[2]; 
      weights[2] = - att[2];
      // weights 3 and 4 should be NA/0
      
      // Update attention, while keeping the att3 and att4 at 0
      // If the choice was correct
      if (choice == 1){
        att_updated[1] = att[1] + inv_logit(alpha_pos)*weights[1];
        att_updated[2] = att[2] + inv_logit(alpha_pos)*weights[2];
        att_updated[3] = 0;
        att_updated[4] = 0;
      }
      
      // If the choice was incorrect
      else if (choice == 0){
        att_updated[1] = att[1] + inv_logit(alpha_neg)*weights[1];
        att_updated[2] = att[2] + inv_logit(alpha_neg)*weights[2];
        att_updated[3] = 0;
        att_updated[4] = 0;
      }
    }
      
    // In all other stages
    else if (stage >= 3){
      
      // Get the weights
      // If stimulus set 1: correct stim: f1/f3, incorrect stim: f2/f4
      if (stimulus == 1){ 
        weights[1] = (1-inv_logit(f))*att[2] + inv_logit(f)*att[4];
        weights[2] = - att[2];
        weights[3] = (1-inv_logit(f))*att[4] + inv_logit(f)*att[2];
        weights[4] = - att[4];
        
      // If stimulus set 2: correct stim: f1/f4, incorrect stim: f2/f3
      } else if (stimulus == 2) { 
        weights[1] = (1-inv_logit(f))*att[2] + inv_logit(f)*att[3];
        weights[2] = - att[2];
        weights[3] = - att[3];
        weights[4] = (1-inv_logit(f))*att[3] + inv_logit(f)*att[2];
      }
      
      // Update attention 
      // If the choice was correct
      if (choice == 1){
        att_updated[1] = att[1] + inv_logit(alpha_pos)*weights[1];
        att_updated[2] = att[2] + inv_logit(alpha_pos)*weights[2];
        att_updated[3] = att[3] + inv_logit(alpha_pos)*weights[3];
        att_updated[4] = att[4] + inv_logit(alpha_pos)*weights[4];
      }
        
      // If the choice was incorrect
      else if (choice == 0){
        att_updated[1] = att[1] + inv_logit(alpha_neg)*weights[1];
        att_updated[2] = att[2] + inv_logit(alpha_neg)*weights[2];
        att_updated[3] = att[3] + inv_logit(alpha_neg)*weights[3];
        att_updated[4] = att[4] + inv_logit(alpha_neg)*weights[4];
      }
    }
    
    // Sanity check: do the weights sum to 0
    if (nansum(weights) < -0.0000005 || nansum(weights) > 0.0000005){
      print("Non-0 weights: ", nansum(weights), " weights: ", weights,  "(", stage, ")");
    }
    
    // Sanity check: do the att's sum to 1
    if (nansum(att_updated) < 0.999995 || nansum(att_updated) > 1.0000005){
      print("Non-1 att updated: ", nansum(att_updated), " att updated: ", att_updated, " (", stage, ")");
    }
    
    return att_updated;
  }
  
  // Function to get choice probability (i.e. probability for the choosing the correct stimulus)
  real get_choicep(int stage, int trial, int stimulus, vector att, real d, real lambda) {
    
    real correct_stim_att;
    real d_param;
    real choicep;
    
    // Get decision consistency parameter
    d_param = d*exp(-lambda*(trial - 1));
    
    // Get att on correct stimulus
    // If stimulus set 1
    if (stimulus == 1) {
      
      correct_stim_att = att[1]^d_param + att[3]^d_param;
      
    // If stimulus set 2
    } else if (stimulus == 2) {
      
      correct_stim_att = att[1]^d_param + att[4]^d_param;
      
    }
    
    // Calcualte choice probability for correct choice using softmax rule
    choicep = correct_stim_att /  (att[1]^d_param + att[2]^d_param + att[3]^d_param + att[4]^d_param);
    
    // Return the choice probability
    return choicep;
    
  }
  
}

// INPUT DATA -------------------------------------------------------------------------------------------------------
data {
  
  // Input from task
  int<lower=1> n_trials; // Total number of trials (in stage)
  array[n_trials] int<lower=1, upper=9> stage; // Stage that the participant is in
  array[n_trials] int<lower=1, upper=50> trial; // Trial within the stage (resets at beginning of stage)
  array[n_trials] int<lower=0, upper=1> choice; // Agents choice of 1 (correct) or 0 (incorrect)
  array[n_trials] int<lower=1, upper=2> stimulus; // Stimulus set 1 (1/3, 2/4) or 2 (1/4, 2/3)
  
  // Input for priors for parameters that are estimated
  // These should be defined in log odds in R
  real alpha_pos_mean;
  real alpha_pos_sd;
  real alpha_neg_mean;
  real alpha_neg_sd;
  real f_mean;
  real f_sd;
}


// FIXED DATA --------------------------------------------------------------------------------------------------------
transformed data {
  
  real d; // Decision consistency (d0)
  real lambda; // Docus, disengagement parameter
  vector[2] att_switch; // Control switch (S1), continuity parameter
  vector[4] init_att; // Inital attention biases for for S1/S2, i.e. equally distributed
  
  d = 3;
  lambda = 0.05; 
  att_switch = [0.95, 0.95]';
  init_att = [0.5, 0.5, 0, 0]';  
}


// PARAMETERS TO ESTIMATE ---------------------------------------------------------------------------------------------
parameters {
  
  real alpha_pos; // Learning rate for positive feedback (r), reward learning rate
  real alpha_neg; // Learning rate for negative feedback (p), punishment learning rate
  real f; // Attention switch parameter, dimension shift parameter
}


// MODEL TO ESTIMATE PARAMETERS ----------------------------------------------------------------------------------------
model {
  
  // Define variables that are used in the model
  real choicep;
  vector[4] att;
  vector[4] att_updated;
  
  // Define Priors
  target += normal_lpdf(alpha_pos | alpha_pos_mean, alpha_pos_sd); 
  target += normal_lpdf(alpha_neg | alpha_neg_mean, alpha_neg_sd); 
  target += normal_lpdf(f | f_mean, f_sd); 
  
  // Loop through the trials
  for (t in 1:n_trials) {
    
    // In trial 1 of stage: if stage 1, init_att, in other stages, initiate based on function
    if (trial[t] == 1){
      att = stage[t] == 1 ? init_att : initiate_att(att_updated, att_switch, stage[t]);
    
    // In all other trials, use the updated att from previous trial
    } else {
      att = att_updated;
    }
    
    // Get choice p for correct stimulus
    choicep = get_choicep(stage[t], trial[t], stimulus[t], att, d, lambda);
  
    // Choice based on choice probability (choice = 1: correct, choice = 0: incorrect)
    target += bernoulli_lpmf(choice[t] | choicep); 
    
    // Update att based on the choice
    att_updated = update_att(stage[t], stimulus[t], choice[t], att, f, alpha_pos, alpha_neg);

  }
}


// GENERATED QUANTATIES FOR OUTPUT --------------------------------------------------------------------------------------
generated quantities {
  
  // Define priors
  real alpha_pos_prior_log;
  real alpha_neg_prior_log;
  real f_prior_log;
  
  real<lower=0, upper=1> alpha_pos_prior_p;
  real<lower=0, upper=1> alpha_neg_prior_p;
  real<lower=0, upper=1> f_prior_p;
  
  // Define posteriors
  real alpha_pos_posterior_log;
  real alpha_neg_posterior_log;
  real f_posterior_log;
  
  real<lower=0, upper=1> alpha_pos_posterior_p;
  real<lower=0, upper=1> alpha_neg_posterior_p;
  real<lower=0, upper=1> f_posterior_p;
  
  // Prior and posterior predictions
  vector<lower=0, upper=1>[n_trials] prior_pred;
  vector<lower=0, upper=1>[n_trials] posterior_pred;
  
  // Log likelihood for model comparison 
  array[n_trials] real log_lik;
  real choicep;
  vector[4] att;
  vector[4] att_updated;
   
  // Save priors
  alpha_pos_prior_log = normal_rng(alpha_pos_mean, alpha_pos_sd);
  alpha_neg_prior_log = normal_rng(alpha_neg_mean, alpha_neg_sd);
  f_prior_log = normal_rng(f_mean, f_sd);
  
  alpha_pos_prior_p = inv_logit(normal_rng(alpha_pos_mean, alpha_pos_sd));
  alpha_neg_prior_p = inv_logit(normal_rng(alpha_neg_mean, alpha_neg_sd));
  f_prior_p = inv_logit(normal_rng(f_mean, f_sd));
  
  // Save posteriors 
  alpha_pos_posterior_log = alpha_pos;
  alpha_neg_posterior_log = alpha_neg;
  f_posterior_log = f;
  
  alpha_pos_posterior_p = inv_logit(alpha_pos);
  alpha_neg_posterior_p = inv_logit(alpha_neg);
  f_posterior_p = inv_logit(f);
  
  // Get prior predictions (see code comments in model section above)
  for (t in 1:n_trials) {
    
    if (trial[t] == 1){
      att = stage[t] == 1 ? init_att : initiate_att(att_updated, att_switch, stage[t]);
    
    } else {
      att = att_updated;
    }
    
    choicep = get_choicep(stage[t], trial[t], stimulus[t], att, d, lambda);
    prior_pred[t] = bernoulli_rng(choicep);
    att_updated = update_att(stage[t], stimulus[t], choice[t], att, f_prior_log, alpha_pos_prior_log, alpha_neg_prior_log);
    
  }
  
  // Get posterior predictions and loglik (see code comments in model section above)
  for (t in 1:n_trials) {
    
    if (trial[t] == 1){
      att = stage[t] == 1 ? init_att : initiate_att(att_updated, att_switch, stage[t]);
    
    } else {
      att = att_updated;
    }
    
    choicep = get_choicep(stage[t], trial[t], stimulus[t], att, d, lambda);
    posterior_pred[t] = bernoulli_rng(choicep);
    log_lik[t] = bernoulli_lpmf(choice[t] | choicep);
    att_updated = update_att(stage[t], stimulus[t], choice[t], att, f, alpha_pos, alpha_neg);
    
  }
  
}

