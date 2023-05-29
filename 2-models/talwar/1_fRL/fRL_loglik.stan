// T-fRL Model ----------------------------------
// For mapping of variable names see README file

// Functions used in the model
functions {
  
  // Function to get sum while ignoring nan
  real nansum(vector x){
    
    vector[4] y;
    for (i in 1:num_elements(x)){y[i] = is_nan(x[i]) ? 0 : x[i];}

    return sum(y);
    
  } 
  
  // Function to initiate weights in first trial of each stage 
  vector initiate_weights(int stage, vector f_updated) {
    
    vector[4] f;
    
    // Stage 1: Initialize to 0
    if (stage == 1) {
      
      f[1] = 0;
      f[2] = 0;
      f[3] = 0;
      f[4] = 0;
      
    // Stage 2: Reversal with 1 dimension
    } else if (stage == 2) {
      
      f[1] = f_updated[2];
      f[2] = f_updated[1];
      f[3] = 0;
      f[4] = 0;
      
    // Stage 3: New dimension introduced, initialised to 0
    } else if (stage == 3) {
      
      f[1] = f_updated[1];
      f[2] = f_updated[2];
      f[3] = 0;
      f[4] = 0;
        
    // Stage 4: Overlapping stimuli, but same relevance/correctness
    } else if (stage == 4) {
      
      f[1] = f_updated[1];
      f[2] = f_updated[2];
      f[3] = f_updated[3];
      f[4] = f_updated[4];
      
    // Stage 5: Reversal, same relevant dimension
    } else if (stage == 5) {
      
      f[1] = f_updated[2];
      f[2] = f_updated[1];
      f[3] = f_updated[3];
      f[4] = f_updated[4];
      
    // Stage 6: ID, new stimuli and resetting of weights
    } else if (stage == 6) {
      
      f[1] = 0;
      f[2] = 0;
      f[3] = 0;
      f[4] = 0;
    
    // Stage 7: Reversal, same relevant dimension
    } else if (stage == 7) {
      
      f[1] = f_updated[2];
      f[2] = f_updated[1];
      f[3] = f_updated[3];
      f[4] = f_updated[4];
      
    // Stage 8: ED, new stimuli, and new relevant dimension, all reset to 0
    } else if (stage == 8) {
      
      f[1] = 0;
      f[2] = 0;
      f[3] = 0;
      f[4] = 0;
      
    // Stage 9: Reversal, same relevant dimension
    } else if (stage == 9) {
      
      f[1] = f_updated[2];
      f[2] = f_updated[1];
      f[3] = f_updated[3];
      f[4] = f_updated[4];
      
    }
    
    // Return feature weights
    return f;
  }
  
  // Function to get the expected values of a stimuli
  vector get_values(int stage, int stimulus, vector f) {
    
    vector[2] V;
    
    // If stage with only one dimensions, the value equals the feature weight
    if (stage < 3) {
      
      V[1] = f[1];
      V[2] = f[2];
      
    // If two dimensions, the value is the sum of the feature weights
    } else {
    
      // If stimulus set 1: f1/f3; f2/f4
      if (stimulus == 1) {
        
        V[1] = f[1] + f[3];
        V[2] = f[2] + f[4];
        
      // If stimulus set 2:  f1/f4; f2/f3
      } else if (stimulus == 2) {
        
        V[1] = f[1] + f[4];
        V[2] = f[2] + f[3];
        
      }
    }
    
    // Return expected value
    return V;
  }
  
  // Function to update the feature weights
  vector update_weights(int stage, int stimulus, vector f, vector V, real alpha) {
    
    // R = 1 for the correct stimulus and R = -1 for the incorrect stimulus 
    
    vector[4] f_updated;
    
    // If only one dimension, only update f1 and f2
    if (stage < 3) {
    
      f_updated[1] = f[1] + inv_logit(alpha)*(1 - V[1]);
      f_updated[2] = f[2] + inv_logit(alpha)*(-1 - V[2]);
      f_updated[3] = 0;
      f_updated[4] = 0;
  
    // If two dimensions in the stimulus, update all values
    } else {
    
      // For stimulus set 1: f1/f3 correct, f2f4 incorrect
      if (stimulus == 1) {
        
        f_updated[1] = f[1] + inv_logit(alpha)*(1 - V[1]);
        f_updated[2] = f[2] + inv_logit(alpha)*(-1 - V[2]);
        f_updated[3] = f[3] + inv_logit(alpha)*(1 - V[1]);
        f_updated[4] = f[4] + inv_logit(alpha)*(-1 - V[2]);
        
      // For stimulus set 2: f1/f4 correct, f2f3 incorrect
      } else if (stimulus == 2) {
        
        f_updated[1] = f[1] + inv_logit(alpha)*(1 - V[1]);
        f_updated[2] = f[2] + inv_logit(alpha)*(-1 - V[2]);
        f_updated[3] = f[3] + inv_logit(alpha)*(-1 - V[2]);
        f_updated[4] = f[4] + inv_logit(alpha)*(1 - V[1]);
        
      }
    }
    
    // Return the updated feature weights
    return f_updated;
  }
}

// INPUT DATA -------------------------------------------------------------------------------------------------------
data {
  
  // Input from task
  int<lower=1> n_trials; // Total number of trials (in stage)
  array[n_trials] int<lower=1, upper=9> stage; // Stage that the participant is in
  array[n_trials] int<lower=1, upper=50> trial; // Trial within the stage (resets at beginning of stage)
  array[n_trials] int<lower=0, upper=1> choice; // Agents choice of 1 (chose S2) or 2 (chose S1)
  array[n_trials] int<lower=1, upper=2> stimulus; // Stimulus set 1 or 2 (here S1/S2)
  
  // Input for priors for parameters that are estimated
  real alpha_mean; // Learning rate (on log odds), generic learning rate
  real alpha_sd; 
  real<lower=0> beta_beta; // Inverse temperature, choice determinism
  real<lower=0> beta_alpha;
}


// PARAMETERS TO ESTIMATE ---------------------------------------------------------------------------------------------
parameters {
  
  real alpha; // Learning rate 
  real<lower=0.0000001> beta; // Inverse temperature, range of 0-inf
}


// MODEL TO ESTIMATE PARAMETERS ----------------------------------------------------------------------------------------
model {
  
  // Define variables that are used in the model
  vector[4] f; // Vector of feature weights
  vector[4] f_updated; // Vector of updated feature weights after trial
  vector[2] V; // Stimulus values (knowing that V[1] is the correct stimulus)
  real correctp; // Probability of making the correct choice 
  
  // Priors
  target += normal_lpdf(alpha | alpha_mean, alpha_sd); // Learning rate 
  target += gamma_lpdf(beta | beta_alpha, beta_beta); // Inverse temperature
  
  // Loop through the trials
  for (t in 1:n_trials) {
    
    // Get feature weights
    // If it's the first trial in stage 1, initialse all to 0, otherwise use the updated ones
    f_updated = (t == 1) ? [0,0,0,0]' : f_updated;
    // Use the updated features as features in the given trial, and if it's the first trial, re-initiate them 
    f = (trial[t] == 1) ? initiate_weights(stage[t], f_updated) : f_updated; 
    
    // Get stimulus values
    V = get_values(stage[t], stimulus[t], f);
    
    // Get probability for correct choice
    correctp = softmax(beta*V)[1];
    
    // Get choice based on probability (1=correct, 0=false)
    target += bernoulli_lpmf(choice[t] | correctp);
    
    // Update feature weights 
    f_updated = update_weights(stage[t], stimulus[t], f, V, alpha);
    
    // Sanity check feature weigths
    if (nansum(f_updated) < -0.0000005 || nansum(f_updated) > 0.0000005){
      print("Non-0 features: ", nansum(f_updated), " weights: ", f_updated,  "(", stage[t], ", ", trial[t], ")");
    }
  }
}


// GENERATED QUANTATIES FOR OUTPUT --------------------------------------------------------------------------------------
generated quantities {
  
  // Define priors
  real alpha_prior_log;
  real<lower=0, upper=1> alpha_prior_p;
  real beta_prior;
  
  // Define posteriors
  real alpha_posterior_log;
  real<lower=0, upper=1> alpha_posterior_p;
  real beta_posterior;
  
  // Prior and posterior predictions
  vector<lower=0, upper=1>[n_trials] prior_pred;
  vector<lower=0, upper=1>[n_trials] posterior_pred;
  
  // Define log likelihood for model comparison 
  array[n_trials] real log_lik;
  vector[4] f; // Vector of feature weights
  vector[4] f_updated; // Vector of updated feature weights after trial
  vector[2] V; // Stimulus values (knowing that V[1] is the correct stimulus)
  real correctp; // Probability of making the correct choice 
  
  // Save priors
  alpha_prior_log = normal_rng(alpha_mean, alpha_sd);
  alpha_prior_p = inv_logit(normal_rng(alpha_mean, alpha_sd));
  beta_prior = gamma_rng(beta_alpha, beta_beta);
  
  // Save posteriors 
  alpha_posterior_log = alpha;
  alpha_posterior_p = inv_logit(alpha);
  beta_posterior = beta;
  
  // Get prior predictions (see code comments in model section above)
  for (t in 1:n_trials) {
    
    f_updated = (t == 1) ? [0,0,0,0]' : f_updated;
    
    f = (trial[t] == 1) ? initiate_weights(stage[t], f_updated) : f_updated; 
    
    V = get_values(stage[t], stimulus[t], f);
    
    correctp = softmax(beta_prior*V)[1];
    
    prior_pred[t] = bernoulli_rng(correctp);
    
    f_updated = update_weights(stage[t], stimulus[t], f, V, alpha_prior_log);
    
  }
  
  // Get posterior predictions and loglik (see code comments in model section above)
  for (t in 1:n_trials) {
    
    f_updated = (t == 1) ? [0,0,0,0]' : f_updated;
    
    f = (trial[t] == 1) ? initiate_weights(stage[t], f_updated) : f_updated; 
    
    V = get_values(stage[t], stimulus[t], f);
    
    correctp = softmax(beta*V)[1];
    
    posterior_pred[t] = bernoulli_rng(correctp);
    
    log_lik[t] = bernoulli_lpmf(choice[t] | correctp);
    
    f_updated = update_weights(stage[t], stimulus[t], f, V, alpha);
    
  }  
}


