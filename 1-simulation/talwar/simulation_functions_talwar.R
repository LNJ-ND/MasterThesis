# Load libraries and suppress messages for group_by
pacman::p_load(tidyverse, random, brms)
options(dplyr.summarise.inform = FALSE)


######## General initialisation function #######################################################

# Function for initiating weights at the beginning of each stage
initiate_weights = function(stage, f1_updated, f2_updated, f3_updated, f4_updated) {
  
  # Stage 1: Initialise all feature weights to 0
  if (stage == 1) {
    
    f1 = 0 # Dimension 1
    f2 = 0 # Dimension 1
    f3 = 0
    f4 = 0
    
  # Stage 2: Reversal with 1 dimension, f3 and f4 remain unchanged (the second dimension)
  } else if (stage == 2) {
    
    f1 = f2_updated
    f2 = f1_updated
    f3 = 0
    f4 = 0
    
  # Stage 3: New dimension introduced, but initialised as 0
  } else if (stage == 3) {
    
    f1 = f1_updated  # Dimension 1
    f2 = f2_updated  # Dimension 1
    f3 = 0 # Dimension 2
    f4 = 0 # Dimension 2
      
  # Stage 4: Overlapping stimuli, but same relevance/correctness
  } else if (stage == 4) {
    
    f1 = f1_updated
    f2 = f2_updated
    f3 = f3_updated
    f4 = f4_updated
    
  # Stage 5: Reversal, same relevant dimension
  } else if (stage == 5) {
    
    f1 = f2_updated
    f2 = f1_updated
    f3 = f3_updated
    f4 = f4_updated
    
  # Stage 6: New stimuli, re-initialise all feature weights to 0
  } else if (stage == 6) {
    
    f1 = 0
    f2 = 0
    f3 = 0
    f4 = 0
  
  # Stage 7: Reversal, same relevant dimension
  } else if (stage == 7) {
    
    f1 = f2_updated
    f2 = f1_updated
    f3 = f3_updated
    f4 = f4_updated
    
  # Stage 8: New stimuli, and new relevant dimension. f1 and f2 now reflect dimension 2, f3 and f4 reflect dimension 1.
  # Re-initialise all feature weights to 0
  } else if (stage == 8) {
    
    f1 = 0 # Dimension 2
    f2 = 0 # Dimension 2
    f3 = 0 # Dimension 1
    f4 = 0 # Dimension 1
    
  # Stage 9: Reversal of relevant features 
  } else if (stage == 9) {
    
    f1 = f2_updated # Dimension 2
    f2 = f1_updated # Dimension 2
    f3 = f3_updated # Dimension 1
    f4 = f4_updated # Dimension 1
    
  }
  
  return(list(f1 = f1, f2 = f2, f3 = f3, f4 = f4))
}

######## Feature Reinforcement Learning Model #######################################################

# Parameters 
# alpha: learning rate for updating of the feature weights
# beta: inverse temperature/choice determinism

# Function for feature reinforcement learning model
fRL_model = function(stage, stimulus, f1, f2, f3, f4, alpha, beta) {
  
  # GET STIMULUS VALUES
  
  # Stage 1-2: Only one dimension. Values correspond to feature weights  
  if (stage < 3) {
    
    v1 = f1
    v2 = f2
    
  # Stage 3-9: Two dimensions. Values correspond to the sum of feature weights within the given stimulus    
  } else {
  
    # If stimulus set 1: f1, f3 are in the same stimulus, f2 and f4 are in the same stimulus
    if (stimulus == 1) {
      v1 = sum(f1, f3)
      v2 = sum(f2, f4)
    
    # If stimulus set 2: : f1, f4 are in the same stimulus, f2 and 3 are in the same stimulus  
    } else if (stimulus == 2) {
      
      v1 = sum(f1, f4)
      v2 = sum(f2, f3)
      
    }
  }
  
  # GET CORRECT PROBABILITY AND CHOICE
  
  # Probability of the correct choice (softmax function) based on expected values and inverse temperature
  correctp = exp(beta*v1) / (exp(beta*v1) + exp(beta*v2))
  
  # Generate choice based on rate of correctp (0 = incorrect, 1 = correct)
  choice = as.numeric(rbinom(1, 1, correctp))
  
  # FEATURE WEIGHT UPDATING
  # Stage 1-2: Only 1 dimension, therefore only updating f1 and f2. When correct (f1/v1) reward = 1. When incorrect (f2/v2) reward = -1.
  if (stage < 3) {
    
    f1_updated = f1 + alpha*(1 - v1) # Reward = 1
    f2_updated = f2 + alpha*(-1 - v2) # Reward = -1
    f3_updated = 0
    f4_updated = 0

  # Stage 3-9: 2 dimensions, therefore updating all four feature weights.  
  } else {
  
    # If stimulus set 1: When correct (f1/f3/v1) reward = 1. When incorrect (f2//f4/v2) reward = -1.
    if (stimulus == 1) {
      
      f1_updated = f1 + alpha*(1 - v1)
      f2_updated = f2 + alpha*(-1 - v2)
      f3_updated = f3 + alpha*(1 - v1)
      f4_updated = f4 + alpha*(-1 - v2)
      
    # If stimulus set 2: When correct (f1/f4/v1) reward = 1. When incorrect (f2//f3/v2) reward = -1.
    } else if (stimulus == 2) {
      
      f1_updated = f1 + alpha*(1 - v1)
      f2_updated = f2 + alpha*(-1 - v2)
      f3_updated = f3 + alpha*(-1 - v2)
      f4_updated = f4 + alpha*(1 - v1)
      
    }
  }
    
  # SAVE OUTPUT
  return(list(correctp = correctp,
              choice = choice,
              f1_updated = f1_updated, 
              f2_updated = f2_updated, 
              f3_updated = f3_updated,
              f4_updated = f4_updated))
  
}

######## Combined Attention-Modulated Feature Reinforcement Learning Model #######################################################

# Parameters 
# alpha: learning rate for updating of the feature weights
# beta: inverse temperature/choice determinism
# theta: dimension weight 

# Function for feature reinforcement learning model
cafRL_model = function(stage, stimulus, f1, f2, f3, f4, alpha, beta, theta) {
  
  # DEFINE FORMULA TO CALCULTE PROBABILITY FOR THE CORRECT CHOICE IN EACH STAGE
  
  # Stage 1-2: Stages with 1 dimension
  if (stage %in% c(1,2)) { 
    
    # Define choice probability for correct choice (f1 is the correct feature)
    correctp_formula = quote(exp(beta*f1) / (exp(beta*f1) + exp(beta*f2)))
    
    # Stage 3-7: Pre-ED stages with 2 dimensions
  } else if (stage %in% c(3,4,5,6,7)) {
    
    # If stimulus set 1 (f1f3, f2f4)
    if (stimulus == 1) {
      
      correctp_formula = quote((exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f3)) /    # v1 / 
                                  (exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f3)) +    # (v1 + 
                                     exp(beta*((1/(1+exp(-theta)))*f2 + (1-(1/(1+exp(-theta))))*f4)))))   # v2)
      
    # If stimulus set 2 (f1f4, f2f3)
    } else if (stimulus == 2) {
      
      correctp_formula = quote((exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f4)) /    # v1 / 
                                  (exp(beta*((1/(1+exp(-theta)))*f1 + (1-(1/(1+exp(-theta))))*f4)) +    # (v1 + 
                                     exp(beta*((1/(1+exp(-theta)))*f2 + (1-(1/(1+exp(-theta))))*f3)))))   # v2)
      
    }
    
    # Stage 8-9: Stages with 2 dimensions with ED shift
  } else if (stage %in% c(8,9)) {
    
    # If stimulus set 1 (f1f3, f2f4)
    if (stimulus == 1) {
      
      correctp_formula = quote(exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f3)) / # v1 / 
                                  (exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f3)) + # (v1 +
                                   exp(beta*((1-(1/(1+exp(-theta))))*f2 + (1/(1+exp(-theta)))*f4)))) # v2)
      
      # If stimulus set 2 (f1f4, f2f3)
    } else if (stimulus == 2) {
      
      correctp_formula = quote(exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f4)) / # v1 / 
                                 (exp(beta*((1-(1/(1+exp(-theta))))*f1 + (1/(1+exp(-theta)))*f4)) + # (v1 +
                                    exp(beta*((1-(1/(1+exp(-theta))))*f2 + (1/(1+exp(-theta)))*f3)))) # v2)
    
    }
  
  }
    
  # GET CORRECT PROBABILITY AND CHOICE
  
  # Get probability of being correct
  correctp = eval(correctp_formula)
  
  # Generate choice based on rate of correctp (0 = incorrect, 1 = correct)
  choice = as.numeric(rbinom(1, 1, correctp))
  
  # UPDATE THE FEATURE AND DIMENSION WEIGHTS
  
  # Calculate loss with the appropriate formula
  L = substitute((1 - correctp_formula)^2, list(correctp_formula=correctp_formula))
  
  # Update the feature weights and dimension weight (theta) with learning rate (alpha) through partial differentiation with respect to the given weight
  f1_updated = f1 - alpha*eval(D(L, "f1"))
  f2_updated = f2 - alpha*eval(D(L, "f2"))
  f3_updated = f3 - alpha*eval(D(L, "f3"))
  f4_updated = f4 - alpha*eval(D(L, "f4"))
  theta_updated = theta - alpha*eval((D(L, "theta")))
  
  # SAVE OUTPUT
  return(list(correctp = correctp,
              choice = choice,
              f1_updated = f1_updated, 
              f2_updated = f2_updated, 
              f3_updated = f3_updated,
              f4_updated = f4_updated,
              theta_updated = theta_updated))
  
}


######## Running Simulation with Model ##############################################################################################################

# Function for running either the t-fRL or t-cafRL models defined above
run_simulation = function(params, n_agents, sim_seeds) {
  
  # Reset simulation data frame
  simdf = NULL
  
  # Extract parameters
  model = params$model
  group = params$group
  n_correct = params$n_correct
  alpha = params$alpha
  beta = params$beta
  theta_init = params$theta_init
  epsilon = params$epsilon # Not relevant for fRL and the cafRL models implemented in this script
  
  # Define structure for experiment 
  n_trials = 50
  n_stages = 9
  
  # Loop through agents
  for (agent in 1:n_agents) {
    
    # Set seed for the agent
    set.seed(sim_seeds[agent])
    
    # Reset end experiment for agent
    end_experiment = F
    
    # Loop through stages
    for (stage in 1:n_stages) {
      
      # If 50 trials reached, experiment will be ended
      if (end_experiment == T) {break}
      
      # Reset correct count in each stage
      correct_count = 0
      
      # Define vectors to save data from the stage
      trial_vector = rep(NA, n_trials)
      stimulus_vector = rep(NA, n_trials)
      f1_vector = rep(NA, n_trials) 
      f2_vector = rep(NA, n_trials)
      f3_vector = rep(NA, n_trials)
      f4_vector = rep(NA, n_trials)
      theta_vector = rep(NA, n_trials)
      correctp_vector = rep(NA,n_trials)
      choice_vector = rep(NA, n_trials)
      
      # Loop through trials
      for (t in 1:n_trials) {
        
        # DEFINE FEATURE WEIGHTS
        # If trial 1: initiate weights as specified by initialisation function and the given stage
        if (t == 1) {f = initiate_weights(stage, f1_updated, f2_updated, f3_updated, f4_updated)}
        # If not trial 1: update weights as specified by update function
        else {f = list(f1 = f1_updated, f2 = f2_updated, f3 = f3_updated, f4 = f4_updated)}
        
        # Save feature weights
        f1_vector[t] = f$f1
        f2_vector[t] = f$f2
        f3_vector[t] = f$f3
        f4_vector[t] = f$f4
    
        # DEFINE DIMENSION WEIGHT (only applies to cafRL)
        # If stage 1-2: 1 dimension
        if (stage %in% c(1,2)) {
          
          # No theta because only one dimension
          theta_vector[t] = NA
          
        # If stage 3 in cafRL model (not fRL) when dimension 2 is introduced
        } else if ((stage == 3) & (model != "fRL")) {
          
          # If trial 1, use the initial theta values parameter, otherwise the updated one
          theta_vector[t] = ifelse(t == 1, theta_init, theta_updated)
        
        # If stage 4-9 in cafRL model (not fRL)
        } else if ((stage > 3) & (model != "fRL")) { 
          
          # Use the updated theta (i.e. no re-initialisation)
          theta_vector[t] = theta_updated
          
        }
        
        # GENERATE STIMULI 
        # If stage 1-2: Stimulus set 1. If stage 3-9: sample equally from stimulus set 1 or stimulus set 2 (rate of 0.5)
        stimulus_vector[t] = ifelse(stage %in% c(1,2), 1, (rbinom(1,1,0.5)+1)) 
        
        # GENERATE CHOICE AND UPDATE WEIGHTS DEPENDING ON MODEL
        # If fRL model use fRL function
        if (model == "fRL") { 
          
          out = fRL_model(stage, stimulus_vector[t], f1_vector[t], f2_vector[t], f3_vector[t], f4_vector[t], alpha, beta)
        
        # If cafRL model use cafRL function
        } else if (model == "cafRL") {
        
          out = cafRL_model(stage, stimulus_vector[t], f1_vector[t], f2_vector[t], f3_vector[t], f4_vector[t], alpha, beta, theta_vector[t])
          
        } 
        
        # SAVE FOR NEXT TRIAL
        f1_updated = out$f1_updated
        f2_updated = out$f2_updated
        f3_updated = out$f3_updated
        f4_updated = out$f4_updated
        theta_updated = out$theta_updated
        
        # SAVE OTHER RELVANT DATA
        trial_vector[t] = t
        correctp_vector[t] = out$correctp
        choice_vector[t] = out$choice
        
        # MOVE TO NEXT STAGE IF CRITERION FULFILLED
        # Running count of correct responses (if correct choice, add 1 to count, otherwise reset to 0)
        correct_count = ifelse(choice_vector[t] == 1, correct_count+1, 0)
        # If achieved n_correct number of correct responses, move to next stage
        if (correct_count == n_correct) {break}
        
        # END EXPERIMENT IF 50 TRIALS REACHED
        # If done with trial 50 at this point, end experiment in beginning of next stage
        if (t == 50) {end_experiment = T}
        # If not at trial 50, do not end experiment in beginning of next stage
        else {(end_experiment = F)}
        
      }
      
      # SAVE STAGE DATA
      temp = tibble(
        # Over trial parameters
        model = model, 
        group = group, 
        agent = agent,
        seed_agent = sim_seeds[agent],
        stage = stage, 
        trial = trial_vector,
        f1 = f1_vector, 
        f2 = f2_vector,
        f3 = f3_vector, 
        f4 = f4_vector,
        theta = theta_vector,
        stimulus = stimulus_vector,
        correctp = correctp_vector,
        choice = choice_vector,
        stage_completed = ifelse(end_experiment == F, 1, 0),
        # Fixed parameters
        n_correct = n_correct,
        alpha = alpha,
        beta = beta,
        theta_init = theta_init,
        epsilon = epsilon # Not relevant for fRL and the cafRL models implemented in this script
      )
      
      # Save files
      # Remove rows without data (stage ended after 6 consecutive correct)
      temp = temp[!is.na(temp$trial),]
      # If simulated data frame already exists: bind to simulated data frame, else create simulated data frame
      if (exists("simdf")) {simdf = rbind(simdf, temp)} else {simdf = temp}
      
    }
  }
  # Return simulated data frame
  return(simdf)
}