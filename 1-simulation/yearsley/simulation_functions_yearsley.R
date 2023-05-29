# Suppress messages for group_by
options(dplyr.summarise.inform = FALSE)

# Function for initiating attentions (feature weights) in the first trial of each stage
initiate_att = function(stage, att_updated, switch1, switch2) {
  
  # Stage 1: Initialise with equally distributed attention between the two features (only one dimension)
  if (stage == 1) {
    att1 = 0.5
    att2 = 0.5
    att3 = NA
    att4 = NA}
  
  # Stage 2-9:
  else {
    
    # Get the attentions from the previous trial
    att1_updated = att_updated$att1_updated
    att2_updated = att_updated$att2_updated
    att3_updated = att_updated$att3_updated
    att4_updated = att_updated$att4_updated
    
    # Reversal stage (1 dimension)
    else if (stage == 2) {
      # Take the last value of the previous stage (i.e. from the last trial before moving to the next stage)
      att1 = att2_updated
      att2 = att1_updated
      att3 = NA
      att4 = NA}
    
    # Reversal stages (2 dimensions)
    reversal_stages = c(5,7,9)
    if (stage %in% reversal_stages) {
      # Reversal of attentions within dimension
      att1 = att2_updated
      att2 = att1_updated
      att3 = att3_updated
      att4 = att4_updated}
    
    # Stage 3: Features of the second dimension are introduced
    else if (stage == 3) {
      # Switch1 defines balance between keeping attention from known features, and allow some spreading to the new features/dimension
      att1 = (1 - switch1)*0.25 + switch1*att1_updated
      att2 = (1 - switch1)*0.25 + switch1*att2_updated
      att3 = (1 - switch1)*0.25
      att4 = (1 - switch1)*0.25}
    
    # Stage 4: Overlapping features
    else if (stage == 4) {
      # Move to the center, add uncertainty
      att1 = (1 - switch2)*0.25 + switch2*att1_updated
      att2 = (1 - switch2)*0.25 + switch2*att2_updated
      att3 = (1 - switch2)*0.25 + switch2*att3_updated
      att4 = (1 - switch2)*0.25 + switch2*att4_updated}
    
    # Stage 6: ID, averaging within dimensions. New stimuli.
    else if (stage == 6) {
      # Averaged attention of features within dimensions, increasing uncertainty
      att1 = (1 - switch2)*0.25 + switch2*(att1_updated + att2_updated)/2
      att2 = (1 - switch2)*0.25 + switch2*(att1_updated + att2_updated)/2
      att3 = (1 - switch2)*0.25 + switch2*(att3_updated + att4_updated)/2
      att4 = (1 - switch2)*0.25 + switch2*(att3_updated + att4_updated)/2}
    
    # Stage 8: ED, averaging within dimensions and reversing the dimensions. New stimuli.
    else if (stage == 8) {
      # Averaged attention of features within dimensions, but reversed, increasing uncertainty
      att1 = (1 - switch2)*0.25 + switch2*(att3_updated + att4_updated)/2
      att2 = (1 - switch2)*0.25 + switch2*(att3_updated + att4_updated)/2
      att3 = (1 - switch2)*0.25 + switch2*(att1_updated + att2_updated)/2
      att4 = (1 - switch2)*0.25 + switch2*(att1_updated + att2_updated)/2}
  }
  
  return(list(att1 = att1, att2 = att2, att3 = att3, att4 = att4))
}

# Get probability for making the correct choice
get_choice_prop = function(s, t, att1, att2, att3, att4, stimulus, d, lambda) {
  
  # Make att3 and att4 0 if it's stage 1 or 2 (since the stimuli don't exist)
  if (s %in% c(1,2)) {
    att3 = 0
    att4 = 0
  }
  
  # Get decision consistency parameter
  d_param = d*exp(-lambda*(t - 1))
  
  # If stimulus 1, the stimulus value of the correct stimulus is a combination of 1 + 3
  if (stimulus == 1) {
    stim_att = att1^d_param + att3^d_param}
  
  # If stimulus 2, the stimulus value of the correct stimulus is a combination of 1 + 4
  else if (stimulus == 2) {
    stim_att = att1^d_param + att4^d_param}
  
  # Using softmax to get the probability of the correct choice
  choice_p = stim_att / (att1^d_param + att2^d_param + att3^d_param + att4^d_param)
  
  return(choice_p)
  
}

# Function to get the feedback/update signal for att1-att4 (feature weights)
get_weights = function(s, att1, att2, att3, att4, stimulus, f) {
  
  # If in stage 1,2 only 1 dimension and f is not relevant
  if (s %in% c(1,2)) {
    f = 0
    att3 = 0
    att4 = 0
  }
  
  # If stimulus 1: reallocation of att2, att4 (incorrect stim) to correct features
  if (stimulus == 1) { 
    w1 = (1 - f)*att2 + f*att4 
    w2 = -att2
    w3 = (1 - f)*att4 + f*att2
    w4 = -att4}
  
  # If stimulus 2: reallocation of att2, att3 (incorrect stim) to correct features
  else if (stimulus == 2) { 
    w1 = (1 - f)*att2 + f*att3
    w2 = -att2
    w3 = -att3
    w4 = (1 - f)*att3 + f*att2
  }
  
  return(list(w1 = w1, w2 = w2, w3 = w3, w4 = w4))
}

# Update the attention weights using the feedback and alpha 
update_attention = function(att1, att2, att3, att4, w1, w2, w3, w4, alpha) {
  
  # Update with update signal modified by the learning rate (alpha)
  att1_updated = att1 + alpha*w1  
  att2_updated = att2 + alpha*w2
  att3_updated = att3 + alpha*w3  
  att4_updated = att4 + alpha*w4  
  
  return(list(att1_updated = att1_updated, att2_updated = att2_updated, att3_updated = att3_updated, att4_updated = att4_updated))
}

# Running the entire simulation
run_simulation = function(params, n_agents, sim_seeds) {
  
  # Reset simulation data frame
  simdf = NULL
  
  # Extract parameters
  group = params$group
  n_correct = params$n_correct
  f = params$f
  r = params$r
  p = params$p
  d = params$d
  lambda = params$lambda
  switch1 = params$switch1
  switch2 = params$switch2
  
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
      
      # If 50 trials in the last stage, end experiment
      if (end_experiment == T) {break}  
      
      # Create empty vectors to save data in
      trial = rep(NA, n_trials)
      att1 = rep(NA, n_trials)
      att2 = rep(NA, n_trials)
      att3 = rep(NA, n_trials)
      att4 = rep(NA, n_trials)
      w1 = rep(NA, n_trials)
      w2 = rep(NA, n_trials)
      w3 = rep(NA, n_trials)
      w4 = rep(NA, n_trials)
      stimulus = rep(NA, n_trials)
      choicep = rep(NA, n_trials)
      choice = rep(NA, n_trials)
      
      # Make correct count 0, as it's the beginning of the stage
      correct_count = 0
      
      # Loop through trials
      for (t in 1:n_trials) {
        
        # Save trial in vector
        trial[t] = t
        
        # If it's the first trial: define attentions and correct count, otherwise use the updated
        if (t == 1) {att = initiate_att(stage, att_updated, switch1, switch2)}
        else {att = att_updated}
        
        # Extract attentions
        att1[t] = att$att1
        att2[t] = att$att2
        att3[t] = att$att3
        att4[t] = att$att4
        
        # Generate a stimulus set (stimulus set 1 for stage1 and stage2, otherwise equally balanced between stimulus set 1 and stimulus set 2)
        stimulus[t] = ifelse(stage %in% c(1,2), 1, (rbinom(1, 1,0.5) + 1))
        
        # Generate choice probability and choice
        choicep[t] = get_choice_prop(stage, t, att1[t], att2[t], att3[t], att4[t], stimulus[t], d, lambda)
        # Generate choice based on rate of choicep (0 = incorrect, 1 = correct)
        choice[t] = as.numeric(rbinom(1, 1, choicep[t]))
        
        # If correct add to count of correct choices, otherwise reset to 0
        correct_count = ifelse(choice[t] == 1, correct_count + 1, 0)
        
        # Calculate weights (sigma, feedback)
        weights = get_weights(stage, att1[t], att2[t], att3[t], att4[t], stimulus[t], f)
        w1[t] = weights$w1
        w2[t] = weights$w2
        w3[t] = weights$w3
        w4[t] = weights$w4
        
        # Define alpha based on the choice (reward learning rate (r) if correct, punishment learning rate (p) if incorrect)
        alpha = ifelse(choice[t] == 1, r, p)
        # Update attention and save in last_att for next trial 
        att_updated = update_attention(att1[t], att2[t], att3[t], att4[t], w1[t], w2[t], w3[t], w4[t], alpha)
        
        # If 6 consecutive correct, break stage and move to next one 
        if (correct_count == n_correct) {break}
        
        # If 50 trials, then end the experiment
        if (t == 50) {end_experiment = T}
        else {end_experiment = F}
      }
      
      # Save stage data in tibble
      temp = tibble(
        # Save experiment data
        group = group,
        agent = agent,
        seed_agent = sim_seeds[agent],
        stage = stage,
        trial = trial,
        att1 = att1,
        att2 = att2,
        att3 = att3,
        att4 = att4,
        w1 = w1,
        w2 = w2,
        w3 = w3,
        w4 = w4,
        stimulus = stimulus,
        choicep = choicep,
        choice = choice,
        stage_completed = ifelse(end_experiment == F, 1, 0),
        
        # Also save parameters
        n_correct = n_correct,
        d = d,
        lambda = lambda,
        r = r,
        p = p,
        switch1 = switch1,
        switch2 = switch2,
        f = f
      )
      
      # Remove rows without data (stage ended after 6 consecutive correct)
      temp = temp[rowSums(is.na(temp)) != ncol(temp) - 13,]
      
      # Append stage data to overall data (or create if not existing)
      if (exists("simdf")) {simdf = rbind(simdf, temp)} else {simdf = temp}
    }
  }
  
  # Return simulated data frame
  return(simdf) 
}