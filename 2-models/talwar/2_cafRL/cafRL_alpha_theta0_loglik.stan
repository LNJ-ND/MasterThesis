// T-cafRL Model with fixed beta ----------------------------------
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
    
    // Return features
    return f;
  }
  
  // Function to get the expected values of stimuli
  vector get_values(int stage, int stimulus, vector f, real theta) {
    
    vector[2] V;
    
    // If stage with only one dimensions, the value equals the feature weight 
    if (stage < 3) {
      
      V[1] = f[1];
      V[2] = f[2];
      
    // If two dimensions, the value is the sum of the feature weights, weighted by theta
    } else if (stage > 2 && stage < 8) {
      
      // Stimulus set 1: f1/f3, f2f4
      if (stimulus == 1) {
        
        V[1] = inv_logit(theta)*f[1] + (1-inv_logit(theta))*f[3];
        V[2] = inv_logit(theta)*f[2] + (1-inv_logit(theta))*f[4];
        
      // Stimulus set 2: f1/f4, f2/f3
      } else if (stimulus == 2) {
        
        V[1] = inv_logit(theta)*f[1] + (1-inv_logit(theta))*f[4];
        V[2] = inv_logit(theta)*f[2] + (1-inv_logit(theta))*f[3];
        
      }
    
    // In stage 8 and stage 9 theta needs to be moved, as the dimensions are reversed
    } else if (stage > 7) {
      
      // Stimulus set 1: f1/f3, f2f4
      if (stimulus == 1) {
        
        V[1] = (1-inv_logit(theta))*f[1] + inv_logit(theta)*f[3];
        V[2] = (1-inv_logit(theta))*f[2] + inv_logit(theta)*f[4];
        
      // Stimulus set 2: f1/f4, f2/f3
      } else if (stimulus == 2) {
        
        V[1] = (1-inv_logit(theta))*f[1] + inv_logit(theta)*f[4];
        V[2] = (1-inv_logit(theta))*f[2] + inv_logit(theta)*f[3];
        
      }
      
    }
    
    // Return expected values
    return V;
  }
  
  // Function to update feature weights
  vector update_weights(int stage, int stimulus, vector f, real alpha, real beta, real theta) {
    
    vector[4] G; // gradient
    real inv_logit_theta; // theta, dimension weight
    vector[4] f_updated; // updated feature
    
    // Put theta on 0-1 scale 
    inv_logit_theta = inv_logit(theta);
    
    // Calculate gradients: this is based on partial differentian of the loss with respect to the given feature weight (see simulations)
    // In stages with one dimension, only necessary for f1 and f2
    if (stage < 3) {
      
      G[1] = -(2 * ((exp(beta * f[1]) * beta/(exp(beta * f[1]) + exp(beta * f[2])) - exp(beta * f[1]) * (exp(beta * f[1]) * beta)/(exp(beta * f[1]) + exp(beta * f[2]))^2) * (1 - (exp(beta * f[1])/(exp(beta * f[1]) + exp(beta * f[2]))))));
      G[2] = 2 * (exp(beta * f[1]) * (exp(beta * f[2]) * beta)/(exp(beta * f[1]) + exp(beta * f[2]))^2 * (1 - (exp(beta * f[1])/(exp(beta * f[1]) + exp(beta * f[2])))));
      
    // In stages with two dimensions, calculate gradient for all features
    } else if (stage > 2 && stage < 8) { 
      
      // Stimulus set 1
      if (stimulus == 1) {
        
        G[1] = -(2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])) * (beta * inv_logit_theta)/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4]))))) - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3]))) * (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4]))))))));
        G[2] = 2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3]))) * (exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])))))));
        G[3] = -(2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])) * (beta * (1 - inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4]))))) - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3]))) * (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4]))))))));
        G[4] = 2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3]))) * (exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])))))));
        
      // Stimulus set 2
      } else if (stimulus == 2) {
        
        G[1] = -(2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])) * (beta * inv_logit_theta)/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3]))))) - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4]))) * (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3]))))))));
        G[2] = 2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4]))) * (exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])))))));
        G[3] = 2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4]))) * (exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])))))));
        G[4] = -(2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])) * (beta * (1 - inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3]))))) - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4]))) * (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3]))))))));
        
      }
    
    // In stage 8 and 9: the relevant dimension is reversed, so theta needs to be switched
    } else if (stage > 7) {
      
      // Stimulus set 1
      if (stimulus == 1) {
        
        G[1] = -(2 * ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])) * (beta * (1 - inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2]))))) - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2]))))))));
        G[2] = 2 * ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])))))));
        G[3] = -(2 * ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])) * (beta * inv_logit_theta)/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2]))))) - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2]))))))));
        G[4] = 2 * ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])))))));
      
      // Stimulus set 2
      } else if (stimulus == 2) {
        
        G[1] = -(2 * ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])) * (beta * (1 - inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2]))))) - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2]))))))));
        G[2] = 2 * ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])) * (beta * (1 - inv_logit_theta)))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])))))));
        G[3] = 2 * ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])))))^2 * (1 - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])))))));
        G[4] = -(2 * ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])) * (beta * inv_logit_theta)/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2]))))) - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])) * (beta * inv_logit_theta))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2]))))))));
        
      }
    }
    
    // Update features
    // Update f1 and f2 in all stages, and f3 and f4 only in stages 3-9
    f_updated[1] = f[1] - inv_logit(alpha) * G[1]; 
    f_updated[2] = f[2] - inv_logit(alpha) * G[2];
    f_updated[3] = (stage == 1 || stage == 2) ? 0 : f[3] - inv_logit(alpha) * G[3];
    f_updated[4] = (stage == 1 || stage == 2) ? 0 : f[4] - inv_logit(alpha) * G[4];
    
    return f_updated;
    
  }
    
  // Function to update theta, dimension weight 
  real update_theta(int stage, int stimulus, vector f, real alpha, real beta, real theta) {
    
    real G; // gradient
    real inv_logit_theta; // theta, dimension weight
    real theta_updated; // updated theta
    
    // Sigmoid, onto 0-1 scale
    inv_logit_theta = inv_logit(theta);
    
    // Calculate gradient in stages 3-7
    // This is based on the partially differentiated loss with respect to theta (see simulations for more details)
    if (stage < 8) {
      
      // From online calculator, works if theta is -1000, but not if theta is 1000
      if (theta <= 0) {
        
        // Stimulus set 1
        if (stimulus == 1) {
          G = -(2*beta*(f[4]-f[3]-f[2]+f[1])*exp((beta*((2*f[2]+f[1])*exp(theta)+2*f[4]+f[3])+theta*(exp(theta)+1))/(exp(theta)+1)))/((exp(theta)+1)^2*(exp((beta*(f[2]*exp(theta)+f[4]))/(exp(theta)+1))+exp((beta*(f[1]*exp(theta)+f[3]))/(exp(theta)+1)))^3);
          
        // Stimulus set 2
        } else if (stimulus == 2) {
          G = (2*beta*(f[4]-f[3]+f[2]-f[1])*exp((beta*((2*f[2]+f[1])*exp(theta)+f[4]+2*f[3])+theta*(exp(theta)+1))/(exp(theta)+1)))/((exp(theta)+1)^2*(exp((beta*(f[2]*exp(theta)+f[3]))/(exp(theta)+1))+exp((beta*(f[1]*exp(theta)+f[4]))/(exp(theta)+1)))^3);
          
        }
        
      // Directly from R/d(): works if theta is 1000, but not if theta is -1000
      } else if (theta > 0) {
        
        // Stimulus set 1
        if (stimulus == 1) {
          G = -(2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[1] - exp(-theta)/(1 + exp(-theta))^2 * f[3]))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4]))))) - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3]))) * (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[1] - exp(-theta)/(1 + exp(-theta))^2 * f[3])) + exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[2] - exp(-theta)/(1 + exp(-theta))^2 * f[4])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[3])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[4]))))))));
          
        // Stimulus set 2
        } else if (stimulus == 2) {
          G = -(2 * ((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[1] - exp(-theta)/(1 + exp(-theta))^2 * f[4]))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3]))))) - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4]))) * (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[1] - exp(-theta)/(1 + exp(-theta))^2 * f[4])) + exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[2] - exp(-theta)/(1 + exp(-theta))^2 * f[3])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))/(((exp(beta * (inv_logit_theta * f[1] + (1 - inv_logit_theta) * f[4])))) + ((exp(beta * (inv_logit_theta * f[2] + (1 - inv_logit_theta) * f[3]))))))));
          
        }
      }
      
    // In stage 8 and 9, dimension is reversed, so theta needs to be shifted
    } else if (stage > 7) {
      
      // From online calculator, works if theta is -1000, but not if theta is 1000
      if (theta <= 0) {
        
        // Stimulus set 1
        if (stimulus == 1) {
          G = (2*beta*(f[4]-f[3]-f[2]+f[1])*exp((beta*((2*f[4]+f[3])*exp(theta)+2*f[2]+f[1])+theta*(exp(theta)+1))/(exp(theta)+1)))/((exp(theta)+1)^2*(exp((beta*(f[4]*exp(theta)+f[2]))/(exp(theta)+1))+exp((beta*(f[3]*exp(theta)+f[1]))/(exp(theta)+1)))^3);
          
        // Stimulus set 2
        } else if (stimulus == 2) {
          G = -(2*beta*(f[4]-f[3]+f[2]-f[1])*exp((beta*((f[4]+2*f[3])*exp(theta)+2*f[2]+f[1])+theta*(exp(theta)+1))/(exp(theta)+1)))/((exp(theta)+1)^2*(exp((beta*(f[4]*exp(theta)+f[1]))/(exp(theta)+1))+exp((beta*(f[3]*exp(theta)+f[2]))/(exp(theta)+1)))^3);
          
        }
        
      // Directly from R/d(): works if theta is 1000, but not if theta is -1000
      } else if (theta > 0) {
        
        // Stimulus set 1
        if (stimulus == 1) {
          G = -(2 * ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[3] - exp(-theta)/(1 + exp(-theta))^2 * f[1]))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2]))))) - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[3] - exp(-theta)/(1 + exp(-theta))^2 * f[1])) + exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[4] - exp(-theta)/(1 + exp(-theta))^2 * f[2])))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[2]))))))));
          
        // Stimulus set 2
        } else if (stimulus == 2) {
          G = -(2 * ((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[4] - exp(-theta)/(1 + exp(-theta))^2 * f[1]))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2]))))) - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1]))) * (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[4] - exp(-theta)/(1 + exp(-theta))^2 * f[1])) + exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])) * (beta * (exp(-theta)/(1 + exp(-theta))^2 * f[3] - exp(-theta)/(1 + exp(-theta))^2 * f[2])))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2])))))^2) * (1 - (exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))/(((exp(beta * (inv_logit_theta * f[4] + (1 - inv_logit_theta) * f[1])))) + ((exp(beta * (inv_logit_theta * f[3] + (1 - inv_logit_theta) * f[2]))))))));
          
        }
      }
    }
    
    // Update theta
    theta_updated = theta - inv_logit(alpha) * G;
    
    // Return updated theta
    return theta_updated;
    
  }
}


// INPUT DATA -------------------------------------------------------------------------------------------------------
data {
  
  // Input from task
  int<lower=1> n_trials; // Total number of trials (in stage)
  array[n_trials] int<lower=1, upper=9> stage; // Stage that the participant is in
  array[n_trials] int<lower=1, upper=50> trial; // Trial within the stage (resets at beginning of stage)
  array[n_trials] int<lower=0, upper=1> choice; // Agents choice of 1 (chose S2) or 2 (chose S1)
  array[n_trials] int<lower=1, upper=2> stimulus; // Stimulus 1 or 2 (here S1/S2)
  
    // Input for priors for parameters that are estimated
  real alpha_mean; // Learning rate (log odds)
  real alpha_sd;
  real theta0_mean; // Initial dimension weight (log odds)
  real<lower=0> theta0_sd;
  
  // Input for fixed parameters 
  real beta;  // Inverse temperature, choice determinism (0-inf)
  
}


// PARAMETERS TO ESTIMATE ---------------------------------------------------------------------------------------------
parameters {
  
  real alpha; // Learning rate 
  real theta0; // Initial dimension weight in stage 3 (first stage with 2 dimensions)

}


// MODEL TO ESTIMATE PARAMETERS ----------------------------------------------------------------------------------------
model {
  
  // Define variables
  vector[4] f; // Vector of feature weights
  vector[4] f_updated; // Vector of updated feature weights after trial
  
  real theta; // Dimension weight 
  real theta_updated; // Updated dimension weight after trial
  
  vector[2] V; // Stimulus values (knowing that V[1] is the correct stimulus)
  real correctp; // Probability of making the correct choice

  // Priors
  target += normal_lpdf(alpha | alpha_mean, alpha_sd); // Learning rate
  target += normal_lpdf(theta0 | theta0_mean, theta0_sd); // Initial dimension weight
  
  // Loop through the trials
  for (t in 1:n_trials) {
    
    // Get feature weights
    // If first trial in first stage, initialise all values to 0, otherwise get the updated ones
    f_updated = (t == 1) ? [0,0,0,0]' : f_updated;
    // If first trial in any stage, initiate weights, otherwise keep the updated ones
    f = (trial[t] == 1) ? initiate_weights(stage[t], f_updated) : f_updated; 
    
    // Get theta (not relevant in stage 1 and 2 (only 1 dimension)
    theta = (stage[t] == 3 && trial[t] == 1) ? theta0 : theta_updated;
    
    // Get stimulus values 
    V = get_values(stage[t], stimulus[t], f, theta);
      
    // Get probability for correct choice
    correctp = softmax(beta*V)[1];

    // Get choice based on probability (1=correct, 0=incorrect)
    target += bernoulli_lpmf(choice[t] | correctp);
      
    // Update features weights
    f_updated = update_weights(stage[t], stimulus[t], f, alpha, beta, theta);
    
    // Update theta if relevant
    if (stage[t] > 2) {
      theta_updated = update_theta(stage[t], stimulus[t], f, alpha, beta, theta);
    }
    
    // Sanity check feature weigths
    if (is_nan(correctp) == 1) {
      print("stage ", stage[t], " trial ", trial[t], " f ", f, " V ", V, " correctp ", correctp, " beta ", beta);
    }
    
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
  real theta0_prior_log;
  
  real<lower=0, upper=1> alpha_prior_p;
  real<lower=0, upper=1> theta0_prior_p;

  // Define posteriors
  real alpha_posterior_log;
  real theta0_posterior_log;
  
  real<lower=0, upper=1> alpha_posterior_p;
  real<lower=0, upper=1> theta0_posterior_p;
  
  // Prior and posterior predictions
  vector<lower=0, upper=1>[n_trials] prior_pred;
  vector<lower=0, upper=1>[n_trials] posterior_pred;
  
  // Define log likelihood for model comparison 
  array[n_trials] real log_lik;
  
  // Define variables for model
  vector[4] f; // Vector of feature weights
  vector[4] f_updated; // Vector of updated feature weights after trial
  real theta; // Dimension weight 
  real theta_updated; // Updated dimension weight after trial
  vector[2] V; // Stimulus values (knowing that V[1] is the correct stimulus)
  real correctp; // Probability of making the correct choice
  
  // Save priors
  alpha_prior_log = normal_rng(alpha_mean, alpha_sd);
  theta0_prior_log = normal_rng(theta0_mean, theta0_sd);
  
  alpha_prior_p = inv_logit(normal_rng(alpha_mean, alpha_sd));
  theta0_prior_p = inv_logit(normal_rng(theta0_mean, theta0_sd));
  
  // Save posteriors 
  alpha_posterior_log = alpha;
  theta0_posterior_log = theta0;
  
  alpha_posterior_p = inv_logit(alpha);
  theta0_posterior_p = inv_logit(theta0);
  
  // Get prior predictions (see code comments in model section above)
  for (t in 1:n_trials) {
    
    f_updated = (t == 1) ? [0,0,0,0]' : f_updated;
    f = (trial[t] == 1) ? initiate_weights(stage[t], f_updated) : f_updated; 
    theta = (stage[t] == 3 && trial[t] == 1) ? theta0_prior_log : theta_updated;
    V = get_values(stage[t], stimulus[t], f, theta);
    correctp = softmax(beta*V)[1];
    prior_pred[t] = bernoulli_rng(correctp);
    f_updated = update_weights(stage[t], stimulus[t], f, alpha_prior_log, beta, theta);
    if (stage[t] > 2) {
      theta_updated = update_theta(stage[t], stimulus[t], f, alpha_prior_log, beta, theta);
    }
  }
  
  // Get posterior predictions and loglik (see code comments in model section above)
  for (t in 1:n_trials) {
    
    f_updated = (t == 1) ? [0,0,0,0]' : f_updated;
    f = (trial[t] == 1) ? initiate_weights(stage[t], f_updated) : f_updated; 
    theta = (stage[t] == 3 && trial[t] == 1) ? theta0 : theta_updated;
    V = get_values(stage[t], stimulus[t], f, theta);
    correctp = softmax(beta*V)[1];
    posterior_pred[t] = bernoulli_rng(correctp);
    log_lik[t] = bernoulli_lpmf(choice[t] | correctp);
    f_updated = update_weights(stage[t], stimulus[t], f, alpha, beta, theta);
    if (stage[t] > 2) {
      theta_updated = update_theta(stage[t], stimulus[t], f, alpha, beta, theta);
    }
  }
}



