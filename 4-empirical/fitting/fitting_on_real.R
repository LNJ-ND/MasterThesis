pacman::p_load(tidyverse, brms, cmdstanr, ggplot2, rstan, R.utils)
rstan::rstan_options("auto_write"= TRUE)

# Input arguments
args = commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop("Input missing!", call.=FALSE)
} 

# Extract input argument
model = args[1]

# Set a seed
seed = 145
set.seed(seed)

# Make folder for the model used for fitting
out_path_model = paste0("output/", model)
dir.create(file.path(out_path_model), showWarnings = FALSE)

# Function to fit model and get loo
fit_and_loo = function(stan_file, data, name, out_path_model, i, seed) {
  
  # Fit and save
  print("--- Fitting Model ---")
  fit <- rstan::sampling(stan_file, data = data,
                         seed = seed, refresh = 0, 
                         iter = 5000, warmup = 1000, 
                         chains = 4, cores = 4, 
                         control = list(adapt_delta = 0.99, max_treedepth = 20))
  
  # Print diagnostics and save model
  print(rstan::check_hmc_diagnostics(fit))
  saveRDS(fit, (paste0(out_path_model, "/fit_", i, ".rds")))
  
  # Run loo with timeout function
  tryCatch(
    
    # Run function with timeout
    withTimeout({
      
      print("--- Model saved, now running loo (without mm) ---")
      loo1 = rstan::loo(fit, cores = 4)
      saveRDS(loo1, (paste0(out_path_model, "/loo_", i, ".rds")))
      
      # Run loo moment match afterwards for Pareto k values
      # And save if successful
      loo2 = rstan::loo_moment_match(fit, loo1, cores = 4)
      saveRDS(loo2, (paste0(out_path_model, "/loo_mm_", i, ".rds")))
      
    }, timeout = 600),
    
    # If timeout occurs
    TimeoutException = function(ex) {
      message("Reached timeout. Skipped!")},
    
    # If error occurs
    error = function(ex) {
      message("Received error. Skipped!")}
  )
  
  print("--- Done! ---")
  
}

# Compile stan models
print("--- Compiling Stan Models ---")

file_yearsley <- file.path("../../2-models-copyforreal/yearsley/yearsley_loglik.stan")
stan_yearsley = rstan::stan_model(file_yearsley, auto_write = T)

file_talwar_fRL = file.path("../../2-models-copyforreal/talwar/1_fRL/fRL_loglik.stan")
stan_talwar_fRL = rstan::stan_model(file_talwar_fRL, auto_write = T)

file_talwar_cafRL_freebeta = file.path("../../2-models-copyforreal/talwar/2_cafRL/cafRL_loglik.stan")
stan_talwar_cafRL_freebeta = rstan::stan_model(file_talwar_cafRL_freebeta, auto_write = T)

file_talwar_cafRL_fixedbeta = file.path("../../2-models-copyforreal/talwar/2_cafRL/cafRL_alpha_theta0_loglik.stan")
stan_talwar_cafRL_fixedbeta = rstan::stan_model(file_talwar_cafRL_fixedbeta, auto_write = T)

file_ln_fRL = file.path("../../2-models-copyforreal/ln/1_ln_fRL/ln_fRL_loglik.stan")
stan_ln_fRL = rstan::stan_model(file_ln_fRL, auto_write = T)

file_ln_cafRL_freebeta = file.path("../../2-models-copyforreal/ln/2_ln_cafRL/ln_cafRL_loglik.stan")
stan_ln_cafRL_freebeta = rstan::stan_model(file_ln_cafRL_freebeta, auto_write = T)

file_ln_cafRL_fixedbeta = file.path("../../2-models-copyforreal/ln/2_ln_cafRL/ln_cafRL_alpha_pos_alpha_neg_theta0_loglik.stan")
stan_ln_cafRL_fixedbeta = rstan::stan_model(file_ln_cafRL_fixedbeta, auto_write = T)

# Load data and add index column for parallel computing
d_all = read.csv("../../data/d_relevant_for_modelling.csv")

# Loop through participants 
for (id in unique(d_all$id_unique)) { # unique(d_all$id_unique)
  
  cat("\n\n---------------------------------------------------------------------------------\n")
  cat(paste("SEED", seed, "PARTICIPANT", id, "INDEX", which(unique(d_all$id_unique) == id), "/78"))
  cat("\n---------------------------------------------------------------------------------\n\n")
  
  # Filter data for participant
  d = filter(d_all, id_unique == id)
  
  # ------------------ FIT MODELS ------------------
  
  if (model == "yearsley") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_pos_mean = 0,
                 alpha_pos_sd = 1,
                 alpha_neg_mean = 0,
                 alpha_neg_sd = 1,
                 f_mean = 0,
                 f_sd = 1)
    
    # Fit and loo
    fit_and_loo(stan_yearsley, data, "yearsley", out_path_model, id, seed)
    
    
  } else if (model == "talwar_fRL") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_mean = 0,
                 alpha_sd = 1, 
                 beta_alpha = 2,
                 beta_beta = 1/2)
    
    # Fit and loo
    fit_and_loo(stan_talwar_fRL, data, "talwar_fRL", out_path_model, id, seed)
    
    
  } else if (model == "talwar_cafRL_freebeta") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_mean = 0,
                 alpha_sd = 1, 
                 beta_alpha = 2,
                 beta_beta = 1/2,
                 theta0_mean = 0,
                 theta0_sd = 1)
    
    # Fit and loo
    fit_and_loo(stan_talwar_cafRL_freebeta, data, "talwar_cafRL_freebeta", out_path_model, id, seed)
    
    
  } else if (model == "talwar_cafRL_fixedbeta") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_mean = 0,
                 alpha_sd = 1, 
                 theta0_mean = 0,
                 theta0_sd = 1,
                 beta = 2)
    
    # Fit and loo
    fit_and_loo(stan_talwar_cafRL_fixedbeta, data, "talwar_cafRL_fixedbeta", out_path_model, id, seed)
    
    
  } else if (model == "ln_fRL") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_pos_mean = 0,
                 alpha_pos_sd = 1,
                 alpha_neg_mean = 0,
                 alpha_neg_sd = 1,
                 beta_alpha = 2,
                 beta_beta = 1/2)
    
    # Fit and loo
    fit_and_loo(stan_ln_fRL, data, "ln_fRL", out_path_model, id, seed)
    
    
  } else if (model == "ln_cafRL_freebeta") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_pos_mean = 0,
                 alpha_pos_sd = 1,
                 alpha_neg_mean = 0,
                 alpha_neg_sd = 1,
                 beta_alpha = 2,
                 beta_beta = 1/2, 
                 theta0_mean = 0,
                 theta0_sd = 1)
    
    # Fit and loo
    fit_and_loo(stan_ln_cafRL_freebeta, data, "ln_cafRL_freebeta", out_path_model, id, seed)
    
    
  } else if (model == "ln_cafRL_fixedbeta") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_pos_mean = 0,
                 alpha_pos_sd = 1,
                 alpha_neg_mean = 0,
                 alpha_neg_sd = 1,
                 theta0_mean = 0,
                 theta0_sd = 1,
                 beta = 2)
    
    # Fit and loo
    fit_and_loo(stan_ln_cafRL_fixedbeta, data, "ln_cafRL_fixedbeta", out_path_model, id, seed)
    
  }
  

}
