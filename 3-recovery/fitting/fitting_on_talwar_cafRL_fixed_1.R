# FITTING ON T-cafRL DATA WITH BETA FIXED TO 1 #

pacman::p_load(tidyverse, brms, cmdstanr, ggplot2, rstan, R.utils)
rstan::rstan_options("auto_write"= TRUE)

# Load functions
source("../../1-simulation/talwar/simulation_functions_talwar.R")

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

# Make output folder for the 
out_path_sim = paste0("output/sim_talwar_cafRL_fixed_1")
#dir.create(file.path(out_path_sim), showWarnings = FALSE)

# Make folder for the model used for fitting
out_path_model = paste0(out_path_sim, "/model_", model)
dir.create(file.path(out_path_model), showWarnings = FALSE)

fit_and_loo = function(stan_model, data, name, out_path_model, i, seed) {
  
  cat("\n\n Model: ", name, "\n")
  
  # Ft and save
  print("--- Fitting Model ---")
  fit <- rstan::sampling(stan_model, data = data,
                         seed = seed, refresh = 0, 
                         iter = 3000, warmup = 1000, 
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

file_talwar_cafRL_fixedbeta = file.path("../../2-models/talwar/2_cafRL/cafRL_alpha_theta0_loglik.stan")
stan_talwar_cafRL_fixedbeta = rstan::stan_model(file_talwar_cafRL_fixedbeta, auto_write = T)

# Load param combination
param_combination = readRDS(paste0(out_path_sim, "/param_combination.rds"))
  
for (i in 1:nrow(param_combination)) {
  
  cat("\n\n---------------------------------------------------------------------------------\n")
  cat(paste("SEED:", seed, "INDEX:", i, "out of", nrow(param_combination)))
  cat("\n\n")
  print(param_combination[i,])
  cat("\n---------------------------------------------------------------------------------\n\n")
  
  # ------------------ SIMULATE DATA ------------------
  
  # Define parameters
  params = list(
    model = "cafRL",
    group = "recovery",
    n_correct = 6,
    alpha = param_combination$alpha[i],
    beta = param_combination$beta[i],
    theta_init = param_combination$theta0[i],
    epsilon = NA)
    
  # Run simulation
  d = run_simulation(params, param_combination$n_agent[i], param_combination$sim_seeds[[i]])
  
  # ------------------ FIT MODELS ------------------
  
  if (model == "talwar_cafRL_fixedbeta") {
    
    # Get data
    data <- list(n_trials = nrow(d),
                 stage = d$stage,
                 trial = d$trial,
                 choice = d$choice,
                 stimulus = d$stimulus,
                 alpha_mean = 0,
                 alpha_sd = 1.5, 
                 theta0_mean = 0,
                 theta0_sd = 1.5,
                 beta = 1)
    
    # Fit and loo
    fit_and_loo(stan_talwar_cafRL_fixedbeta, data, "talwar_cafRL_fixedbeta", out_path_model, i, seed)
    
  } 
}
