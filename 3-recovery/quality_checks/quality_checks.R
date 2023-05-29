pacman::p_load(tidyverse, brms, ggplot2, R.utils, bayesplot)
source("../../utils/quality_check_functions.R")

# Input arguments
args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {stop("Input missing!", call.=FALSE)} 
sim_data = args[1]

# Load param combinations 
param_combinations = readRDS(paste0("../fitting/output/sim_", sim_data, "/param_combination.rds"))
index_sample = sample(seq(1,875,1), 20)

# Make vecotors for max rhat and min n eff
max_rhats = c()
min_neffs = c()
n_highk_percs = c()
n_outside_prior_pred = c()
n_outside_posterior_pred = c()

# List of models
models = c("yearsley", "talwar_fRL", "talwar_cafRL_fixedbeta", "ln_fRL", "ln_cafRL_fixedbeta")

for (model in models) {
  
  # Create directory for output
  dir.create(paste0("output/sim_", sim_data, "/model_", model, "/"), showWarnings = FALSE)
  
  # Loop through samples 
  for (index in index_sample){
    
    # SIMULATE DATA
    
    if (sim_data == "yearsley") {
      
      source("../../1-simulation/yearsley/simulation_functions_yearsley.R")
      
      params = list(
        group = "quality checks",
        n_correct = 6,
        f = param_combinations$f[index],
        r = param_combinations$alpha_pos[index],
        p = param_combinations$alpha_neg[index],
        d = 3,
        lambda = 0.05,
        switch1 = 0.95,
        switch2 = 0.95)
      
      d_index = run_simulation(params, 1, param_combinations$sim_seeds[index][[1]])
      
    } else if (sim_data == "talwar_fRL") {
      
      source("../../1-simulation/talwar/simulation_functions_talwar.R")
      
      params = list(
        model = "fRL",
        group = "quality checks",
        n_correct = 6,
        alpha = param_combinations$alpha[index], 
        beta = param_combinations$beta[index],
        theta_init = NA,
        epsilon = NA)
      
      d_index = run_simulation(params, 1, param_combinations$sim_seeds[index][[1]])
      
    } else if (sim_data == "talwar_cafRL") {
      
      source("../../1-simulation/talwar/simulation_functions_talwar.R")
      
      params = list(
        model = "cafRL",
        group = "quality checks",
        n_correct = 6,
        alpha = param_combinations$alpha[index], 
        beta = param_combinations$beta[index],
        theta_init = param_combinations$theta0[index],
        epsilon = NA)
      
      d_index = run_simulation(params, 1, param_combinations$sim_seeds[index][[1]])
      
    } else if (sim_data == "ln_fRL") {
      
      source("../../1-simulation/ln/simulation_functions_ln.R")
      
      params = list(
        model = "fRL",
        group = "quality checks",
        n_correct = 6,
        alpha_pos = param_combinations$alpha_pos[index], 
        alpha_neg = param_combinations$alpha_neg[index],
        beta = param_combinations$beta[index],
        theta_init = NA,
        epsilon = NA)
      
      d_index = run_simulation(params, 1, param_combinations$sim_seeds[index][[1]])
      
    } else if (sim_data == "ln_cafRL") {
      
      source("../../1-simulation/ln/simulation_functions_ln.R")
      
      params = list(
        model = "cafRL",
        group = "quality checks",
        n_correct = 6,
        alpha_pos = param_combinations$alpha_pos[index], 
        alpha_neg = param_combinations$alpha_neg[index],
        beta = param_combinations$beta[index],
        theta_init = param_combinations$theta0[index],
        epsilon = NA)
      
      d_index = run_simulation(params, 1, param_combinations$sim_seeds[index][[1]])
      
    }
      
    # Print in output file
    cat("\n\n------------------------------------------------------------------------------\n")
    cat("Simulation Data: ", sim_data, "Model: ", model, "Index: ", index)
    cat("\n------------------------------------------------------------------------------\n")
    
    # Read the corresponding model  
    fit = readRDS(paste0("../fitting/output/sim_", sim_data, "/model_", model, "/fit_", index, ".rds"))
    
    # Extract draws and relevant columns
    draws_df <- as.data.frame(fit)
    
    # Define relevant pars
    if (model == "yearsley") {
      relevant_pars = (colnames(draws_df))[4:15]
      
    } else if (model == "talwar_fRL") {
      relevant_pars = (colnames(draws_df))[3:8]
      
    } else if (model == "talwar_cafRL_fixedbeta") {
      relevant_pars = (colnames(draws_df))[3:10]
      
    } else if (model == "ln_fRL") {
      relevant_pars = (colnames(draws_df))[4:13]
      
    } else if (model == "ln_cafRL_fixedbeta") {
      relevant_pars = (colnames(draws_df))[4:15]
    }
    
    # DIAGNOSTICS ---
    
    # Print diagnostics and summary to log file
    rstan::check_hmc_diagnostics(fit)
    
    # Get Rhat and number of effective samples 
    summary = as.data.frame(rstan::summary(fit, relevant_pars)$summary)
    max_rhat = max(round(summary$Rhat, digits=2))
    min_neff = min(summary$n_eff)
    max_rhats = c(max_rhats, max_rhat)
    min_neffs = c(min_neffs, min_neff)
    
    # Load loo and save n of high Pareto k values
    loo = readRDS(paste0("../fitting/output/sim_", sim_data, "/model_", model, "/loo_", index, ".rds"))
    
    n_highk_perc = length(loo::pareto_k_ids(loo, threshold=0.7))/length(loo::pareto_k_values(loo))
    n_highk_percs = c(n_highk_percs, n_highk_perc)
    
    # Print diagnostics for model
    cat("\nMax Rhat:", max_rhat, "\nMin number of effective samples:", min_neff, "\nPerc of pareto k values > 0.7:", n_highk_perc)
    
    # PLOTS ---
    
    # Save chains
    rstan::traceplot(fit, relevant_pars, size=0.1)
    ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_chains.png"), width=12, height=7)
    
    # Prior predictive checks
    pp_predictive_hist("prior_pred", fit, d_index)
    ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_prior_pred_hist.png"), width=12, height=7)
    pp_predictive_dens("prior_pred", fit, d_index, 100)
    ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_prior_pred_dens.png"), width=12, height=7)
    
    # Posterior predictive checks
    pp_predictive_hist("posterior_pred", fit, d_index)
    ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_posterior_pred_hist.png"), width=12, height=7)
    pp_predictive_dens("posterior_pred", fit, d_index, 100)
    ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_posterior_pred_dens.png"), width=12, height=7)
    
    if (model == "yearsley") {
      
      # Prior posterior update in log
      plot_prior_posterior(draws_df, "alpha_pos_prior_log", "alpha_pos_posterior_log", "Alpha Pos Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_pos_log.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "alpha_neg_prior_log", "alpha_neg_posterior_log", "Alpha Neg Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_neg_log.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "f_prior_log", "f_posterior_log", "f Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_f_log.png"), width=12, height=7)
      
      # Prior posterior update in p
      plot_prior_posterior(draws_df, "alpha_pos_prior_p", "alpha_pos_posterior_p", "Alpha Pos P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_pos_p.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "alpha_neg_prior_p", "alpha_neg_posterior_p", "Alpha Neg P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_neg_p.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "f_prior_p", "f_posterior_p", "f Prior/Posterior P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_f_p.png"), width=12, height=7)
      
    } else if (model == "talwar_fRL") {
      
      # Prior posterior update in log
      plot_prior_posterior(draws_df, "alpha_prior_log", "alpha_posterior_log", "Alpha Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_log.png"), width=12, height=7)
      
      # Prior posterior update in p
      plot_prior_posterior(draws_df, "alpha_prior_p", "alpha_posterior_p", "Alpha P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_p.png"), width=12, height=7)
      
      # Prior posterior update for beta
      plot_prior_posterior(draws_df, "beta_prior", "beta_posterior", "Beta")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_beta.png"), width=12, height=7)
      
    } else if (model == "talwar_cafRL_fixedbeta") {
      
      # Prior posterior update in log
      plot_prior_posterior(draws_df, "alpha_prior_log", "alpha_posterior_log", "Alpha Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_log.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "theta0_prior_log", "theta0_posterior_log", "Theta0 Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_theta0_log.png"), width=12, height=7)
      
      # Prior posterior update in p
      plot_prior_posterior(draws_df, "alpha_prior_p", "alpha_posterior_p", "Alpha P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_p.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "theta0_prior_p", "theta0_posterior_p", "Theta0 P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_theta0_p.png"), width=12, height=7)
      
    } else if (model == "ln_fRL") {
      
      # Prior posterior update in log
      plot_prior_posterior(draws_df, "alpha_pos_prior_log", "alpha_pos_posterior_log", "Alpha Pos Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_pos_log.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "alpha_neg_prior_log", "alpha_neg_posterior_log", "Alpha Neg Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_neg_log.png"), width=12, height=7)
      
      # Prior posterior update in p
      plot_prior_posterior(draws_df, "alpha_pos_prior_p", "alpha_pos_posterior_p", "Alpha Pos P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_pos_p.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "alpha_neg_prior_p", "alpha_neg_posterior_p", "Alpha Neg P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_neg_p.png"), width=12, height=7)
      
      # Prior posterior update for beta
      plot_prior_posterior(draws_df, "beta_prior", "beta_posterior", "Beta")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_beta.png"), width=12, height=7)
      
      
    } else if (model == "ln_cafRL_fixedbeta") {
      
      # Prior posterior update in log
      plot_prior_posterior(draws_df, "alpha_pos_prior_log", "alpha_pos_posterior_log", "Alpha Pos Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_pos_log.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "alpha_neg_prior_log", "alpha_neg_posterior_log", "Alpha Neg Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_neg_log.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "theta0_prior_log", "theta0_posterior_log", "Theta0 Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_theta0_log.png"), width=12, height=7)
      
      # Prior posterior update in p
      plot_prior_posterior(draws_df, "alpha_pos_prior_p", "alpha_pos_posterior_p", "Alpha Pos P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_pos_p.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "alpha_neg_prior_p", "alpha_neg_posterior_p", "Alpha Neg P")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_alpha_neg_p.png"), width=12, height=7)
      plot_prior_posterior(draws_df, "theta0_prior_log", "theta0_posterior_log", "Theta0 Log")
      ggsave(paste0("output/sim_", sim_data, "/model_", model, "/", index, "_pp_theta0_p.png"), width=12, height=7)
        
    }  
  }
}


# Print the max stats, this will be across all models fitted on the model used for simulation
cat("\n------------------------------------------------------------------------------\n")
cat("Sorted max Rhats:\n", sort(max_rhats))
cat("\nMax of max Rhats: ", max(max_rhats))
cat("\n\n")
cat("Sorted min number of effective samples:\n", sort(min_neffs))
cat("\nMin of min number of effective samples: ", min(min_neffs))
cat("\n\n")
cat("Sorted percent of pareto k values above threshold (0.7):\n", sort(n_highk_percs))
cat("\nMax of percent of pareto k values above threshold (0.7): ", max(n_highk_percs))
cat("\n------------------------------------------------------------------------------\n")